module WikimediaCommons.RandomPictures
    exposing
        ( PictureResource
        , Url
        , decodeResources
        , fetchResources
        , getMaxUrl
        , getUrl
        )

{-| A library for working with the WikimediaCommons API


# Simple

@docs PictureResource, getUrl, getMaxUrl, Url, fetchResources


# Advanced

@docs decodeResources, toSpec

-}

import Http as Http exposing (Request)
import Internal as Internal exposing (..)
import Json.Decode as Decode exposing (Decoder)


{-| Main type of the library, a picture resource from WikiMedia Commons.
-}
type alias PictureResource =
    Internal.PictureResource


type alias Url =
    Internal.Url


{-| Get an `Url` from a `PictureResource` if the given width is valid.
-}
getUrl : Int -> PictureResource -> Maybe Url
getUrl width (Internal.PictureResource { maxSize, urlTemplate }) =
    let
        ( maxWidth, _ ) =
            maxSize

        ( prefix, suffix ) =
            urlTemplate
    in
    if width <= maxWidth && width > 0 then
        Just (mkUrl urlTemplate width)
    else
        Nothing


{-| Get an `Url` from a `PictureResource` pointing to a picture in the largest possible size.
-}
getMaxUrl : PictureResource -> Url
getMaxUrl (PictureResource { urlTemplate, maxSize }) =
    let
        ( maxWidth, _ ) =
            maxSize
    in
    mkUrl urlTemplate maxWidth


{-| Fetches a number of picture resources from the API.
-}
fetchResources : Int -> Request (List PictureResource)
fetchResources count =
    let
        url =
            "https://commons.wikimedia.org/w/api.php?"
                ++ String.join "&"
                    [ "origin=*"
                    , "action=query"
                    , "format=json"
                    , "prop=imageinfo"
                    , "iiprop=url|size|sha1"
                    , "generator=random"
                    , "iiurlwidth=100"
                    , "grnnamespace=6"
                    , "grnlimit=" ++ toString count
                    ]
    in
    Http.get url decodeResources


{-| JSON decoder for a list of picture resources.
-}
decodeResources : Decoder (List PictureResource)
decodeResources =
    Internal.decodeResources
