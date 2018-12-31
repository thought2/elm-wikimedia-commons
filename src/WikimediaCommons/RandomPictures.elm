module WikimediaCommons.RandomPictures
    exposing
        ( PictureResource
        , PictureResourceSpec
        , Url
        , decodeResources
        , fetchResources
        , getMaxUrl
        , getUrl
        , toSpec
        )

{-| A library for working with the WikimediaCommons API


# Simple

@docs PictureResource, getUrl, getMaxUrl, Url, fetchResources


# Advanced

@docs decodeResources, toSpec

-}

import Http as Http exposing (Request)
import Json.Decode as Decode exposing (..)
import Json.Decode.Extra as Decode exposing (..)
import Regex exposing (..)
import Util as Util


type PictureResource
    = PictureResource PictureResourceSpec


{-| Inspect data of a picture resource.
-}
toSpec : PictureResource -> PictureResourceSpec
toSpec (PictureResource spec) =
    spec


type alias PictureResourceSpec =
    { maxSize : ( Int, Int )
    , urlTemplate : ( String, String )
    }


type alias UrlTemplate =
    ( String, String )


type alias Url =
    String


{-| Get an `Url` from a `PictureResource` if the given width is valid.
-}
getUrl : Int -> PictureResource -> Maybe Url
getUrl width (PictureResource { maxSize, urlTemplate }) =
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
    at [ "query", "pages" ] (Util.decodeIndexedObject decodeResource)


decodeResource : Decoder PictureResource
decodeResource =
    at [ "imageinfo", "0" ] <|
        (Decode.map3 mkPictureResource
            (field "thumburl" string)
            (field "width" int)
            (field "height" int)
            |> Decode.andThen (Result.fromMaybe "Wrong data format." >> Decode.fromResult)
        )


mkUrl : UrlTemplate -> Int -> Url
mkUrl ( prefix, suffix ) width =
    prefix ++ toString width ++ suffix


mkPictureResource : String -> Int -> Int -> Maybe PictureResource
mkPictureResource thumburl width height =
    let
        result =
            find All (regex "(.*[/-])\\d*(px.*)") thumburl
    in
    case result of
        { submatches } :: [] ->
            case submatches of
                (Just a) :: (Just b) :: [] ->
                    Just <|
                        PictureResource
                            { maxSize = ( width - 1, height - 1 )
                            , urlTemplate = ( a, b )
                            }

                _ ->
                    Nothing

        _ ->
            Nothing
