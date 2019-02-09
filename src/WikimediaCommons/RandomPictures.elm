module WikimediaCommons.RandomPictures
    exposing
        ( PictureResource
        , Url
        , fetchResources
        , getMaxSize
        , getMaxUrl
        , getUrl
        )

{-| A library for working with the WikimediaCommons API

@docs PictureResource, getUrl, getMaxUrl, getMaxSize, Url, fetchResources

-}

import Http as Http exposing (Response(..))
import Internal as Internal exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Task exposing (Task)


{-| Main type of the library, a picture resource from WikiMedia Commons.
-}
type alias PictureResource =
    Internal.PictureResource


{-| An URL String
-}
type alias Url =
    Internal.Url


{-| Get an `Url` from a `PictureResource` if the given width is valid.
-}
getUrl : Int -> PictureResource -> Maybe Url
getUrl width ((Internal.PictureResource { maxSize }) as pictureResource) =
    let
        ( maxWidth, _ ) =
            maxSize
    in
    if width <= maxWidth && width > 0 then
        Just (mkUrl pictureResource width)
    else
        Nothing


{-| Get an `Url` from a `PictureResource` pointing to a picture in the largest possible size.
-}
getMaxUrl : PictureResource -> Url
getMaxUrl ((PictureResource { maxSize }) as pictureResource) =
    let
        ( maxWidth, _ ) =
            maxSize
    in
    mkUrl pictureResource maxWidth


{-| Get the maximum size in which the resource is available.
-}
getMaxSize : PictureResource -> ( Int, Int )
getMaxSize (PictureResource { maxSize }) =
    maxSize


{-| Fetches a number of picture resources from the API.
-}



-- fetchResources : Int -> Request (List PictureResource)
-- fetchResources count =
--     let
--         url =
--             "https://commons.wikimedia.org/w/api.php?"
--                 ++ String.join "&"
--                     [ "origin=*"
--                     , "action=query"
--                     , "format=json"
--                     , "prop=imageinfo"
--                     , "iiprop=url|size|sha1"
--                     , "generator=random"
--                     , "iiurlwidth=100"
--                     , "grnnamespace=6"
--                     , "grnlimit=" ++ toString count
--                     ]
--     in
--     Http.get url decodeResources


fetchResources : Int -> Task Http.Error (List PictureResource)
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
                    , "grnlimit=" ++ String.fromInt count
                    ]

        f response =
            case response of
                Http.BadUrl_ url_ ->
                    Err (Http.BadUrl url_)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                GoodStatus_ _ bodyStr ->
                    case Decode.decodeString decodeResources bodyStr of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (Decode.errorToString err))
    in
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = Http.stringResolver f
        , timeout = Nothing
        }
