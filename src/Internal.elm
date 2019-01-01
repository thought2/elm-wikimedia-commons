module Internal exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Extra as Decode exposing (..)
import Regex exposing (..)
import Util as Util


type PictureResource
    = PictureResource
        { maxSize : ( Int, Int )
        , folder : String
        , fileName : String
        }


type alias Url =
    String


decodeResources : Decoder (List PictureResource)
decodeResources =
    at [ "query", "pages" ] (Util.decodeIndexedObject decodeResource)


decodeResource : Decoder PictureResource
decodeResource =
    at [ "imageinfo", "0" ] <|
        (Decode.map3 mkPictureResource
            (field "url" string)
            (field "width" int)
            (field "height" int)
            |> Decode.andThen (Result.fromMaybe "Wrong data format." >> Decode.fromResult)
        )


mkPictureResource : String -> Int -> Int -> Maybe PictureResource
mkPictureResource url width height =
    parseUrl url
        |> Maybe.map
            (\{ folder, fileName } ->
                PictureResource
                    { maxSize = ( width - 1, height - 1 )
                    , folder = folder
                    , fileName = fileName
                    }
            )


parseUrl : String -> Maybe { folder : String, fileName : String }
parseUrl url =
    let
        regexStr =
            "^https://upload.wikimedia.org/"
                ++ "wikipedia/commons/([0-9a-z]{1,2}/[0-9a-z]{1,2}/)(.*)$"
    in
    case find All (regex regexStr) url of
        { submatches } :: [] ->
            case submatches of
                (Just folder) :: (Just fileName) :: [] ->
                    Just
                        { folder = folder
                        , fileName = fileName
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


mkUrl : PictureResource -> Int -> Url
mkUrl (PictureResource { maxSize, folder, fileName }) width =
    "https://upload.wikimedia.org/"
        ++ "wikipedia/commons/thumb/"
        ++ folder
        ++ fileName
        ++ "/"
        ++ toString width
        ++ "px-"
        ++ fileName
