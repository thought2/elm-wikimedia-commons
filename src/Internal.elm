module Internal exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Extra as Decode exposing (..)
import Regex exposing (..)
import Util as Util


type PictureResource
    = PictureResource
        { maxSize : ( Int, Int )
        , urlTemplate : ( String, String )
        }


type alias UrlTemplate =
    ( String, String )


type alias Url =
    String


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


mkUrlTemplate : Url -> Maybe UrlTemplate
mkUrlTemplate thumbUrl =
    case find All (regex "(.*[/-])\\d*(px.*)") thumbUrl of
        { submatches } :: [] ->
            case submatches of
                (Just prefix) :: (Just suffix) :: [] ->
                    Just ( prefix, suffix )

                _ ->
                    Nothing

        _ ->
            Nothing


mkPictureResource : String -> Int -> Int -> Maybe PictureResource
mkPictureResource thumbUrl width height =
    mkUrlTemplate thumbUrl
        |> Maybe.map
            (\urlTemplate ->
                PictureResource
                    { maxSize = ( width - 1, height - 1 )
                    , urlTemplate = urlTemplate
                    }
            )
