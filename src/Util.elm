module Util exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Extra as Decode exposing (..)


decodeIndexedObject : Decoder a -> Decoder (List a)
decodeIndexedObject decoder =
    let
        updateKeyValuePair ( key, value ) =
            String.toInt key
                |> Maybe.map (\index -> ( index, value ))
                |> Result.fromMaybe "not a valid integer"
    in
    keyValuePairs decoder
        |> Decode.andThen
            (List.map (updateKeyValuePair >> Decode.fromResult) >> combine)
        |> Decode.map (List.sortBy Tuple.first >> List.map Tuple.second)
