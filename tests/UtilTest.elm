module UtilTest exposing (..)

import Expect exposing (Expectation)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode exposing (..)
import Test exposing (..)
import Util as U


decodeIndexedObject : Test
decodeIndexedObject =
    describe "decodeIndexedObject"
        [ test "should work with correct data." <|
            \_ ->
                let
                    val =
                        object
                            [ ( "3", string "three" )
                            , ( "1", string "one" )
                            , ( "2", string "two" )
                            ]
                in
                decodeValue (U.decodeIndexedObject Decode.string) val
                    |> Expect.equal (Ok [ "one", "two", "three" ])
        , test "should fail with wrong data." <|
            \_ ->
                let
                    val =
                        object
                            [ ( "3", string "three" )
                            , ( "word", string "one" )
                            , ( "2", string "two" )
                            ]
                in
                decodeValue (U.decodeIndexedObject Decode.string) val
                    |> (\result ->
                            case result of
                                Err _ ->
                                    Expect.equal True True

                                _ ->
                                    Expect.equal True False
                       )
        ]
