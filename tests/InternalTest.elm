module InternalTest exposing (..)

import Expect exposing (Expectation)
import Internal as I
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode exposing (..)
import Test exposing (..)
import Util as U


encodePictureResourceSpecList : List I.PictureResource -> Value
encodePictureResourceSpecList xs =
    object
        [ ( "query"
          , object
                [ ( "pages"
                  , object
                        (List.indexedMap (\i entry -> ( toString i, encodePictureResourceSpec entry )) xs)
                  )
                ]
          )
        ]


encodePictureResourceSpec : I.PictureResource -> Value
encodePictureResourceSpec (I.PictureResource { urlTemplate, maxSize }) =
    let
        ( prefix, suffix ) =
            urlTemplate

        ( maxWidth, maxHeight ) =
            maxSize

        thumburl =
            prefix ++ "100" ++ suffix

        width =
            maxWidth + 1

        height =
            maxHeight + 1
    in
    object
        [ ( "imageinfo"
          , list
                [ object
                    [ ( "thumburl"
                      , string thumburl
                      )
                    , ( "width", int width )
                    , ( "height", int height )
                    ]
                ]
          )
        ]


testCases : List I.PictureResource
testCases =
    [ I.PictureResource
        { urlTemplate =
            ( "https://upload.wikimedia.org/"
                ++ "wikipedia/commons/thumb/d/d0/Kaple_svateho_Jana_Krtitele.jpg/"
            , "px-Kaple_svateho_Jana_Krtitele.jpg"
            )
        , maxSize = ( 2304, 1728 )
        }
    , I.PictureResource
        { urlTemplate =
            ( "https://upload.wikimedia.org/"
                ++ "wikipedia/commons/thumb/a/a7/Alti-cand.jpg/"
            , "px-Alti-cand.jpg"
            )
        , maxSize = ( 3072, 2304 )
        }
    ]


decodeResources : Test
decodeResources =
    describe "decodeResources"
        [ test "works well with a data round trip." <|
            \_ ->
                testCases
                    |> encodePictureResourceSpecList
                    |> decodeValue I.decodeResources
                    |> Expect.equal (Ok testCases)
        ]


mkUrl : Test
mkUrl =
    describe "mkUrl"
        [ test "inserts the integer correctly." <|
            \_ ->
                I.mkUrl ( "prefix", "suffix" ) 200
                    |> Expect.equal "prefix200suffix"
        ]


mkUrlTemplate : Test
mkUrlTemplate =
    describe "mkUrlTemplate"
        [ test "works with correct data." <|
            \_ ->
                let
                    prefix =
                        "https://upload.wikimedia.org"
                            ++ "/wikipedia/commons/thumb/7/7b/MichiganStadium2010.JPG/"

                    size =
                        100

                    suffix =
                        "px-MichiganStadium2010.JPG"
                in
                I.mkUrlTemplate (prefix ++ toString size ++ suffix)
                    |> Expect.equal (Just ( prefix, suffix ))
        , test "fails with wrong data." <|
            \_ ->
                I.mkUrlTemplate ""
                    |> Expect.equal Nothing
        ]
