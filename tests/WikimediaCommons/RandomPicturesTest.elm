module WikimediaCommons.RandomPicturesTest exposing (..)

import Expect exposing (Expectation)
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (..)
import Test exposing (..)
import WikimediaCommons.RandomPictures as RP


encodePictureResourceSpecList : List RP.PictureResourceSpec -> Value
encodePictureResourceSpecList xs =
    object
        [ ( "query"
          , object
                [ ( "pages"
                  , object
                        (List.indexedMap
                            (\i entry ->
                                ( toString i, encodePictureResourceSpec entry )
                            )
                            xs
                        )
                  )
                ]
          )
        ]


encodePictureResourceSpec : RP.PictureResourceSpec -> Value
encodePictureResourceSpec { urlTemplate, maxSize } =
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


testCases : List RP.PictureResourceSpec
testCases =
    [ { urlTemplate =
            ( "https://upload.wikimedia.org/"
                ++ "wikipedia/commons/thumb/d/d0/Kaple_svateho_Jana_Krtitele.jpg/"
            , "px-Kaple_svateho_Jana_Krtitele.jpg"
            )
      , maxSize = ( 2304, 1728 )
      }
    , { urlTemplate =
            ( "https://upload.wikimedia.org/"
                ++ "wikipedia/commons/thumb/a/a7/Alti-cand.jpg/"
            , "px-Alti-cand.jpg"
            )
      , maxSize = ( 3072, 2304 )
      }
    ]


suite : Test
suite =
    describe "WikimediaCommons.RandomPictures"
        [ describe "decodeResources"
            [ test "works well with a data round trip." <|
                \_ ->
                    testCases
                        |> encodePictureResourceSpecList
                        |> decodeValue RP.decodeResources
                        |> Result.map (List.map RP.toSpec)
                        |> Expect.equal (Ok testCases)
            ]
        ]
