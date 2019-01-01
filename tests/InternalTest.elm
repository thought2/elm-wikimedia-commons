module InternalTest exposing (..)

import Expect exposing (Expectation)
import Internal as I
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode exposing (..)
import Test exposing (..)
import Util as U


encodePictureResourceList : List I.PictureResource -> Value
encodePictureResourceList xs =
    object
        [ ( "query"
          , object
                [ ( "pages"
                  , object
                        (List.indexedMap (\i entry -> ( toString i, encodePictureResource entry )) xs)
                  )
                ]
          )
        ]


encodePictureResource : I.PictureResource -> Value
encodePictureResource (I.PictureResource { folder, fileName, maxSize }) =
    let
        ( maxWidth, maxHeight ) =
            maxSize

        url =
            "https://upload.wikimedia.org/"
                ++ "wikipedia/commons/"
                ++ folder
                ++ fileName

        width =
            maxWidth + 1

        height =
            maxHeight + 1
    in
    object
        [ ( "imageinfo"
          , list
                [ object
                    [ ( "url"
                      , string url
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
        { folder = "d/d0/"
        , fileName = "Kaple_svateho_Jana_Krtitele.jpg"
        , maxSize = ( 2304, 1728 )
        }
    , I.PictureResource
        { folder = "a/a7/"
        , fileName = "Alti-cand.jpg"
        , maxSize = ( 3072, 2304 )
        }
    ]


decodeResources : Test
decodeResources =
    describe "decodeResources"
        [ test "works well with a data round trip." <|
            \_ ->
                testCases
                    |> encodePictureResourceList
                    |> decodeValue I.decodeResources
                    |> Expect.equal (Ok testCases)
        ]


parseUrl : Test
parseUrl =
    describe "parseUrl"
        [ test "works with correct data." <|
            \_ ->
                let
                    url =
                        "https://upload.wikimedia.org/"
                            ++ "wikipedia/commons/4/4c/In_The_Yard_bySLGerry.png"
                in
                I.parseUrl url
                    |> Expect.equal
                        (Just
                            { folder = "4/4c/"
                            , fileName = "In_The_Yard_bySLGerry.png"
                            }
                        )
        , test "fails with empty string." <|
            \_ ->
                I.parseUrl ""
                    |> Expect.equal Nothing
        ]
