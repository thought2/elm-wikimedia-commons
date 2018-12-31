module WikimediaCommons.RandomPicturesTest exposing (..)

import Expect exposing (Expectation)
import Internal as I
import Test exposing (..)
import WikimediaCommons.RandomPictures as RP


mockPictureResource : I.PictureResource
mockPictureResource =
    I.PictureResource
        { urlTemplate = ( "prefix", "suffix" )
        , maxSize = ( 100, 200 )
        }


getUrl : Test
getUrl =
    describe "getUrl"
        [ test "Works with correct data." <|
            \_ ->
                RP.getUrl 100 mockPictureResource
                    |> Expect.equal (Just "prefix100suffix")
        , test "Fails with wrong data." <|
            \_ ->
                RP.getUrl 101 mockPictureResource
                    |> Expect.equal Nothing
        ]


getMaxUrl : Test
getMaxUrl =
    describe "getMaxUrl"
        [ test "Works with correct data." <|
            \_ ->
                RP.getMaxUrl mockPictureResource
                    |> Expect.equal "prefix100suffix"
        ]
