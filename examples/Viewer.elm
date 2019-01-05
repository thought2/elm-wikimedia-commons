module Viewer
    exposing
        ( Model
        , Msg(..)
        , constants
        , init
        , isValidResource
        , main
        , subscriptions
        , update
        , view
        , viewLarge
        )

import Html as Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http as Http
import WikimediaCommons.RandomPictures as RP


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


constants :
    { thumbWidth : Int
    , thumbMaxHeight : Int
    , largeWidth : Int
    , count : Int
    }
constants =
    { thumbWidth = 100
    , thumbMaxHeight = 150
    , largeWidth = 500
    , count = 40
    }



-- MODEL


type alias Model =
    { resources : List RP.PictureResource
    , selection : Maybe RP.PictureResource
    }


init : ( Model, Cmd Msg )
init =
    ( { resources = [], selection = Nothing }
    , getResources
    )


getResources : Cmd Msg
getResources =
    Http.send NewResources (RP.fetchResources constants.count)



-- UPDATE


type Msg
    = NewResources (Result Http.Error (List RP.PictureResource))
    | Refresh
    | Select RP.PictureResource
    | Close


isValidResource : RP.PictureResource -> Bool
isValidResource resource =
    let
        ( maxWidth, maxHeight ) =
            RP.getMaxSize resource

        aspect =
            toFloat maxWidth / toFloat maxHeight

        maxAspect =
            toFloat constants.thumbWidth / toFloat constants.thumbMaxHeight
    in
    maxWidth >= constants.largeWidth && aspect > maxAspect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( model, getResources )

        NewResources (Ok resources) ->
            ( { model | resources = List.filter isValidResource resources }
            , Cmd.none
            )

        NewResources (Err error) ->
            let
                _ =
                    Debug.log "unhandled error" error
            in
            ( model, Cmd.none )

        Select selection ->
            ( { model | selection = Just selection }
            , Cmd.none
            )

        Close ->
            ( { model | selection = Nothing }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewLarge : RP.PictureResource -> Html Msg
viewLarge resource =
    let
        styleWrapper =
            [ ( "position", "fixed" )
            , ( "top", "0px" )
            , ( "right", "0px" )
            , ( "bottom", "0px" )
            , ( "left", "0px" )
            , ( "background-color", "rgba(0,0,0,0.5)" )
            , ( "display", "flex" )
            , ( "align-items", "center" )
            , ( "justify-content", "center" )
            , ( "cursor", "pointer" )
            ]
    in
    div [ style styleWrapper, onClick Close ]
        [ img
            [ src (RP.getUrl constants.largeWidth resource |> Maybe.withDefault "")
            ]
            []
        ]


view : Model -> Html Msg
view { resources, selection } =
    let
        styleWrapper =
            [ ( "margin", "25px" ) ]

        styleWrapperList =
            [ ( "display", "flex" )
            , ( "flex-wrap", "wrap" )
            ]

        styleItem =
            [ ( "border", "1px solid rgba(0,0,0, 0.3)" )
            , ( "width", toString constants.thumbWidth ++ "px" )
            , ( "height", toString constants.thumbMaxHeight ++ "px" )
            , ( "margin", "7px" )
            , ( "display", "flex" )
            , ( "align-items", "flex-end" )
            , ( "cursor", "pointer" )
            ]

        styleButton =
            [ ( "margin-left", "7px" )
            , ( "margin-bottom", "20px" )
            ]
    in
    div [ style styleWrapper ]
        [ button [ onClick Refresh, style styleButton ] [ text "refresh" ]
        , div
            [ style styleWrapperList
            ]
            (List.map
                (\res ->
                    div
                        [ style styleItem
                        , onClick (Select res)
                        ]
                        [ img
                            [ width 100
                            , src (RP.getUrl 100 res |> Maybe.withDefault "")
                            ]
                            []
                        ]
                )
                resources
            )
        , Maybe.map viewLarge selection |> Maybe.withDefault (text "")
        ]
