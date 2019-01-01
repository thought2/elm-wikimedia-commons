module Viewer exposing (..)

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



-- MODEL


type alias Model =
    List RP.PictureResource


init : ( Model, Cmd Msg )
init =
    ( [], getResources )


getResources =
    Http.send NewResources (RP.fetchResources 50)



-- UPDATE


type Msg
    = NewResources (Result Http.Error (List RP.PictureResource))
    | Refresh


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( model, getResources )

        NewResources (Ok resources) ->
            ( resources, Cmd.none )

        NewResources (Err error) ->
            let
                _ =
                    Debug.log "err" error
            in
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view resources =
    div []
        [ h1 [] []
        , button [ onClick Refresh ] [ text "hello world" ]
        , div [] (List.map (\res -> img [ width 100, src (RP.getUrl 100 res |> Maybe.withDefault "") ] []) resources)
        ]
