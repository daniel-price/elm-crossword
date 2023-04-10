module Cell exposing (..)

import Browser
import Html exposing (Html, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { content : Maybe Char }


init : Model
init =
    { content = Nothing }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = List.head (String.toList newContent) }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "height" "90px"
        , style "width" "90px"
        ]
        [ input
            [ placeholder ""
            , value
                (case model.content of
                    Nothing ->
                        ""

                    Just a ->
                        String.fromChar a
                )
            , onInput Change
            , style "width" "15px"
            , style "height" "15px"
            ]
            []
        ]
