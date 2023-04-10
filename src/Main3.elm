module Main exposing (..)

import Browser
import Html exposing (Html, div, input, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Cell =
    Maybe Char


type alias Grid =
    List (List Cell)


type alias Model =
    { grid : Grid }


init : Model
init =
    { grid = [ [ Just 'a', Just 'b', Just 'c' ], [ Just 'd', Just 'e', Just 'f' ] ] }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change ->
        Change newContent ->
            { model | content = List.head (String.toList newContent) }
            model



-- VIEW


view : Model -> Html Msg
view model =
    viewPuzzle model


viewPuzzle : Model -> Html Msg
viewPuzzle model =
    div []
        [ viewGrid model.grid
        ]


viewGrid : Grid -> Html Msg
viewGrid grid =
    div [ style "width" "300px", style "border" "1px solid black" ] (List.map viewRow grid)


viewRow : List Cell -> Html Msg
viewRow row =
    pre
        [ style "display" "flex"
        , style "justify-content" "space-around"
        ]
        (List.map viewCell row)


viewCell : Cell -> Html Msg
viewCell cell =
    div
        [ style "height" "90px"
        , style "width" "90px"
        ]
        [ input
            [ placeholder ""
            , value
                (case cell of
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

