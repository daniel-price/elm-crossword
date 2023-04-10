module Main exposing (..)

import Browser
import Html exposing (Html, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List.Extra



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Cell
    = White String
    | Black


type alias Grid =
    List Cell


type alias Model =
    { grid : Grid }


init : Model
init =
    { grid =
        [ Black
        , White ""
        , White ""
        , Black
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        , White ""
        ]
    }



-- UPDATE


type Msg
    = Change Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change index newContent ->
            { model | grid = updateGrid model.grid index newContent }


updateGrid : Grid -> Int -> String -> Grid
updateGrid grid index newContent =
    grid
        |> List.Extra.updateIfIndex ((==) index) (\_ -> White (String.right 1 newContent))



-- VIEW


view : Model -> Html Msg
view model =
    viewPuzzle model


viewPuzzle : Model -> Html Msg
viewPuzzle model =
    div
        []
        [ viewGrid model.grid
        ]


viewGrid : Grid -> Html Msg
viewGrid grid =
    div
        [ style "border" "1px solid black"
        , style "display" "grid"
        , style "height" "650px"
        , style "width" "650px"
        , style "padding" "0"
        , style "margin" "0"
        , style "grid-template" "repeat(13, 7.6923076923%)/repeat(13, 7.6923076923%)"
        , style "list-style-type" "none"
        ]
        (List.indexedMap viewCell grid)


viewCell : Int -> Cell -> Html Msg
viewCell index cell =
    case cell of
        White a ->
            input
                [ placeholder ""
                , value a
                , onInput (Change index)
                , style "text-transform" "uppercase"
                , style "box-sizing" "border-box"
                , style "border" "1px solid black"
                , style "outline" "none"
                , style "text-align" "center"
                , style "font-size" "20px"
                , style "font-weight" "bold"
                , style "z-index" "100"
                , style "background" "transparent"
                ]
                []

        Black ->
            div
                [ style "background-color" "black"
                , style "z-index" "100"
                ]
                []
