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
    = Filled Char
    | Empty
    | Blank


type alias Grid =
    List (List Cell)


type alias Model =
    { grid : Grid }


init : Model
init =
    { grid = [ [ Empty, Blank, Filled 'c' ], [ Filled 'd', Filled 'e', Filled 'f' ] ] }



-- UPDATE


type Msg
    = Change Int Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change x y newContent ->
            { model | grid = updateGrid model.grid x y newContent }


updateGrid : Grid -> Int -> Int -> String -> Grid
updateGrid grid x y newContent =
    grid
        |> List.Extra.updateIfIndex ((==) x)
            (\row ->
                row
                    |> List.Extra.updateIfIndex ((==) y) (\_ -> getCell newContent)
            )


getCell : String -> Cell
getCell s =
    case List.head (List.reverse (String.toList s)) of
        Nothing ->
            Empty

        Just a ->
            Filled a



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
    div [] (List.indexedMap viewRow grid)


viewRow : Int -> List Cell -> Html Msg
viewRow x row =
    div
        [ style "display" "flex" ]
        (List.indexedMap (viewCell x) row)


viewCell : Int -> Int -> Cell -> Html Msg
viewCell x y cell =
    case cell of
        Empty ->
            input
                [ placeholder ""
                , value ""
                , onInput (Change x y)
                , style "border" "1px solid black"
                , style "height" "20px"
                , style "width" "20px"
                ]
                []

        Filled a ->
            input
                [ placeholder ""
                , value (String.fromChar a)
                , onInput (Change x y)
                , style "border" "1px solid black"
                , style "height" "20px"
                , style "width" "20px"
                ]
                []

        Blank ->
            div
                [ style "border" "1px solid black"
                , style "background-color" "black"
                , style "height" "20px"
                , style "width" "20px"
                ]
                []
