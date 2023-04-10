module Main exposing (..)

import Browser
import Html exposing (Html, div, input, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List.Extra



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
                    |> List.Extra.updateIfIndex ((==) y) (List.head (String.toList newContent))
            )



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
    div [ style "width" "300px", style "border" "1px solid black" ] (List.indexedMap viewRow grid)


viewRow : Int -> List Cell -> Html Msg
viewRow x row =
    pre
        [ style "display" "flex"
        , style "justify-content" "space-around"
        ]
        (List.indexedMap (viewCell x) row)


viewCell : Int -> Int -> Cell -> Html Msg
viewCell x y cell =
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
            , onInput (Change x y)
            , style "width" "15px"
            , style "height" "15px"
            ]
            []
        ]
