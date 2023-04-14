module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Debug exposing (log)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List.Extra
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = \_ -> init, update = update, view = view, subscriptions = always Sub.none }



-- MODEL


type Cell
    = Item String
    | NumberedItem Int String
    | Black


type alias Grid =
    List Cell


type alias Model =
    { grid : Grid }


init : ( Model, Cmd Msg )
init =
    ( { grid =
            [ Black
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , NumberedItem 9 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , NumberedItem 10 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , NumberedItem 11 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , NumberedItem 12 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , NumberedItem 13 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , NumberedItem 14 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Black
            , Item ""
            , NumberedItem 15 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , NumberedItem 16 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Black
            , Black
            , NumberedItem 17 ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , NumberedItem 18 ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , NumberedItem 19 ""
            , Black
            , NumberedItem 20 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , NumberedItem 21 ""
            , Item ""
            , Item ""
            , NumberedItem 22 ""
            , Item ""
            , Item ""
            , Item ""
            , NumberedItem 23 ""
            , Item ""
            , NumberedItem 24 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , NumberedItem 25 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , NumberedItem 26 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            , Item ""
            , Black
            ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change Int String
    | FocusResult (Result Dom.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change index newContent ->
            ( { model | grid = updateGrid model.grid index newContent }, Dom.focus (String.fromInt (getNextIndex model.grid index)) |> Task.attempt FocusResult )

        FocusResult _ ->
            ( model, Cmd.none )


getNextIndex : Grid -> Int -> Int
getNextIndex grid index =
    case List.Extra.findIndex isWhiteSquare (Tuple.second (List.Extra.splitAt (index + 1) grid)) of
        Just n ->
            index + 1 + n

        Nothing ->
            -- reached the last square
            index


isWhiteSquare : Cell -> Bool
isWhiteSquare cell =
    case cell of
        Item _ ->
            True

        NumberedItem _ _ ->
            True

        Black ->
            False


updateGrid : Grid -> Int -> String -> Grid
updateGrid grid index newContent =
    grid
        |> List.Extra.updateIfIndex ((==) index)
            (\item ->
                case item of
                    NumberedItem number _ ->
                        NumberedItem number (String.right 1 newContent)

                    Item _ ->
                        Item (String.right 1 newContent)

                    Black ->
                        Black
            )



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
        , style "grid-template" (getGridTemplate grid)
        , style "list-style-type" "none"
        ]
        (List.indexedMap viewCell grid)


getGridTemplate : Grid -> String
getGridTemplate grid =
    let
        rowCount =
            sqrt (toFloat (List.length grid))

        singleCellPercentage =
            100 / rowCount
    in
    String.concat [ "repeat(", String.fromFloat rowCount, ", ", String.fromFloat singleCellPercentage, "%)/repeat(", String.fromFloat rowCount, ", ", String.fromFloat singleCellPercentage, "%)" ]


viewCell : Int -> Cell -> Html Msg
viewCell index cell =
    case cell of
        Item a ->
            input
                [ id (String.fromInt index)
                , placeholder ""
                , value a
                , onInput (Change index)
                , style "text-transform" "uppercase"
                , style "box-sizing" "border-box"
                , style "border" "1px solid black"
                , style "outline" "none"
                , style "text-align" "center"
                , style "font-size" "20px"
                , style "font-weight" "bold"
                , style "background" "transparent"
                ]
                []

        NumberedItem number letter ->
            div
                []
                [ div
                    [ style "position" "absolute"
                    ]
                    [ text (String.fromInt number) ]
                , input
                    [ id (String.fromInt index)
                    , placeholder ""
                    , value letter
                    , onInput (Change index)
                    , style "text-transform" "uppercase"
                    , style "box-sizing" "border-box"
                    , style "border" "1px solid black"
                    , style "outline" "none"
                    , style "text-align" "center"
                    , style "font-size" "20px"
                    , style "font-weight" "bold"
                    , style "background" "transparent"
                    , style "width" "100%"
                    , style "height" "100%"
                    ]
                    []
                ]

        Black ->
            div
                [ style "background-color" "black"
                ]
                []
