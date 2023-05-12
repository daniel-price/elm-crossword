module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Debug exposing (log)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onFocus, onInput)
import Json.Decode as Decode
import List.Extra
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = \_ -> init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Cell
    = Item String
    | NumberedItem Int String
    | Black


type Direction
    = Across
    | Down


type alias Clue =
    ( Int, String )


type alias Grid =
    List Cell


type alias Model =
    { grid : Grid, currentIndex : Int, numberOfColumns : Int, numberOfRows : Int, clues : { across : List Clue, down : List Clue }, currentClue : Int }


init : ( Model, Cmd Msg )
init =
    ( { currentIndex = 0
      , numberOfColumns = 15
      , numberOfRows = 15
      , currentClue = 1
      , clues =
            { across =
                [ ( 1, "Woman's tucked into ridiculously pricey dessert (6,3)" )
                ]
            , down =
                [ ( 1, "Barrel that is holding feline or marine invertebrate (8)" )
                ]
            }
      , grid =
            [ Black
            , NumberedItem 1 ""
            , Item ""
            , NumberedItem 2 ""
            , Item ""
            , NumberedItem 3 ""
            , Item ""
            , NumberedItem 4 ""
            , Item ""
            , NumberedItem 5 ""
            , Black
            , NumberedItem 6 ""
            , Item ""
            , NumberedItem 7 ""
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
            , Item ""
            , Black
            , NumberedItem 8 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Black
            , NumberedItem 9 ""
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
            , Item ""
            , Black
            , NumberedItem 10 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Black
            , NumberedItem 11 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Black
            , Black
            , Black
            , Item ""
            , Black
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
            , NumberedItem 12 ""
            , NumberedItem 13 ""
            , Item ""
            , Item ""
            , Item ""
            , NumberedItem 14 ""
            , Black
            , NumberedItem 15 ""
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
            , Black
            , Black
            , Item ""
            , Black
            , Black
            , Black
            , Item ""
            , Black
            , Black
            , Black
            , Item ""
            , Black
            , NumberedItem 16 ""
            , Item ""
            , Item ""
            , NumberedItem 17 ""
            , Item ""
            , Item ""
            , Item ""
            , NumberedItem 18 ""
            , Black
            , NumberedItem 19 ""
            , Item ""
            , NumberedItem 20 ""
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
            , Black
            , Black
            , Item ""
            , Black
            , Black
            , Black
            , NumberedItem 21 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Black
            , NumberedItem 22 ""
            , Item ""
            , Item ""
            , Item ""
            , NumberedItem 23 ""
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
            , Item ""
            , Black
            , NumberedItem 24 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Black
            , NumberedItem 25 ""
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
            , Item ""
            , Black
            , NumberedItem 26 ""
            , Item ""
            , Item ""
            , Item ""
            , Black
            , NumberedItem 27 ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Item ""
            , Black
            ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change Int String
    | Focus Int
    | FocusResult (Result Dom.Error ())
    | KeyReleasedMsg KeyEventMsg


focusCell : Int -> Cmd Msg
focusCell index =
    Dom.focus (String.fromInt index) |> Task.attempt FocusResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change _ " " ->
            ( model, Cmd.none )

        Change index "" ->
            ( { model | grid = updateGrid model.grid index "" }, Cmd.none )

        Change index newContent ->
            let
                nextIndex =
                    getRightWhiteIndex model.grid index
            in
            ( { model | grid = updateGrid model.grid index newContent, currentIndex = nextIndex }, focusCell nextIndex )

        Focus index ->
            ( { model | currentIndex = index, currentClue = 5 }, Cmd.none )

        FocusResult _ ->
            ( model, Cmd.none )

        KeyReleasedMsg keyEventMsg ->
            case keyEventMsg of
                Left ->
                    let
                        nextIndex =
                            getLeftWhiteIndex model.grid model.currentIndex
                    in
                    ( { model | currentIndex = nextIndex }, focusCell nextIndex )

                Right ->
                    let
                        nextIndex =
                            getRightWhiteIndex model.grid model.currentIndex
                    in
                    ( { model | currentIndex = nextIndex }, focusCell nextIndex )

                Up ->
                    let
                        nextIndex =
                            getUpWhiteIndex model
                    in
                    ( { model | currentIndex = nextIndex }, focusCell nextIndex )

                KeyDown ->
                    let
                        nextIndex =
                            getDownWhiteIndex model
                    in
                    ( { model | currentIndex = nextIndex }, focusCell nextIndex )

                _ ->
                    ( model, Cmd.none )


getLeftWhiteIndex : Grid -> Int -> Int
getLeftWhiteIndex grid index =
    let
        previousSquares =
            List.reverse (Tuple.first (List.Extra.splitAt index grid))

        offset =
            List.Extra.findIndex isWhiteSquare previousSquares
    in
    case offset of
        Just n ->
            index - n - 1

        Nothing ->
            -- reached the first square
            index


getDownWhiteIndex : Model -> Int
getDownWhiteIndex model =
    let
        columnNumber =
            currentColumnNumber model

        rowNumber =
            currentRowNumber model

        columnSquares =
            takeEveryNthIndexesFromIndex model.numberOfRows columnNumber model.grid

        columnsDown =
            Tuple.second (List.Extra.splitAt rowNumber columnSquares)

        index =
            List.Extra.findIndex isWhiteSquare columnsDown
    in
    case index of
        Just n ->
            model.currentIndex + (model.numberOfRows * (n + 1))

        Nothing ->
            -- reached the last square
            model.currentIndex


getUpWhiteIndex : Model -> Int
getUpWhiteIndex model =
    let
        columnNumber =
            currentColumnNumber model

        rowNumber =
            currentRowNumber model

        columnSquares =
            takeEveryNthIndexesFromIndex model.numberOfRows columnNumber model.grid

        columnsUp =
            List.reverse (Tuple.first (List.Extra.splitAt (rowNumber - 1) columnSquares))

        index =
            List.Extra.findIndex isWhiteSquare columnsUp
    in
    case index of
        Just n ->
            model.currentIndex - (model.numberOfRows * (n + 1))

        Nothing ->
            -- reached the last square
            model.currentIndex


currentColumnNumber : Model -> Int
currentColumnNumber model =
    getColumnNumber model.numberOfColumns model.currentIndex


getColumnNumber : Int -> Int -> Int
getColumnNumber numberOfColumns index =
    modBy numberOfColumns index + 1


currentRowNumber : Model -> Int
currentRowNumber model =
    getRowNumber model.numberOfColumns model.currentIndex


getRowNumber : Int -> Int -> Int
getRowNumber numberOfColumns index =
    floor (toFloat index / toFloat numberOfColumns) + 1


takeEveryNthIndexesFromIndex : Int -> Int -> List a -> List a
takeEveryNthIndexesFromIndex n initialIndex l =
    let
        cellsFromIndex =
            Tuple.second (List.Extra.splitAt (initialIndex - 1) l)
    in
    cellsFromIndex
        |> List.indexedMap
            (\i x ->
                if (i |> modBy n) == 0 then
                    Just x

                else
                    Nothing
            )
        |> List.filterMap identity


getRightWhiteIndex : Grid -> Int -> Int
getRightWhiteIndex grid index =
    let
        nextSquares =
            Tuple.second (List.Extra.splitAt (index + 1) grid)
    in
    case List.Extra.findIndex isWhiteSquare nextSquares of
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
        [ style "display" "flex"
        ]
        [ viewGrid model.grid
        , viewCluesSection Across model.clues.across
        , viewCluesSection Down model.clues.down
        , textDiv (String.fromInt model.currentClue)
        ]


viewCluesSection : Direction -> List Clue -> Html Msg
viewCluesSection direction clues =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
        [ textDiv (directionToString direction)
        , viewClues clues
        ]


directionToString : Direction -> String
directionToString direction =
    case direction of
        Across ->
            "Across"

        Down ->
            "Down"


viewClues : List Clue -> Html Msg
viewClues clues =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
        (List.map viewClue clues)


viewClue : Clue -> Html Msg
viewClue clue =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        ]
        [ textDiv (String.fromInt (Tuple.first clue))
        , textDiv (Tuple.second clue)
        ]


textDiv : String -> Html Msg
textDiv string =
    div
        []
        [ text string
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
                , onFocus (Focus index)
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
                    [ text (String.fromInt number)
                    ]
                , input
                    [ id (String.fromInt index)
                    , placeholder ""
                    , value letter
                    , onInput (Change index)
                    , onFocus (Focus index)
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown keyReleasedDecoder
        ]


type KeyEventMsg
    = KeyEventLetter Char
    | KeyEventUnknown String
    | Left
    | Right
    | Up
    | KeyDown


keyReleasedDecoder : Decode.Decoder Msg
keyReleasedDecoder =
    Decode.map (toKeyEventMsg >> KeyReleasedMsg) (Decode.field "key" Decode.string)


toKeyEventMsg : String -> KeyEventMsg
toKeyEventMsg eventKeyString =
    case eventKeyString of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "ArrowUp" ->
            Up

        "ArrowDown" ->
            KeyDown

        string_ ->
            case String.uncons string_ of
                Just ( char, "" ) ->
                    KeyEventLetter char

                _ ->
                    KeyEventUnknown eventKeyString
