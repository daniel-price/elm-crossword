module Main exposing (Cell(..), CellData, Clue, ClueId, Direction(..), Grid, KeyEventMsg(..), Model, Msg(..), getColumnNumber, getRowNumber, main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events
import Dict exposing (Dict)
import Dict.Extra exposing (groupBy)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (id, placeholder, style, value)
import Html.Events exposing (keyCode, onClick, onFocus, onInput, preventDefaultOn)
import Html.Lazy
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, field, int, map2, map6, string)
import List exposing (indexedMap, partition)
import List.Extra
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = \_ -> init "1ec37f05-0e8c-4b4e-a793-38ff439f00bb", update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias CellData =
    { value : Maybe Char
    , clueId1 : ClueId
    , clueId2 : Maybe ClueId
    }


type Cell
    = Item CellData
    | NumberedItem Int CellData
    | Black


type alias ClueId =
    ( Direction, Int )


type Direction
    = Across
    | Down


type alias Clue =
    ( Int, String )


type alias Grid =
    List Cell


type alias Model =
    { version : Int
    , showDebug : Bool
    , grid : Grid
    , currentIndex : Int
    , numberOfColumns : Int
    , numberOfRows : Int
    , clues : { across : List Clue, down : List Clue }
    , currentClue : ( Direction, Int )
    , currentRow : Int
    , currentColumn : Int
    , currentDirection : Direction
    , shiftHeld : Bool
    , latestString : String
    }


init : CrosswordId -> ( Model, Cmd Msg )
init id =
    ( { latestString = ""
      , version = 5
      , showDebug = True
      , shiftHeld = False
      , currentDirection = Across
      , currentRow = 0
      , currentColumn = 0
      , currentIndex = 1
      , numberOfColumns = 15
      , numberOfRows = 15
      , currentClue = ( Across, 1 )
      , clues =
            { across =
                [ ( 1, "Woman's tucked into ridiculously pricey dessert (6,3)" )
                , ( 6, "Exercise with walks occasionally on top of a mountain (4)" )
                , ( 8, "Lyrics discovered in jotter Bill returned (8)" )
                , ( 9, "Think about body shape (6)" )
                , ( 10, "Liberal leader's cagey about inheritance (6)" )
                , ( 11, "Market put up inside old church (8)" )
                , ( 12, "Harpoons fish, filling ship (6)" )
                , ( 15, "Understand flipping idiots caught cheating (8)" )
                , ( 16, "Hoping painkiller's good (8)" )
                , ( 19, "Realise student dropped out to get less stressed (6)" )
                , ( 21, "A family briefly squeeze in post-winter sports activity (5-3)" )
                , ( 22, "Barrels with untaxed liquor initially hidden inside cellars (6)" )
                , ( 24, "Popular archaeological site has nothing in the shade (6)" )
                , ( 25, "Customs document chap provided is in French (8)" )
                , ( 26, "Jam's without sharpness (4)" )
                , ( 27, "Reportedly praise soldiers protecting former PM and civic dignitary (4,5)" )
                ]
            , down =
                [ ( 1, "Scold child with proof of age he's defaced (5)" )
                , ( 2, "Allocate English vessel to take on board a Royal Marine (7)" )
                , ( 3, "It's unknowns supporting Radiohead — splendid! (5)" )
                , ( 4, "Design for college is set in stone (7)" )
                , ( 5, "Iron both ways, beginning with crease and ribbon round top of vest, working well (9)" )
                , ( 6, "Summon soldier for parade (7)" )
                , ( 7, "Excessive pride in hideous orange car (9)" )
                , ( 13, "Put off by pressure working in new depots (9)" )
                , ( 14, "Small women's group with case of supplies, bread and cake (5,4)" )
                , ( 17, "List articles about Italy and Spain (7)" )
                , ( 18, "Suggestion Emma endlessly upset girl running around (7)" )
                , ( 20, "Question doubtful point before getting drunk (7)" )
                , ( 22, "Bitterness against reduced cashback (5)" )
                , ( 23, "Stun with outrageous rates (5)" )
                ]
            }
      , grid =
            [ Black
            , NumberedItem 1 { value = Nothing, clueId1 = ( Across, 1 ), clueId2 = Just ( Down, 1 ) }
            , Item { value = Nothing, clueId1 = ( Across, 1 ), clueId2 = Nothing }
            , NumberedItem 2 { value = Nothing, clueId1 = ( Across, 1 ), clueId2 = Just ( Down, 2 ) }
            , Item { value = Nothing, clueId1 = ( Across, 1 ), clueId2 = Nothing }
            , NumberedItem 3 { value = Nothing, clueId1 = ( Across, 1 ), clueId2 = Just ( Down, 3 ) }
            , Item { value = Nothing, clueId1 = ( Across, 1 ), clueId2 = Nothing }
            , NumberedItem 4 { value = Nothing, clueId1 = ( Across, 1 ), clueId2 = Just ( Down, 4 ) }
            , Item { value = Nothing, clueId1 = ( Across, 1 ), clueId2 = Nothing }
            , NumberedItem 5 { value = Nothing, clueId1 = ( Across, 1 ), clueId2 = Just ( Down, 5 ) }
            , Black
            , NumberedItem 6 { value = Nothing, clueId1 = ( Across, 6 ), clueId2 = Just ( Down, 6 ) }
            , Item { value = Nothing, clueId1 = ( Across, 6 ), clueId2 = Nothing }
            , NumberedItem 7 { value = Nothing, clueId1 = ( Across, 6 ), clueId2 = Just ( Down, 7 ) }
            , Item { value = Nothing, clueId1 = ( Across, 6 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 1 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 2 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 3 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 4 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 5 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 6 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 7 ), clueId2 = Nothing }
            , Black
            , NumberedItem 8 { value = Nothing, clueId1 = ( Across, 8 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 8 ), clueId2 = Just ( Down, 1 ) }
            , Item { value = Nothing, clueId1 = ( Across, 8 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 8 ), clueId2 = Just ( Down, 2 ) }
            , Item { value = Nothing, clueId1 = ( Across, 8 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 8 ), clueId2 = Just ( Down, 3 ) }
            , Item { value = Nothing, clueId1 = ( Across, 8 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 8 ), clueId2 = Just ( Down, 4 ) }
            , Black
            , NumberedItem 9 { value = Nothing, clueId1 = ( Across, 9 ), clueId2 = Just ( Down, 5 ) }
            , Item { value = Nothing, clueId1 = ( Across, 9 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 9 ), clueId2 = Just ( Down, 6 ) }
            , Item { value = Nothing, clueId1 = ( Across, 9 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 9 ), clueId2 = Just ( Down, 7 ) }
            , Item { value = Nothing, clueId1 = ( Across, 9 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 1 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 2 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 3 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 4 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 5 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 6 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 7 ), clueId2 = Nothing }
            , Black
            , NumberedItem 10 { value = Nothing, clueId1 = ( Across, 10 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 10 ), clueId2 = Just ( Down, 1 ) }
            , Item { value = Nothing, clueId1 = ( Across, 10 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 10 ), clueId2 = Just ( Down, 2 ) }
            , Item { value = Nothing, clueId1 = ( Across, 10 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 10 ), clueId2 = Just ( Down, 3 ) }
            , Black
            , NumberedItem 11 { value = Nothing, clueId1 = ( Across, 11 ), clueId2 = Just ( Down, 4 ) }
            , Item { value = Nothing, clueId1 = ( Across, 11 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 11 ), clueId2 = Just ( Down, 5 ) }
            , Item { value = Nothing, clueId1 = ( Across, 11 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 11 ), clueId2 = Just ( Down, 6 ) }
            , Item { value = Nothing, clueId1 = ( Across, 11 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 11 ), clueId2 = Just ( Down, 7 ) }
            , Item { value = Nothing, clueId1 = ( Across, 11 ), clueId2 = Nothing }
            , Black
            , Black
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 2 ), clueId2 = Nothing }
            , Black
            , Black
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 4 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 5 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 6 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 7 ), clueId2 = Nothing }
            , Black
            , NumberedItem 12 { value = Nothing, clueId1 = ( Across, 12 ), clueId2 = Nothing }
            , NumberedItem 13 { value = Nothing, clueId1 = ( Across, 12 ), clueId2 = Just ( Down, 13 ) }
            , Item { value = Nothing, clueId1 = ( Across, 12 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 12 ), clueId2 = Just ( Down, 2 ) }
            , Item { value = Nothing, clueId1 = ( Across, 12 ), clueId2 = Nothing }
            , NumberedItem 14 { value = Nothing, clueId1 = ( Across, 12 ), clueId2 = Just ( Down, 14 ) }
            , Black
            , NumberedItem 15 { value = Nothing, clueId1 = ( Across, 15 ), clueId2 = Just ( Down, 4 ) }
            , Item { value = Nothing, clueId1 = ( Across, 15 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 15 ), clueId2 = Just ( Down, 5 ) }
            , Item { value = Nothing, clueId1 = ( Across, 15 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 15 ), clueId2 = Just ( Down, 6 ) }
            , Item { value = Nothing, clueId1 = ( Across, 15 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 15 ), clueId2 = Just ( Down, 7 ) }
            , Item { value = Nothing, clueId1 = ( Across, 15 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 13 ), clueId2 = Nothing }
            , Black
            , Black
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 14 ), clueId2 = Nothing }
            , Black
            , Black
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 5 ), clueId2 = Nothing }
            , Black
            , Black
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 7 ), clueId2 = Nothing }
            , Black
            , NumberedItem 16 { value = Nothing, clueId1 = ( Across, 16 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 16 ), clueId2 = Just ( Down, 13 ) }
            , Item { value = Nothing, clueId1 = ( Across, 16 ), clueId2 = Nothing }
            , NumberedItem 17 { value = Nothing, clueId1 = ( Across, 16 ), clueId2 = Just ( Down, 17 ) }
            , Item { value = Nothing, clueId1 = ( Across, 16 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 16 ), clueId2 = Just ( Down, 14 ) }
            , Item { value = Nothing, clueId1 = ( Across, 16 ), clueId2 = Nothing }
            , NumberedItem 18 { value = Nothing, clueId1 = ( Across, 16 ), clueId2 = Just ( Down, 18 ) }
            , Black
            , NumberedItem 19 { value = Nothing, clueId1 = ( Across, 19 ), clueId2 = Just ( Down, 5 ) }
            , Item { value = Nothing, clueId1 = ( Across, 19 ), clueId2 = Nothing }
            , NumberedItem 20 { value = Nothing, clueId1 = ( Across, 19 ), clueId2 = Just ( Down, 20 ) }
            , Item { value = Nothing, clueId1 = ( Across, 19 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 19 ), clueId2 = Just ( Down, 7 ) }
            , Item { value = Nothing, clueId1 = ( Across, 19 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 13 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 17 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 14 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 18 ), clueId2 = Nothing }
            , Black
            , Black
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 20 ), clueId2 = Nothing }
            , Black
            , Black
            , Black
            , NumberedItem 21 { value = Nothing, clueId1 = ( Across, 21 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 21 ), clueId2 = Just ( Down, 13 ) }
            , Item { value = Nothing, clueId1 = ( Across, 21 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 21 ), clueId2 = Just ( Down, 17 ) }
            , Item { value = Nothing, clueId1 = ( Across, 21 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 21 ), clueId2 = Just ( Down, 14 ) }
            , Item { value = Nothing, clueId1 = ( Across, 21 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 21 ), clueId2 = Just ( Down, 18 ) }
            , Black
            , NumberedItem 22 { value = Nothing, clueId1 = ( Across, 22 ), clueId2 = Just ( Down, 22 ) }
            , Item { value = Nothing, clueId1 = ( Across, 22 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 22 ), clueId2 = Just ( Down, 20 ) }
            , Item { value = Nothing, clueId1 = ( Across, 22 ), clueId2 = Nothing }
            , NumberedItem 23 { value = Nothing, clueId1 = ( Across, 22 ), clueId2 = Just ( Down, 23 ) }
            , Item { value = Nothing, clueId1 = ( Across, 22 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 13 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 17 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 14 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 18 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 22 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 20 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 23 ), clueId2 = Nothing }
            , Black
            , NumberedItem 24 { value = Nothing, clueId1 = ( Across, 24 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 24 ), clueId2 = Just ( Down, 13 ) }
            , Item { value = Nothing, clueId1 = ( Across, 24 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 24 ), clueId2 = Just ( Down, 17 ) }
            , Item { value = Nothing, clueId1 = ( Across, 24 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 24 ), clueId2 = Just ( Down, 14 ) }
            , Black
            , NumberedItem 25 { value = Nothing, clueId1 = ( Across, 25 ), clueId2 = Just ( Down, 18 ) }
            , Item { value = Nothing, clueId1 = ( Across, 25 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 25 ), clueId2 = Just ( Down, 22 ) }
            , Item { value = Nothing, clueId1 = ( Across, 25 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 25 ), clueId2 = Just ( Down, 20 ) }
            , Item { value = Nothing, clueId1 = ( Across, 25 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 25 ), clueId2 = Just ( Down, 23 ) }
            , Item { value = Nothing, clueId1 = ( Across, 25 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 13 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 17 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 14 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 18 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 22 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 20 ), clueId2 = Nothing }
            , Black
            , Item { value = Nothing, clueId1 = ( Down, 23 ), clueId2 = Nothing }
            , Black
            , NumberedItem 26 { value = Nothing, clueId1 = ( Across, 26 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 26 ), clueId2 = Just ( Down, 13 ) }
            , Item { value = Nothing, clueId1 = ( Across, 26 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 26 ), clueId2 = Just ( Down, 17 ) }
            , Black
            , NumberedItem 27 { value = Nothing, clueId1 = ( Across, 27 ), clueId2 = Just ( Down, 14 ) }
            , Item { value = Nothing, clueId1 = ( Across, 27 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 27 ), clueId2 = Just ( Down, 18 ) }
            , Item { value = Nothing, clueId1 = ( Across, 27 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 27 ), clueId2 = Just ( Down, 22 ) }
            , Item { value = Nothing, clueId1 = ( Across, 27 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 27 ), clueId2 = Just ( Down, 20 ) }
            , Item { value = Nothing, clueId1 = ( Across, 27 ), clueId2 = Nothing }
            , Item { value = Nothing, clueId1 = ( Across, 27 ), clueId2 = Just ( Down, 23 ) }
            , Black
            ]
      }
    , getCrosswordData id
    )



-- UPDATE


type Msg
    = Change Int (Maybe Char)
    | Focus Int CellData
    | Click Int CellData
    | KeyTouched KeyEventMsg
    | GotRemoteCrossword (Result Error RemoteCrossword)
    | NoOp


focusTextInput : Cmd Msg
focusTextInput =
    Task.attempt (\_ -> NoOp) (Dom.focus "text-input")


elementAtIndex : Int -> List a -> Maybe a
elementAtIndex index list =
    if List.length list >= index then
        List.take index list
            |> List.reverse
            |> List.head

    else
        Nothing


calculateModelAfterClick : Model -> Int -> CellData -> ( Model, Cmd Msg )
calculateModelAfterClick model index cellData =
    let
        newDirection : Direction
        newDirection =
            case cellData.clueId2 of
                Just _ ->
                    if model.currentDirection == Down then
                        Across

                    else
                        Down

                Nothing ->
                    Tuple.first cellData.clueId1
    in
    ( { model
        | currentIndex = index
        , currentClue =
            if Tuple.first cellData.clueId1 == newDirection then
                cellData.clueId1

            else
                case cellData.clueId2 of
                    Just clue ->
                        clue

                    Nothing ->
                        cellData.clueId1
        , currentDirection = newDirection
      }
    , focusTextInput
    )


calculateModelAfterFocus : Model -> Int -> CellData -> ( Model, Cmd Msg )
calculateModelAfterFocus model index cellData =
    let
        newDirection : Direction
        newDirection =
            case cellData.clueId2 of
                Just _ ->
                    model.currentDirection

                Nothing ->
                    Tuple.first cellData.clueId1
    in
    ( { model
        | currentIndex = index
        , currentClue =
            if Tuple.first cellData.clueId1 == newDirection then
                cellData.clueId1

            else
                case cellData.clueId2 of
                    Just clue ->
                        clue

                    Nothing ->
                        cellData.clueId1
        , currentDirection = newDirection
      }
    , focusTextInput
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change index newContent ->
            case newContent of
                Nothing ->
                    backspacePressed model

                Just char ->
                    let
                        nextIndex : Int
                        nextIndex =
                            if model.currentDirection == Across then
                                getRightWhiteIndex model.grid index

                            else
                                getDownWhiteIndex model
                    in
                    ( { model | latestString = String.fromChar char, grid = updateGrid model.grid index newContent, currentIndex = nextIndex }, focusTextInput )

        Focus index cellData ->
            calculateModelAfterFocus
                model
                index
                cellData

        Click index cellData ->
            calculateModelAfterClick model index cellData

        KeyTouched keyEventMsg ->
            case keyEventMsg of
                BackspacePressed ->
                    backspacePressed model

                TabPressed ->
                    moveToNextWhiteCell model model.currentDirection model.shiftHeld

                ShiftPressed ->
                    ( { model | shiftHeld = True }, Cmd.none )

                ShiftReleased ->
                    ( { model | shiftHeld = False }, Cmd.none )

                LeftPressed ->
                    moveToNextWhiteCell model Across True

                RightPressed ->
                    moveToNextWhiteCell model Across False

                UpPressed ->
                    moveToNextWhiteCell model Down True

                KeyPressed ->
                    moveToNextWhiteCell model Down False

                _ ->
                    ( model, Cmd.none )
        GotRemoteCrossword result ->
                    case result of
                        Ok remoteCrossword ->
                            (updateCrosswordFromRemote model remoteCrossword, focusTextInput)
                        Err _ ->
                            ( model, focusTextInput)
        NoOp ->
            ( model, Cmd.none )


backspacePressed : Model -> ( Model, Cmd Msg )
backspacePressed model =
    let
        currentCellChar : Maybe Char
        currentCellChar =
            getCurrentCellChar model

        nextIndex : Int
        nextIndex =
            case currentCellChar of
                Nothing ->
                    getNextWhiteCell model model.currentDirection True

                _ ->
                    model.currentIndex
    in
    ( { model | latestString = "", currentIndex = nextIndex, grid = updateGrid model.grid model.currentIndex Nothing }, focusTextInput )


getNextWhiteCell : Model -> Direction -> Bool -> Int
getNextWhiteCell model direction backwards =
    if direction == Across then
        if backwards then
            getLeftWhiteIndex model.grid model.currentIndex

        else
            getRightWhiteIndex model.grid model.currentIndex

    else if backwards then
        getUpWhiteIndex model

    else
        getDownWhiteIndex model


moveToNextWhiteCell : Model -> Direction -> Bool -> ( Model, Cmd Msg )
moveToNextWhiteCell model direction backwards =
    let
        nextIndex : Int
        nextIndex =
            getNextWhiteCell model direction backwards
    in
    ( { model | currentIndex = nextIndex, currentDirection = direction }, focusTextInput )


getCurrentCellChar : Model -> Maybe Char
getCurrentCellChar model =
    let
        cell : Maybe Cell
        cell =
            elementAtIndex (model.currentIndex + 1) model.grid
    in
    case cell of
        Just (Item cellData) ->
            cellData.value

        Just (NumberedItem _ cellData) ->
            cellData.value

        _ ->
            Nothing


getLeftWhiteIndex : Grid -> Int -> Int
getLeftWhiteIndex grid index =
    let
        previousSquares : List Cell
        previousSquares =
            List.reverse (Tuple.first (List.Extra.splitAt index grid))

        offset : Maybe Int
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
        columnNumber : Int
        columnNumber =
            currentColumnNumber model

        rowNumber : Int
        rowNumber =
            currentRowNumber model

        columnSquares : List Cell
        columnSquares =
            takeEveryNthIndexesFromIndex model.numberOfRows columnNumber model.grid

        columnsDown : List Cell
        columnsDown =
            Tuple.second (List.Extra.splitAt rowNumber columnSquares)

        index : Maybe Int
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
        columnNumber : Int
        columnNumber =
            currentColumnNumber model

        rowNumber : Int
        rowNumber =
            currentRowNumber model

        columnSquares : List Cell
        columnSquares =
            takeEveryNthIndexesFromIndex model.numberOfRows columnNumber model.grid

        columnsUp : List Cell
        columnsUp =
            List.reverse (Tuple.first (List.Extra.splitAt (rowNumber - 1) columnSquares))

        index : Maybe Int
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
        cellsFromIndex : List a
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
        nextSquares : List Cell
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


updateGrid : Grid -> Int -> Maybe Char -> Grid
updateGrid grid index newChar =
    grid
        |> List.Extra.updateIfIndex ((==) index)
            (\item ->
                case item of
                    NumberedItem number cellData ->
                        NumberedItem number { cellData | value = newChar }

                    Item cellData ->
                        Item { cellData | value = newChar }

                    Black ->
                        Black
            )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Crossword"
    , body =
        [ viewPuzzle model
        , if model.showDebug then
            debug model

          else
            div [] []
        ]
    }


clueIdToString : ClueId -> String
clueIdToString clue =
    String.concat
        [ "("
        , String.fromInt (Tuple.second clue)
        , " "
        , directionToString (Tuple.first clue)
        , ")"
        ]


cellDataToString : CellData -> String
cellDataToString cellData =
    String.concat
        [ clueIdToString cellData.clueId1
        , " "
        , case cellData.clueId2 of
            Just clue ->
                clueIdToString clue

            Nothing ->
                ""
        ]


debug : Model -> Html Msg
debug model =
    let
        cellString : String
        cellString =
            case elementAtIndex (model.currentIndex + 1) model.grid of
                Just Black ->
                    "Black"

                Just (Item cellData) ->
                    String.concat [ "Item ", cellDataToString cellData ]

                Just (NumberedItem number cellData) ->
                    String.concat [ "NumberedItem ", String.fromInt number, " ", cellDataToString cellData ]

                Nothing ->
                    "None selected"
    in
    div []
        [ viewDebugInt "Current version" model.version
        , viewDebug "Current Cell" cellString
        , viewDebug "curentClue" (String.concat [ String.fromInt (Tuple.second model.currentClue), directionToString (Tuple.first model.currentClue) ])
        , viewDebugInt "currentIndex" model.currentIndex
        , viewDebugInt "currentRow" model.currentRow
        , viewDebugInt "currentColumn" model.currentColumn
        , viewDebug "currentDirection" (directionToString model.currentDirection)
        ]


viewPuzzle : Model -> Html Msg
viewPuzzle model =
    div
        [ style "display" "flex"
        ]
        [ viewGridWithInput model
        , viewCluesSection model Across model.clues.across
        , viewCluesSection model Down model.clues.down
        ]


viewDebug : String -> String -> Html Msg
viewDebug string value =
    div
        [ style "display" "flex"
        , style "flex-direction" "flex"
        ]
        [ textDiv (String.concat [ string, ": ", value ])
        ]


viewDebugInt : String -> Int -> Html Msg
viewDebugInt string value =
    viewDebug string (String.fromInt value)


viewCluesSection : Model -> Direction -> List Clue -> Html Msg
viewCluesSection model direction clues =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
        [ textDiv (directionToString direction)
        , viewClues model direction clues
        ]


directionToString : Direction -> String
directionToString direction =
    case direction of
        Across ->
            "Across"

        Down ->
            "Down"


viewClues : Model -> Direction -> List Clue -> Html Msg
viewClues model direction clues =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
        (List.map (viewClueAndModelAndDirection model direction) clues)


viewClue : String -> ( Int, String ) -> Html Msg
viewClue backgroundColor clue =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "background-color" backgroundColor
        ]
        [ textDiv (String.fromInt (Tuple.first clue))
        , textDiv (Tuple.second clue)
        ]


viewClueAndModelAndDirection : Model -> Direction -> Clue -> Html Msg
viewClueAndModelAndDirection model direction clue =
    let
        backgroundColor : String
        backgroundColor =
            if Tuple.second model.currentClue == Tuple.first clue && direction == Tuple.first model.currentClue then
                "yellow"

            else
                "white"
    in
    Html.Lazy.lazy2 viewClue backgroundColor clue


textDiv : String -> Html Msg
textDiv string =
    div
        []
        [ text string
        ]


viewGrid : Model -> Html Msg
viewGrid model =
    div
        [ style "border" "1px solid black"
        , style "display" "grid"
        , style "height" "750px"
        , style "width" "750px"
        , style "padding" "0"
        , style "margin" "0"
        , style "grid-template" (getGridTemplate model)
        , style "list-style-type" "none"
        ]
        (List.indexedMap (viewCellAndModel model) model.grid)


viewGridWithInput : Model -> Html Msg
viewGridWithInput model =
    div []
        [ input
            -- hidden input which always stays over the current cell and takes input
            [ id "text-input"
            , style
                "position"
                "absolute"
            , style "z-index" "0"
            , style "background-color" "transparent"
            , style "font-size" "24px" -- prevents zoom on mobile devices
            , style "height" "50px"
            , style "outline-width" "0"
            , style "outline" "none"
            , style "border" "none"
            , style "-webkit-box-shadow" "none"
            , style "-moz-box-shadow" "none"
            , style "box-shadow" "none"
            , style "width" "50px"
            , style "top" (String.concat [ String.fromInt ((currentRowNumber model - 1) * 50), "px" ])
            , style "left" (String.concat [ String.fromInt ((currentColumnNumber model - 1) * 50), "px" ])
            , onInput (onTextInput model)
            , value model.latestString
            ]
            []
        , viewGrid model
        ]


onTextInput : Model -> String -> Msg
onTextInput model string =
    Change model.currentIndex (List.head (List.reverse (String.toList string)))


getGridTemplate : Model -> String
getGridTemplate model =
    let
        rowCount : Float
        rowCount =
            sqrt (toFloat (List.length model.grid))

        singleCellPercentage : Float
        singleCellPercentage =
            100 / rowCount
    in
    String.concat [ "repeat(", String.fromFloat rowCount, ", ", String.fromFloat singleCellPercentage, "%)/repeat(", String.fromFloat rowCount, ", ", String.fromFloat singleCellPercentage, "%)" ]


shouldHighlight : Model -> CellData -> Bool
shouldHighlight model cellData =
    case cellData.clueId2 of
        Just x ->
            (x == model.currentClue && Tuple.first x == model.currentDirection) || cellData.clueId1 == model.currentClue

        Nothing ->
            cellData.clueId1 == model.currentClue


charToString : Maybe Char -> String
charToString char =
    case char of
        Just c ->
            String.fromChar c

        Nothing ->
            ""


succeededIfTabKey : Int -> Decode.Decoder Int
succeededIfTabKey key =
    if key == 9 then
        Decode.succeed key

    else
        Decode.fail "non-tab"


tabPressed : Decode.Decoder ( Msg, Bool )
tabPressed =
    Decode.andThen succeededIfTabKey keyCode
        |> Decode.map (always ( KeyTouched KeyEventUnknown, True ))


viewCell : Cell -> Int -> String -> String -> String -> Bool -> Html Msg
viewCell cell index border zIndex backgroundColor selected =
    case cell of
        Item cellData ->
            div
                [ style "position" "relative"
                ]
                [ input
                    [ id (String.fromInt index)
                    , placeholder ""
                    , value (charToString cellData.value)
                    , onFocus (Focus index cellData)
                    , onClick (Click index cellData)
                    , preventDefaultOn "keydown" tabPressed
                    , style "text-transform" "uppercase"
                    , style "box-sizing" "border-box"
                    , style "border" border
                    , style "z-index" zIndex
                    , style "position" "relative"
                    , style "outline" "none"
                    , style "text-align" "center"
                    , style "font-size" "20px"
                    , style "font-weight" "bold"
                    , style "background" "transparent"
                    , style "height" "50px"
                    , style "width" "50px"
                    , style "background-color" backgroundColor
                    , if selected then
                        style "outline" "3px solid DodgerBlue"

                      else
                        style "outline" "0px"
                    , style "border-width"
                        (if selected then
                            "3px"

                         else
                            "1px"
                        )
                    ]
                    [ text (charToString cellData.value)
                    ]
                ]

        NumberedItem number cellData ->
            div
                [ style "position" "relative"
                ]
                [ div
                    [ style "position" "absolute"
                    , style "z-index" "20"
                    ]
                    [ text (String.fromInt number)
                    ]
                , input
                    [ id (String.fromInt index)
                    , style
                        "position"
                        "relative"
                    , placeholder ""
                    , value (charToString cellData.value)
                    , preventDefaultOn "keydown" tabPressed
                    , onFocus (Focus index cellData)
                    , onClick (Click index cellData)
                    , style "border" border
                    , style "z-index" zIndex
                    , style "text-transform" "uppercase"
                    , style "box-sizing" "border-box"
                    , style "outline" "none"
                    , style "text-align" "center"
                    , style "font-size" "20px"
                    , style "font-weight" "bold"
                    , style "background" "transparent"
                    , style "width" "50px"
                    , style "height" "50px"
                    , style "background-color" backgroundColor
                    , if selected then
                        style "outline" "3px solid DodgerBlue"

                      else
                        style "outline" "0px"
                    , style "border-width"
                        (if selected then
                            "3px"

                         else
                            "1px"
                        )
                    ]
                    [ text (charToString cellData.value) ]
                ]

        Black ->
            div
                [ style "background-color" "black"
                ]
                []


viewCellAndModel : Model -> Int -> Cell -> Html Msg
viewCellAndModel model index cell =
    let
        highlight : Bool
        highlight =
            case cell of
                Black ->
                    False

                Item cellData ->
                    shouldHighlight model cellData

                NumberedItem _ cellData ->
                    shouldHighlight model cellData

        backgroundColor : String
        backgroundColor =
            if highlight then
                "yellow"

            else
                "white"

        selected : Bool
        selected =
            index == model.currentIndex

        zIndex : String
        zIndex =
            if selected then
                "10"

            else
                "1"

        border : String
        border =
            if selected then
                "3px solid DodgerBlue"

            else
                "0px solid black"
    in
    Html.Lazy.lazy6 viewCell cell index border zIndex backgroundColor selected


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown keyPressedDecoder
        , Browser.Events.onKeyUp keyReleasedDecoder
        ]


type KeyEventMsg
    = KeyEventUnknown
    | TabPressed
    | BackspacePressed
    | ShiftPressed
    | ShiftReleased
    | LeftPressed
    | RightPressed
    | UpPressed
    | KeyPressed


keyReleasedDecoder : Decode.Decoder Msg
keyReleasedDecoder =
    Decode.map
        (keyReleasedToKeyEventMsg >> KeyTouched)
        (Decode.field "key" Decode.string)


keyPressedDecoder : Decode.Decoder Msg
keyPressedDecoder =
    Decode.map
        (keyPressedToKeyEventMsg >> KeyTouched)
        (Decode.field "key" Decode.string)


keyReleasedToKeyEventMsg : String -> KeyEventMsg
keyReleasedToKeyEventMsg eventKeyString =
    case eventKeyString of
        "Shift" ->
            ShiftReleased

        _ ->
            KeyEventUnknown


keyPressedToKeyEventMsg : String -> KeyEventMsg
keyPressedToKeyEventMsg eventKeyString =
    case eventKeyString of
        "ArrowLeft" ->
            LeftPressed

        "Shift" ->
            ShiftPressed

        "Tab" ->
            TabPressed

        "Backspace" ->
            BackspacePressed

        "ArrowRight" ->
            RightPressed

        "ArrowUp" ->
            UpPressed

        "ArrowDown" ->
            KeyPressed

        _ ->
            KeyEventUnknown


-- HTTP

type alias CrosswordId = String

type alias RemoteCrossword =
    {    id: CrosswordId
        ,entries: List RemoteEntry
    }

type alias RemoteEntry =
   {    id: String
       ,number: Int
       ,clue : String
       ,direction: Direction
       ,length: Int
       ,position: Position
   }

type alias Position =
    {  x: Int
      ,y: Int
    }

getCrosswordData : CrosswordId -> Cmd Msg
getCrosswordData id =
  Http.get
    { url = "http://localhost:8080/crossword/" ++ id
    , expect = Http.expectJson GotRemoteCrossword remoteCrosswordDecoder
    }

remoteCrosswordDecoder : Decoder RemoteCrossword
remoteCrosswordDecoder =
  map2 RemoteCrossword
    (field "id" string)
    (field "entries" (Decode.list remoteEntryDecoder))

remoteEntryDecoder : Decoder RemoteEntry
remoteEntryDecoder =
  map6 RemoteEntry
    (field "id" string)
    (field "number" int)
    (field "clue" string)
    (field "direction" directionDecoder)
    (field "length" int)
    (field "position" positionDecoder)

positionDecoder : Decoder Position
positionDecoder =
  map2 Position
    (field "x" int)
    (field "y" int)

directionDecoder : Decoder Direction
directionDecoder =
  Decode.string
          |> Decode.andThen (\str ->
             case str of
                  "across" ->
                      Decode.succeed Across
                  "down" ->
                      Decode.succeed Down
                  somethingElse ->
                      Decode.fail <| "Unknown direction: " ++ somethingElse
          )

updateCrosswordFromRemote: Model -> RemoteCrossword -> Model
updateCrosswordFromRemote model remoteCrossword =
    let
        (across, down) = partition (\entry -> entry.direction == Across) remoteCrossword.entries
        toClues = List.map (\entry -> (entry.number, entry.clue))
        clueItemsByIndex = groupBy (\(p, _, _) -> p.y * model.numberOfRows + p.x) <| List.concatMap getClueSquares remoteCrossword.entries
        newGrid =
            List.map (getClueItem clueItemsByIndex) <| List.range 0 (model.numberOfRows * model.numberOfColumns - 1)
        initialSquare = List.head <| List.filter (\(_, cell) -> isWhiteSquare cell) <| indexedMap Tuple.pair newGrid

    in
        { model
                 | clues = { across = toClues across, down = toClues down},
                 grid = newGrid,
                 currentIndex = Maybe.withDefault 0 <| Maybe.map Tuple.first initialSquare
               }

getClueItem: Dict Int (List ({ x : Int, y : Int }, ClueId, Maybe Int)) -> Int -> Cell
getClueItem dict index = case Dict.get index dict of
    Just [(_ , clueId, Just n)] -> NumberedItem n (CellData Nothing clueId Nothing)
    Just [(_ , clueId, Nothing)] -> Item (CellData Nothing clueId Nothing)
    Just [(_ , clueId1, Nothing), (_ , clueId2, Nothing)] -> Item (CellData Nothing clueId1  <| Just clueId2)
    Just [(_ , clueId1, Just n), (_ , clueId2, Nothing)] -> NumberedItem n (CellData Nothing clueId1 <| Just clueId2)
    Just [(_ , clueId1, _), (_ , clueId2, Just n)] -> NumberedItem n (CellData Nothing clueId2 <| Just clueId1)
    _ -> Black


getClueSquares: RemoteEntry -> List (Position, ClueId, Maybe Int)
getClueSquares { number, direction, position, length } =
    (position, (direction, number), (Just number)) ::
    (List.map (\p -> (p, (direction, number), Nothing)) <| getPositionArray position length direction)

getPositionArray: Position -> Int -> Direction -> List Position
getPositionArray position length direction =
    case direction of
        Across -> List.map (\x -> ({x = x, y = position.y})) (List.range (position.x + 1) (position.x + length - 1))
        Down -> List.map (\y -> ({x = position.x, y = y})) (List.range (position.y + 1) (position.y + length - 1))


