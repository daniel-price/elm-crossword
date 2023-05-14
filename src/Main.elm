module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onFocus, onInput)
import Html.Lazy
import Json.Decode as Decode
import List.Extra
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = \_ -> init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias CellData =
    { value : String
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
    { showDebug : Bool
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
    }


init : ( Model, Cmd Msg )
init =
    ( { showDebug = True
      , shiftHeld = False
      , currentDirection = Across
      , currentRow = 0
      , currentColumn = 0
      , currentIndex = 0
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
            , NumberedItem 1 { value = "", clueId1 = ( Across, 1 ), clueId2 = Just ( Down, 1 ) }
            , Item { value = "", clueId1 = ( Across, 1 ), clueId2 = Nothing }
            , NumberedItem 2 { value = "", clueId1 = ( Across, 1 ), clueId2 = Just ( Down, 2 ) }
            , Item { value = "", clueId1 = ( Across, 1 ), clueId2 = Nothing }
            , NumberedItem 3 { value = "", clueId1 = ( Across, 1 ), clueId2 = Just ( Down, 3 ) }
            , Item { value = "", clueId1 = ( Across, 1 ), clueId2 = Nothing }
            , NumberedItem 4 { value = "", clueId1 = ( Across, 1 ), clueId2 = Just ( Down, 4 ) }
            , Item { value = "", clueId1 = ( Across, 1 ), clueId2 = Nothing }
            , NumberedItem 5 { value = "", clueId1 = ( Across, 1 ), clueId2 = Just ( Down, 5 ) }
            , Black
            , NumberedItem 6 { value = "", clueId1 = ( Across, 6 ), clueId2 = Just ( Down, 6 ) }
            , Item { value = "", clueId1 = ( Across, 6 ), clueId2 = Nothing }
            , NumberedItem 7 { value = "", clueId1 = ( Across, 6 ), clueId2 = Just ( Down, 7 ) }
            , Item { value = "", clueId1 = ( Across, 6 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 1 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 2 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 3 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 4 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 5 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 6 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 7 ), clueId2 = Nothing }
            , Black
            , NumberedItem 8 { value = "", clueId1 = ( Across, 8 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 8 ), clueId2 = Just ( Down, 1 ) }
            , Item { value = "", clueId1 = ( Across, 8 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 8 ), clueId2 = Just ( Down, 2 ) }
            , Item { value = "", clueId1 = ( Across, 8 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 8 ), clueId2 = Just ( Down, 3 ) }
            , Item { value = "", clueId1 = ( Across, 8 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 8 ), clueId2 = Just ( Down, 4 ) }
            , Black
            , NumberedItem 9 { value = "", clueId1 = ( Across, 9 ), clueId2 = Just ( Down, 5 ) }
            , Item { value = "", clueId1 = ( Across, 9 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 9 ), clueId2 = Just ( Down, 6 ) }
            , Item { value = "", clueId1 = ( Across, 9 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 9 ), clueId2 = Just ( Down, 7 ) }
            , Item { value = "", clueId1 = ( Across, 9 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 1 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 2 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 3 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 4 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 5 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 6 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 7 ), clueId2 = Nothing }
            , Black
            , NumberedItem 10 { value = "", clueId1 = ( Across, 10 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 10 ), clueId2 = Just ( Down, 1 ) }
            , Item { value = "", clueId1 = ( Across, 10 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 10 ), clueId2 = Just ( Down, 2 ) }
            , Item { value = "", clueId1 = ( Across, 10 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 10 ), clueId2 = Just ( Down, 3 ) }
            , Black
            , NumberedItem 11 { value = "", clueId1 = ( Across, 11 ), clueId2 = Just ( Down, 4 ) }
            , Item { value = "", clueId1 = ( Across, 11 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 11 ), clueId2 = Just ( Down, 5 ) }
            , Item { value = "", clueId1 = ( Across, 11 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 11 ), clueId2 = Just ( Down, 6 ) }
            , Item { value = "", clueId1 = ( Across, 11 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 11 ), clueId2 = Just ( Down, 7 ) }
            , Item { value = "", clueId1 = ( Across, 11 ), clueId2 = Nothing }
            , Black
            , Black
            , Black
            , Item { value = "", clueId1 = ( Down, 2 ), clueId2 = Nothing }
            , Black
            , Black
            , Black
            , Item { value = "", clueId1 = ( Down, 4 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 5 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 6 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 7 ), clueId2 = Nothing }
            , Black
            , NumberedItem 12 { value = "", clueId1 = ( Across, 12 ), clueId2 = Nothing }
            , NumberedItem 13 { value = "", clueId1 = ( Across, 12 ), clueId2 = Just ( Down, 13 ) }
            , Item { value = "", clueId1 = ( Across, 12 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 12 ), clueId2 = Just ( Down, 2 ) }
            , Item { value = "", clueId1 = ( Across, 12 ), clueId2 = Nothing }
            , NumberedItem 14 { value = "", clueId1 = ( Across, 12 ), clueId2 = Just ( Down, 14 ) }
            , Black
            , NumberedItem 15 { value = "", clueId1 = ( Across, 15 ), clueId2 = Just ( Down, 4 ) }
            , Item { value = "", clueId1 = ( Across, 15 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 15 ), clueId2 = Just ( Down, 5 ) }
            , Item { value = "", clueId1 = ( Across, 15 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 15 ), clueId2 = Just ( Down, 6 ) }
            , Item { value = "", clueId1 = ( Across, 15 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 15 ), clueId2 = Just ( Down, 7 ) }
            , Item { value = "", clueId1 = ( Across, 15 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 13 ), clueId2 = Nothing }
            , Black
            , Black
            , Black
            , Item { value = "", clueId1 = ( Down, 14 ), clueId2 = Nothing }
            , Black
            , Black
            , Black
            , Item { value = "", clueId1 = ( Down, 5 ), clueId2 = Nothing }
            , Black
            , Black
            , Black
            , Item { value = "", clueId1 = ( Down, 7 ), clueId2 = Nothing }
            , Black
            , NumberedItem 16 { value = "", clueId1 = ( Across, 16 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 16 ), clueId2 = Just ( Down, 13 ) }
            , Item { value = "", clueId1 = ( Across, 16 ), clueId2 = Nothing }
            , NumberedItem 17 { value = "", clueId1 = ( Across, 16 ), clueId2 = Just ( Down, 17 ) }
            , Item { value = "", clueId1 = ( Across, 16 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 16 ), clueId2 = Just ( Down, 14 ) }
            , Item { value = "", clueId1 = ( Across, 16 ), clueId2 = Nothing }
            , NumberedItem 18 { value = "", clueId1 = ( Across, 16 ), clueId2 = Just ( Down, 18 ) }
            , Black
            , NumberedItem 19 { value = "", clueId1 = ( Across, 19 ), clueId2 = Just ( Down, 5 ) }
            , Item { value = "", clueId1 = ( Across, 19 ), clueId2 = Nothing }
            , NumberedItem 20 { value = "", clueId1 = ( Across, 19 ), clueId2 = Just ( Down, 20 ) }
            , Item { value = "", clueId1 = ( Across, 19 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 19 ), clueId2 = Just ( Down, 7 ) }
            , Item { value = "", clueId1 = ( Across, 19 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 13 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 17 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 14 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 18 ), clueId2 = Nothing }
            , Black
            , Black
            , Black
            , Item { value = "", clueId1 = ( Down, 20 ), clueId2 = Nothing }
            , Black
            , Black
            , Black
            , NumberedItem 21 { value = "", clueId1 = ( Across, 21 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 21 ), clueId2 = Just ( Down, 13 ) }
            , Item { value = "", clueId1 = ( Across, 21 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 21 ), clueId2 = Just ( Down, 17 ) }
            , Item { value = "", clueId1 = ( Across, 21 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 21 ), clueId2 = Just ( Down, 14 ) }
            , Item { value = "", clueId1 = ( Across, 21 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 21 ), clueId2 = Just ( Down, 18 ) }
            , Black
            , NumberedItem 22 { value = "", clueId1 = ( Across, 22 ), clueId2 = Just ( Down, 22 ) }
            , Item { value = "", clueId1 = ( Across, 22 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 22 ), clueId2 = Just ( Down, 20 ) }
            , Item { value = "", clueId1 = ( Across, 22 ), clueId2 = Nothing }
            , NumberedItem 23 { value = "", clueId1 = ( Across, 22 ), clueId2 = Just ( Down, 23 ) }
            , Item { value = "", clueId1 = ( Across, 22 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 13 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 17 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 14 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 18 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 22 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 20 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 23 ), clueId2 = Nothing }
            , Black
            , NumberedItem 24 { value = "", clueId1 = ( Across, 24 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 24 ), clueId2 = Just ( Down, 13 ) }
            , Item { value = "", clueId1 = ( Across, 24 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 24 ), clueId2 = Just ( Down, 17 ) }
            , Item { value = "", clueId1 = ( Across, 24 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 24 ), clueId2 = Just ( Down, 14 ) }
            , Black
            , NumberedItem 25 { value = "", clueId1 = ( Across, 25 ), clueId2 = Just ( Down, 18 ) }
            , Item { value = "", clueId1 = ( Across, 25 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 25 ), clueId2 = Just ( Down, 22 ) }
            , Item { value = "", clueId1 = ( Across, 25 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 25 ), clueId2 = Just ( Down, 20 ) }
            , Item { value = "", clueId1 = ( Across, 25 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 25 ), clueId2 = Just ( Down, 23 ) }
            , Item { value = "", clueId1 = ( Across, 25 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 13 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 17 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 14 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 18 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 22 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 20 ), clueId2 = Nothing }
            , Black
            , Item { value = "", clueId1 = ( Down, 23 ), clueId2 = Nothing }
            , Black
            , NumberedItem 26 { value = "", clueId1 = ( Across, 26 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 26 ), clueId2 = Just ( Down, 13 ) }
            , Item { value = "", clueId1 = ( Across, 26 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 26 ), clueId2 = Just ( Down, 17 ) }
            , Black
            , NumberedItem 27 { value = "", clueId1 = ( Across, 27 ), clueId2 = Just ( Down, 14 ) }
            , Item { value = "", clueId1 = ( Across, 27 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 27 ), clueId2 = Just ( Down, 18 ) }
            , Item { value = "", clueId1 = ( Across, 27 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 27 ), clueId2 = Just ( Down, 22 ) }
            , Item { value = "", clueId1 = ( Across, 27 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 27 ), clueId2 = Just ( Down, 20 ) }
            , Item { value = "", clueId1 = ( Across, 27 ), clueId2 = Nothing }
            , Item { value = "", clueId1 = ( Across, 27 ), clueId2 = Just ( Down, 23 ) }
            , Black
            ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change Int String
    | Focus Int CellData
    | Click Int CellData
    | FocusResult (Result Dom.Error ())
    | KeyTouched KeyEventMsg


focusCell : Int -> Cmd Msg
focusCell index =
    Dom.focus (String.fromInt index) |> Task.attempt FocusResult


onCellSelected : msg -> Html.Attribute msg
onCellSelected message =
    on "click" (Decode.succeed message)


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
        newDirection =
            case cellData.clueId2 of
                Just clue ->
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
    , Cmd.none
    )


calculateModelAfterFocus : Model -> Int -> CellData -> ( Model, Cmd Msg )
calculateModelAfterFocus model index cellData =
    let
        newDirection =
            case cellData.clueId2 of
                Just clue ->
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
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Debug.log "update"
        (case msg of
            Change _ " " ->
                ( model, Cmd.none )

            Change index "" ->
                ( { model | grid = updateGrid model.grid index "" }, Cmd.none )

            Change index newContent ->
                let
                    nextIndex =
                        if model.currentDirection == Across then
                            getRightWhiteIndex model.grid index

                        else
                            getDownWhiteIndex model
                in
                ( { model | grid = updateGrid model.grid index newContent, currentIndex = nextIndex }, focusCell nextIndex )

            Focus index cellData ->
                calculateModelAfterFocus model index cellData

            Click index cellData ->
                calculateModelAfterClick model index cellData

            FocusResult _ ->
                ( model, Cmd.none )

            KeyTouched keyEventMsg ->
                case keyEventMsg of
                    TabPressed ->
                        if model.shiftHeld == True then
                            let
                                nextIndex =
                                    getLeftWhiteIndex model.grid model.currentIndex
                            in
                            ( { model | currentIndex = nextIndex, currentDirection = Across }, focusCell nextIndex )

                        else
                            let
                                nextIndex =
                                    getRightWhiteIndex model.grid model.currentIndex
                            in
                            ( { model | currentIndex = nextIndex, currentDirection = Across }, focusCell nextIndex )

                    ShiftPressed ->
                        ( { model | shiftHeld = True }, Cmd.none )

                    ShiftReleased ->
                        ( { model | shiftHeld = False }, Cmd.none )

                    LeftPressed ->
                        let
                            nextIndex =
                                getLeftWhiteIndex model.grid model.currentIndex
                        in
                        ( { model | currentIndex = nextIndex, currentDirection = Across }, focusCell nextIndex )

                    RightPressed ->
                        let
                            nextIndex =
                                getRightWhiteIndex model.grid model.currentIndex
                        in
                        ( { model | currentIndex = nextIndex, currentDirection = Across }, focusCell nextIndex )

                    UpPressed ->
                        let
                            nextIndex =
                                getUpWhiteIndex model
                        in
                        ( { model | currentIndex = nextIndex, currentDirection = Down }, focusCell nextIndex )

                    KeyPressed ->
                        let
                            nextIndex =
                                getDownWhiteIndex model
                        in
                        ( { model | currentIndex = nextIndex, currentDirection = Down }, focusCell nextIndex )

                    _ ->
                        ( model, Cmd.none )
        )


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
                    NumberedItem number cellData ->
                        NumberedItem number { cellData | value = String.right 1 newContent }

                    Item cellData ->
                        Item { cellData | value = String.right 1 newContent }

                    Black ->
                        Black
            )



-- VIEW


view : Model -> Html Msg
view model =
    Debug.log "view"
        div
        []
        [ viewPuzzle model
        , if model.showDebug then
            Debug.log "debug = true" (debug model)

          else
            div [] []
        ]


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
        [ viewDebug "Current Cell" cellString
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
        [ viewGrid model
        , viewCluesSection model Across model.clues.across
        , viewCluesSection model Down model.clues.down
        ]


type PrintableUnion
    = Int
    | String


type Printable
    = PrintableInt Int
    | PrintableString String


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
    Debug.log "viewClue"
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


getGridTemplate : Model -> String
getGridTemplate model =
    let
        rowCount =
            sqrt (toFloat (List.length model.grid))

        singleCellPercentage =
            100 / rowCount
    in
    String.concat [ "repeat(", String.fromFloat rowCount, ", ", String.fromFloat singleCellPercentage, "%)/repeat(", String.fromFloat rowCount, ", ", String.fromFloat singleCellPercentage, "%)" ]



-- NumberedItem 1 { value = "", clue1 = ( Across, 1 ), clue2 = Just ( Down, 1 ) }


shouldHighlight : Model -> CellData -> Bool
shouldHighlight model cellData =
    case cellData.clueId2 of
        Just x ->
            (x == model.currentClue && Tuple.first x == model.currentDirection) || cellData.clueId1 == model.currentClue

        Nothing ->
            cellData.clueId1 == model.currentClue



-- viewCell1 : Cell -> Int -> Html.Attribute Msg -> Html.Attribute Msg -> String -> Bool -> Html Msg
-- viewCell1 cell index border zIndex backgroundColor selected =


viewCell1 : Cell -> Int -> string -> Html Msg
viewCell1 cell index string =
    Debug.log (String.concat [ "viewCell1 " ])
        (case cell of
            Item cellData ->
                div
                    [ style "position" "relative"
                    ]
                    [ input
                        [ id (String.fromInt index)
                        , placeholder ""
                        , value cellData.value
                        , onInput (Change index)
                        , onFocus (Focus index cellData)
                        , onClick (Click index cellData)
                        , style "text-transform" "uppercase"
                        , style "box-sizing" "border-box"
                        , style "position" "relative"
                        , style "outline" "none"
                        , style "text-align" "center"
                        , style "font-size" "20px"
                        , style "font-weight" "bold"
                        , style "background" "transparent"
                        , style "height" "50px"
                        , style "width" "50px"
                        ]
                        []
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
                        , value cellData.value
                        , onInput (Change index)
                        , onFocus (Focus index cellData)
                        , onClick (Click index cellData)
                        , style "text-transform" "uppercase"
                        , style "box-sizing" "border-box"
                        , style "outline" "none"
                        , style "text-align" "center"
                        , style "font-size" "20px"
                        , style "font-weight" "bold"
                        , style "background" "transparent"
                        , style "width" "50px"
                        , style "height" "50px"
                        ]
                        []
                    ]

            Black ->
                div
                    [ style "background-color" "black"
                    ]
                    []
        )


viewCell : Cell -> Int -> String -> String -> String -> Bool -> Html Msg
viewCell cell index border zIndex backgroundColor selected =
    Debug.log (String.concat [ "viewCell ", String.fromInt index, " " ])
        (case cell of
            Item cellData ->
                div
                    [ style "position" "relative"
                    ]
                    [ input
                        [ id (String.fromInt index)
                        , placeholder ""
                        , value cellData.value
                        , onInput (Change index)
                        , onFocus (Focus index cellData)
                        , onClick (Click index cellData)
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
                        []
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
                        , value cellData.value
                        , onInput (Change index)
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
                        []
                    ]

            Black ->
                div
                    [ style "background-color" "black"
                    ]
                    []
        )


viewCellAndModel : Model -> Int -> Cell -> Html Msg
viewCellAndModel model index cell =
    let
        highlight =
            case cell of
                Black ->
                    False

                Item cellData ->
                    shouldHighlight model cellData

                NumberedItem _ cellData ->
                    shouldHighlight model cellData

        backgroundColor =
            if highlight then
                "yellow"

            else
                "white"

        selected =
            index == model.currentIndex

        zIndex =
            if selected then
                "10"

            else
                "1"

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
    = KeyEventLetter Char
    | KeyEventUnknown String
    | TabPressed
    | ShiftPressed
    | ShiftReleased
    | LeftPressed
    | RightPressed
    | UpPressed
    | KeyPressed


keyReleasedDecoder : Decode.Decoder Msg
keyReleasedDecoder =
    Debug.log "keyReleasedDecoder"
        Decode.map
        (keyReleasedToKeyEventMsg >> KeyTouched)
        (Decode.field "key" Decode.string)


keyPressedDecoder : Decode.Decoder Msg
keyPressedDecoder =
    Debug.log "keyPressedDecoder"
        Decode.map
        (keyPressedToKeyEventMsg >> KeyTouched)
        (Decode.field "key" Decode.string)


keyReleasedToKeyEventMsg : String -> KeyEventMsg
keyReleasedToKeyEventMsg eventKeyString =
    case eventKeyString of
        "Shift" ->
            ShiftReleased

        _ ->
            KeyEventUnknown eventKeyString


keyPressedToKeyEventMsg : String -> KeyEventMsg
keyPressedToKeyEventMsg eventKeyString =
    case eventKeyString of
        "ArrowLeft" ->
            LeftPressed

        "Shift" ->
            ShiftPressed

        "Tab" ->
            TabPressed

        "ArrowRight" ->
            RightPressed

        "ArrowUp" ->
            UpPressed

        "ArrowDown" ->
            KeyPressed

        string ->
            case String.uncons string of
                Just ( char, "" ) ->
                    KeyEventLetter char

                _ ->
                    KeyEventUnknown eventKeyString
