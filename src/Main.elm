module Main exposing (getColumnNumber, getRowNumber)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (id, placeholder, style, value)
import Html.Events exposing (keyCode, onClick, onFocus, onInput, preventDefaultOn)
import Html.Lazy
import Json.Decode as Decode
import List.Extra
import Task
import Types exposing (Cell(..), CellData, Clue, Crossword, Direction(..), Model, State)



-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = \_ -> init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


init : ( Model, Cmd Msg )
init =
    ( { latestString = ""
      , shiftHeld = False
      , crossword = initCrossword
      , state =
            { clueId = { direction = Across, number = 1 }
            , direction = Across
            , index = 1
            }
      }
    , focusTextInput
    )



-- UPDATE


type Msg
    = Change Int (Maybe Char)
    | Focus Int CellData
    | Click Int CellData
    | KeyTouched KeyEventMsg
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
                    if model.state.direction == Down then
                        Across

                    else
                        Down

                Nothing ->
                    cellData.clueId1.direction

        state : State
        state =
            model.state

        newState : State
        newState =
            { state
                | index = index
                , direction = newDirection
                , clueId =
                    if cellData.clueId1.direction == newDirection then
                        cellData.clueId1

                    else
                        case cellData.clueId2 of
                            Just clue ->
                                clue

                            Nothing ->
                                cellData.clueId1
            }
    in
    ( { model
        | state = newState
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
                    model.state.direction

                Nothing ->
                    cellData.clueId1.direction

        state : State
        state =
            model.state

        newState : State
        newState =
            { state
                | index = index
                , direction = newDirection
                , clueId =
                    if cellData.clueId1.direction == newDirection then
                        cellData.clueId1

                    else
                        case cellData.clueId2 of
                            Just clue ->
                                clue

                            Nothing ->
                                cellData.clueId1
            }
    in
    ( { model
        | state = newState
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
                            if model.state.direction == Across then
                                getRightWhiteIndex model.crossword.grid index

                            else
                                getDownWhiteIndex model

                        state : State
                        state =
                            model.state

                        newState : State
                        newState =
                            { state | index = nextIndex }
                    in
                    ( { model | latestString = String.fromChar char, crossword = updateCrossword model.crossword index newContent, state = newState }, focusTextInput )

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
                    moveToNextWhiteCell model model.state.direction model.shiftHeld

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

        NoOp ->
            ( model, Cmd.none )


updateCrossword : Crossword -> Int -> Maybe Char -> Crossword
updateCrossword crossword index newContent =
    { crossword | grid = updateGrid crossword.grid index newContent }


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
                    getNextWhiteCell model model.state.direction True

                _ ->
                    model.state.index

        state : State
        state =
            model.state

        newState : State
        newState =
            { state | index = nextIndex }
    in
    ( { model | state = newState, latestString = "", crossword = updateCrossword model.crossword model.state.index Nothing }, focusTextInput )


getNextWhiteCell : Model -> Direction -> Bool -> Int
getNextWhiteCell model direction backwards =
    if direction == Across then
        if backwards then
            getLeftWhiteIndex model.crossword.grid model.state.index

        else
            getRightWhiteIndex model.crossword.grid model.state.index

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

        state : State
        state =
            model.state

        newState : State
        newState =
            { state | index = nextIndex, direction = direction }
    in
    ( { model | state = newState }, focusTextInput )


getCurrentCellChar : Model -> Maybe Char
getCurrentCellChar model =
    let
        cell : Maybe Cell
        cell =
            elementAtIndex (model.state.index + 1) model.crossword.grid
    in
    case cell of
        Just (Item cellData) ->
            cellData.value

        Just (NumberedItem _ cellData) ->
            cellData.value

        _ ->
            Nothing


getLeftWhiteIndex : List Cell -> Int -> Int
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
            takeEveryNthIndexesFromIndex model.crossword.numberOfRows columnNumber model.crossword.grid

        columnsDown : List Cell
        columnsDown =
            Tuple.second (List.Extra.splitAt rowNumber columnSquares)

        index : Maybe Int
        index =
            List.Extra.findIndex isWhiteSquare columnsDown
    in
    case index of
        Just n ->
            model.state.index + (model.crossword.numberOfRows * (n + 1))

        Nothing ->
            -- reached the last square
            model.state.index


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
            takeEveryNthIndexesFromIndex model.crossword.numberOfRows columnNumber model.crossword.grid

        columnsUp : List Cell
        columnsUp =
            List.reverse (Tuple.first (List.Extra.splitAt (rowNumber - 1) columnSquares))

        index : Maybe Int
        index =
            List.Extra.findIndex isWhiteSquare columnsUp
    in
    case index of
        Just n ->
            model.state.index - (model.crossword.numberOfRows * (n + 1))

        Nothing ->
            -- reached the last square
            model.state.index


currentColumnNumber : Model -> Int
currentColumnNumber model =
    getColumnNumber model.crossword.numberOfColumns model.state.index


getColumnNumber : Int -> Int -> Int
getColumnNumber numberOfColumns index =
    modBy numberOfColumns index + 1


currentRowNumber : Model -> Int
currentRowNumber model =
    getRowNumber model.crossword.numberOfColumns model.state.index


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


getRightWhiteIndex : List Cell -> Int -> Int
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


updateGrid : List Cell -> Int -> Maybe Char -> List Cell
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
        ]
    }


viewPuzzle : Model -> Html Msg
viewPuzzle model =
    div
        [ style "display" "flex"
        ]
        [ viewGridWithInput model
        , viewCluesSection model Across model.crossword.clues.across
        , viewCluesSection model Down model.crossword.clues.down
        ]


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


viewClue : String -> Clue -> Html Msg
viewClue backgroundColor clue =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "background-color" backgroundColor
        ]
        [ textDiv (String.fromInt clue.number)
        , textDiv clue.text
        ]


viewClueAndModelAndDirection : Model -> Direction -> Clue -> Html Msg
viewClueAndModelAndDirection model direction clue =
    let
        backgroundColor : String
        backgroundColor =
            if model.state.clueId.number == clue.number && direction == model.state.clueId.direction then
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
        (List.indexedMap (viewCellAndModel model) model.crossword.grid)


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
    Change model.state.index (List.head (List.reverse (String.toList string)))


getGridTemplate : Model -> String
getGridTemplate model =
    let
        rowCount : Float
        rowCount =
            sqrt (toFloat (List.length model.crossword.grid))

        singleCellPercentage : Float
        singleCellPercentage =
            100 / rowCount
    in
    String.concat [ "repeat(", String.fromFloat rowCount, ", ", String.fromFloat singleCellPercentage, "%)/repeat(", String.fromFloat rowCount, ", ", String.fromFloat singleCellPercentage, "%)" ]


shouldHighlight : Model -> CellData -> Bool
shouldHighlight model cellData =
    case cellData.clueId2 of
        Just clueId ->
            (clueId == model.state.clueId && clueId.direction == model.state.direction) || cellData.clueId1 == model.state.clueId

        Nothing ->
            cellData.clueId1 == model.state.clueId


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
            index == model.state.index

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


initCrossword : Crossword
initCrossword =
    { numberOfColumns = 15
    , numberOfRows = 15
    , clues =
        { across =
            [ { number = 1, text = "Woman's tucked into ridiculously pricey dessert (6,3)" }
            , { number = 6, text = "Exercise with walks occasionally on top of a mountain (4)" }
            , { number = 8, text = "Lyrics discovered in jotter Bill returned (8)" }
            , { number = 9, text = "Think about body shape (6)" }
            , { number = 10, text = "Liberal leader's cagey about inheritance (6)" }
            , { number = 11, text = "Market put up inside old church (8)" }
            , { number = 12, text = "Harpoons fish, filling ship (6)" }
            , { number = 15, text = "Understand flipping idiots caught cheating (8)" }
            , { number = 16, text = "Hoping painkiller's good (8)" }
            , { number = 19, text = "Realise student dropped out to get less stressed (6)" }
            , { number = 21, text = "A family briefly squeeze in post-winter sports activity (5-3)" }
            , { number = 22, text = "Barrels with untaxed liquor initially hidden inside cellars (6)" }
            , { number = 24, text = "Popular archaeological site has nothing in the shade (6)" }
            , { number = 25, text = "Customs document chap provided is in French (8)" }
            , { number = 26, text = "Jam's without sharpness (4)" }
            , { number = 27, text = "Reportedly praise soldiers protecting former PM and civic dignitary (4,5)" }
            ]
        , down =
            [ { number = 1, text = "Scold child with proof of age he's defaced (5)" }
            , { number = 2, text = "Allocate English vessel to take on board a Royal Marine (7)" }
            , { number = 3, text = "It's unknowns supporting Radiohead — splendid! (5)" }
            , { number = 4, text = "Design for college is set in stone (7)" }
            , { number = 5, text = "Iron both ways, beginning with crease and ribbon round top of vest, working well (9)" }
            , { number = 6, text = "Summon soldier for parade (7)" }
            , { number = 7, text = "Excessive pride in hideous orange car (9)" }
            , { number = 13, text = "Put off by pressure working in new depots (9)" }
            , { number = 14, text = "Small women's group with case of supplies, bread and cake (5,4)" }
            , { number = 17, text = "List articles about Italy and Spain (7)" }
            , { number = 18, text = "Suggestion Emma endlessly upset girl running around (7)" }
            , { number = 20, text = "Question doubtful point before getting drunk (7)" }
            , { number = 22, text = "Bitterness against reduced cashback (5)" }
            , { number = 23, text = "Stun with outrageous rates (5)" }
            ]
        }
    , grid =
        [ Black
        , NumberedItem 1 { value = Nothing, clueId1 = { direction = Across, number = 1 }, clueId2 = Just { direction = Down, number = 1 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 1 }, clueId2 = Nothing }
        , NumberedItem 2 { value = Nothing, clueId1 = { direction = Across, number = 1 }, clueId2 = Just { direction = Down, number = 2 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 1 }, clueId2 = Nothing }
        , NumberedItem 3 { value = Nothing, clueId1 = { direction = Across, number = 1 }, clueId2 = Just { direction = Down, number = 3 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 1 }, clueId2 = Nothing }
        , NumberedItem 4 { value = Nothing, clueId1 = { direction = Across, number = 1 }, clueId2 = Just { direction = Down, number = 4 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 1 }, clueId2 = Nothing }
        , NumberedItem 5 { value = Nothing, clueId1 = { direction = Across, number = 1 }, clueId2 = Just { direction = Down, number = 5 } }
        , Black
        , NumberedItem 6 { value = Nothing, clueId1 = { direction = Across, number = 6 }, clueId2 = Just { direction = Down, number = 6 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 6 }, clueId2 = Nothing }
        , NumberedItem 7 { value = Nothing, clueId1 = { direction = Across, number = 6 }, clueId2 = Just { direction = Down, number = 7 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 6 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 1 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 2 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 3 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 4 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 5 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 6 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 7 }, clueId2 = Nothing }
        , Black
        , NumberedItem 8 { value = Nothing, clueId1 = { direction = Across, number = 8 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 8 }, clueId2 = Just { direction = Down, number = 1 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 8 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 8 }, clueId2 = Just { direction = Down, number = 2 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 8 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 8 }, clueId2 = Just { direction = Down, number = 3 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 8 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 8 }, clueId2 = Just { direction = Down, number = 4 } }
        , Black
        , NumberedItem 9 { value = Nothing, clueId1 = { direction = Across, number = 9 }, clueId2 = Just { direction = Down, number = 5 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 9 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 9 }, clueId2 = Just { direction = Down, number = 6 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 9 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 9 }, clueId2 = Just { direction = Down, number = 7 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 9 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 1 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 2 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 3 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 4 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 5 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 6 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 7 }, clueId2 = Nothing }
        , Black
        , NumberedItem 10 { value = Nothing, clueId1 = { direction = Across, number = 10 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 10 }, clueId2 = Just { direction = Down, number = 1 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 10 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 10 }, clueId2 = Just { direction = Down, number = 2 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 10 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 10 }, clueId2 = Just { direction = Down, number = 3 } }
        , Black
        , NumberedItem 11 { value = Nothing, clueId1 = { direction = Across, number = 11 }, clueId2 = Just { direction = Down, number = 4 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 11 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 11 }, clueId2 = Just { direction = Down, number = 5 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 11 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 11 }, clueId2 = Just { direction = Down, number = 6 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 11 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 11 }, clueId2 = Just { direction = Down, number = 7 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 11 }, clueId2 = Nothing }
        , Black
        , Black
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 2 }, clueId2 = Nothing }
        , Black
        , Black
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 4 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 5 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 6 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 7 }, clueId2 = Nothing }
        , Black
        , NumberedItem 12 { value = Nothing, clueId1 = { direction = Across, number = 12 }, clueId2 = Nothing }
        , NumberedItem 13 { value = Nothing, clueId1 = { direction = Across, number = 12 }, clueId2 = Just { direction = Down, number = 13 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 12 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 12 }, clueId2 = Just { direction = Down, number = 2 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 12 }, clueId2 = Nothing }
        , NumberedItem 14 { value = Nothing, clueId1 = { direction = Across, number = 12 }, clueId2 = Just { direction = Down, number = 14 } }
        , Black
        , NumberedItem 15 { value = Nothing, clueId1 = { direction = Across, number = 15 }, clueId2 = Just { direction = Down, number = 4 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 15 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 15 }, clueId2 = Just { direction = Down, number = 5 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 15 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 15 }, clueId2 = Just { direction = Down, number = 6 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 15 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 15 }, clueId2 = Just { direction = Down, number = 7 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 15 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 13 }, clueId2 = Nothing }
        , Black
        , Black
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 14 }, clueId2 = Nothing }
        , Black
        , Black
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 5 }, clueId2 = Nothing }
        , Black
        , Black
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 7 }, clueId2 = Nothing }
        , Black
        , NumberedItem 16 { value = Nothing, clueId1 = { direction = Across, number = 16 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 16 }, clueId2 = Just { direction = Down, number = 13 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 16 }, clueId2 = Nothing }
        , NumberedItem 17 { value = Nothing, clueId1 = { direction = Across, number = 16 }, clueId2 = Just { direction = Down, number = 17 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 16 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 16 }, clueId2 = Just { direction = Down, number = 14 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 16 }, clueId2 = Nothing }
        , NumberedItem 18 { value = Nothing, clueId1 = { direction = Across, number = 16 }, clueId2 = Just { direction = Down, number = 18 } }
        , Black
        , NumberedItem 19 { value = Nothing, clueId1 = { direction = Across, number = 19 }, clueId2 = Just { direction = Down, number = 5 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 19 }, clueId2 = Nothing }
        , NumberedItem 20 { value = Nothing, clueId1 = { direction = Across, number = 19 }, clueId2 = Just { direction = Down, number = 20 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 19 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 19 }, clueId2 = Just { direction = Down, number = 7 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 19 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 13 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 17 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 14 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 18 }, clueId2 = Nothing }
        , Black
        , Black
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 20 }, clueId2 = Nothing }
        , Black
        , Black
        , Black
        , NumberedItem 21 { value = Nothing, clueId1 = { direction = Across, number = 21 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 21 }, clueId2 = Just { direction = Down, number = 13 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 21 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 21 }, clueId2 = Just { direction = Down, number = 17 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 21 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 21 }, clueId2 = Just { direction = Down, number = 14 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 21 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 21 }, clueId2 = Just { direction = Down, number = 18 } }
        , Black
        , NumberedItem 22 { value = Nothing, clueId1 = { direction = Across, number = 22 }, clueId2 = Just { direction = Down, number = 22 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 22 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 22 }, clueId2 = Just { direction = Down, number = 20 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 22 }, clueId2 = Nothing }
        , NumberedItem 23 { value = Nothing, clueId1 = { direction = Across, number = 22 }, clueId2 = Just { direction = Down, number = 23 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 22 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 13 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 17 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 14 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 18 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 22 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 20 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 23 }, clueId2 = Nothing }
        , Black
        , NumberedItem 24 { value = Nothing, clueId1 = { direction = Across, number = 24 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 24 }, clueId2 = Just { direction = Down, number = 13 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 24 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 24 }, clueId2 = Just { direction = Down, number = 17 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 24 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 24 }, clueId2 = Just { direction = Down, number = 14 } }
        , Black
        , NumberedItem 25 { value = Nothing, clueId1 = { direction = Across, number = 25 }, clueId2 = Just { direction = Down, number = 18 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 25 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 25 }, clueId2 = Just { direction = Down, number = 22 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 25 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 25 }, clueId2 = Just { direction = Down, number = 20 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 25 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 25 }, clueId2 = Just { direction = Down, number = 23 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 25 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 13 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 17 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 14 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 18 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 22 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 20 }, clueId2 = Nothing }
        , Black
        , Item { value = Nothing, clueId1 = { direction = Down, number = 23 }, clueId2 = Nothing }
        , Black
        , NumberedItem 26 { value = Nothing, clueId1 = { direction = Across, number = 26 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 26 }, clueId2 = Just { direction = Down, number = 13 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 26 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 26 }, clueId2 = Just { direction = Down, number = 17 } }
        , Black
        , NumberedItem 27 { value = Nothing, clueId1 = { direction = Across, number = 27 }, clueId2 = Just { direction = Down, number = 14 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 27 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 27 }, clueId2 = Just { direction = Down, number = 18 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 27 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 27 }, clueId2 = Just { direction = Down, number = 22 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 27 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 27 }, clueId2 = Just { direction = Down, number = 20 } }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 27 }, clueId2 = Nothing }
        , Item { value = Nothing, clueId1 = { direction = Across, number = 27 }, clueId2 = Just { direction = Down, number = 23 } }
        , Black
        ]
    }
