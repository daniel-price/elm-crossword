module Crossword exposing (Msg, getColumnNumber, getRowNumber, htmlView, init, main, subscriptions, update)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (id, placeholder, style, value)
import Html.Events exposing (keyCode, onClick, onFocus, onInput, preventDefaultOn)
import Html.Lazy
import Http
import Json.Decode as Decode exposing (Error)
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode
import List.Extra
import Ports exposing (messageReceiver, sendMessage)
import Task
import Types exposing (Cell(..), CellData, Clue, ClueId, Crossword, CrosswordId, Data, Direction(..), Model(..), State, WebsocketMessage)



-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = \_ -> init "placeholderId", update = update, view = view, subscriptions = subscriptions }



-- MODEL


init : CrosswordId -> ( Model, Cmd Msg )
init crosswordId =
    ( Loading
    , Http.get
        { url = String.concat [ "http://localhost:8080/crossword/", crosswordId ]
        , expect = Http.expectJson GotCrossword crosswordDecoder
        }
    )


crosswordDecoder : Decode.Decoder Crossword
crosswordDecoder =
    Decode.succeed Crossword
        |> DecodePipeline.required "grid" (Decode.list cellDecoder)
        |> DecodePipeline.required "clues" clueDecoder
        |> DecodePipeline.required "numberOfColumns" Decode.int
        |> DecodePipeline.required "numberOfRows" Decode.int


clueDecoder : Decode.Decoder Types.Clues
clueDecoder =
    Decode.succeed Types.Clues
        |> DecodePipeline.required "across" (Decode.list decodeClue)
        |> DecodePipeline.required "down" (Decode.list decodeClue)


decodeClue : Decode.Decoder Clue
decodeClue =
    Decode.succeed Clue
        |> DecodePipeline.required "value" Decode.string
        |> DecodePipeline.required "number" Decode.int


cellDecoder : Decode.Decoder Cell
cellDecoder =
    Decode.oneOf [ decodeWhite, decodeBlack ]


decodeWhite : Decode.Decoder Cell
decodeWhite =
    exactMatch (Decode.field "type" Decode.string)
        "White"
        (Decode.map White <| Decode.field "cellData" decodeCellData)


decodeCellData : Decode.Decoder CellData
decodeCellData =
    Decode.succeed CellData
        |> DecodePipeline.required "clueId" decodeClueId
        |> DecodePipeline.optional "clueId2" (Decode.map Just decodeClueId) Nothing
        |> DecodePipeline.optional "value" decodeChar Nothing
        |> DecodePipeline.optional "number" (Decode.map Just Decode.int) Nothing


decodeChar : Decode.Decoder (Maybe Char)
decodeChar =
    Decode.string
        |> Decode.andThen toChar


toChar : String -> Decode.Decoder (Maybe Char)
toChar string =
    Decode.succeed (List.head (String.toList string))


decodeClueId : Decode.Decoder ClueId
decodeClueId =
    Decode.succeed ClueId
        |> DecodePipeline.required "direction" decodeDirection
        |> DecodePipeline.required "number" Decode.int


decodeDirection : Decode.Decoder Direction
decodeDirection =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Across" ->
                        Decode.succeed Across

                    "Down" ->
                        Decode.succeed Down

                    _ ->
                        Decode.fail "Invalid Direction"
            )


decodeBlack : Decode.Decoder Cell
decodeBlack =
    exactMatch (Decode.field "type" Decode.string) "Black" (Decode.succeed Black)


exactMatch : Decode.Decoder String -> String -> Decode.Decoder a -> Decode.Decoder a
exactMatch matchDecoder match dec =
    matchDecoder
        |> Decode.andThen
            (\str ->
                if str == match then
                    dec

                else
                    Decode.fail <| "[exactMatch] tgt: " ++ match ++ " /= " ++ str
            )


type Msg
    = Change Int (Maybe Char)
    | Focus Int CellData
    | Click Int CellData
    | KeyTouched KeyEventMsg
    | GotCrossword (Result Http.Error Crossword)
    | Recv WebsocketMessage
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


calculateDataAfterClick : Data -> Int -> CellData -> ( Model, Cmd Msg )
calculateDataAfterClick model index cellData =
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
    ( Success
        { model
            | state = newState
        }
    , focusTextInput
    )


calculateDataAfterFocus : Data -> Int -> CellData -> ( Model, Cmd Msg )
calculateDataAfterFocus model index cellData =
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
    ( Success
        { model
            | state = newState
        }
    , focusTextInput
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Recv message ->
            case model of
                Failure _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Success data ->
                    let
                        index : Int
                        index =
                            (message.y - 1) * data.crossword.numberOfColumns + message.x - 1

                        newData : Data
                        newData =
                            { data | crossword = updateCrossword data.crossword index (List.head (String.toList message.value)) }
                    in
                    ( Success newData, focusTextInput )

        GotCrossword result ->
            case result of
                Ok crossword ->
                    let
                        data : Data
                        data =
                            dataFromCrossword crossword
                    in
                    ( Success data, focusTextInput )

                Err err ->
                    ( Failure err, Cmd.none )

        Change index newContent ->
            case model of
                Failure _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Success data ->
                    case newContent of
                        Nothing ->
                            let
                                currentCellChar : Maybe Char
                                currentCellChar =
                                    getCurrentCellChar data

                                nextIndex : Int
                                nextIndex =
                                    case currentCellChar of
                                        Nothing ->
                                            getNextWhiteCell data data.state.direction True

                                        _ ->
                                            data.state.index

                                state : State
                                state =
                                    data.state

                                newState : State
                                newState =
                                    { state | index = nextIndex }

                                newData : Data
                                newData =
                                    { data | state = newState, latestString = "", crossword = updateCrossword data.crossword data.state.index Nothing }
                            in
                            ( Success newData, focusTextInput )

                        Just char ->
                            let
                                nextIndex : Int
                                nextIndex =
                                    if data.state.direction == Across then
                                        getRightWhiteIndex data.crossword.grid index

                                    else
                                        getDownWhiteIndex data

                                state : State
                                state =
                                    data.state

                                newState : State
                                newState =
                                    { state | index = nextIndex }

                                x : Int
                                x =
                                    getColumnNumber data.crossword.numberOfColumns index

                                y : Int
                                y =
                                    getRowNumber data.crossword.numberOfColumns index

                                commands : Cmd Msg
                                commands =
                                    Cmd.batch [ focusTextInput, sendMessage (encode x y (String.fromChar char)) ]
                            in
                            ( Success { data | latestString = String.fromChar char, crossword = updateCrossword data.crossword index newContent, state = newState }, commands )

        Focus index cellData ->
            case model of
                Failure _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Success data ->
                    calculateDataAfterFocus
                        data
                        index
                        cellData

        Click index cellData ->
            case model of
                Failure _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Success data ->
                    calculateDataAfterClick data index cellData

        KeyTouched keyEventMsg ->
            case model of
                Failure _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Success data ->
                    case keyEventMsg of
                        BackspacePressed ->
                            let
                                currentCellChar : Maybe Char
                                currentCellChar =
                                    getCurrentCellChar data

                                nextIndex : Int
                                nextIndex =
                                    case currentCellChar of
                                        Nothing ->
                                            getNextWhiteCell data data.state.direction True

                                        _ ->
                                            data.state.index

                                state : State
                                state =
                                    data.state

                                newState : State
                                newState =
                                    { state | index = nextIndex }
                            in
                            ( Success { data | state = newState, latestString = "", crossword = updateCrossword data.crossword data.state.index Nothing }, focusTextInput )

                        TabPressed ->
                            moveToNextWhiteCell data data.state.direction data.shiftHeld

                        ShiftPressed ->
                            ( Success { data | shiftHeld = True }, Cmd.none )

                        ShiftReleased ->
                            ( Success { data | shiftHeld = False }, Cmd.none )

                        LeftPressed ->
                            moveToNextWhiteCell data Across True

                        RightPressed ->
                            moveToNextWhiteCell data Across False

                        UpPressed ->
                            moveToNextWhiteCell data Down True

                        DownPressed ->
                            moveToNextWhiteCell data Down False

                        _ ->
                            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


encode : Int -> Int -> String -> Json.Encode.Value
encode x y value =
    Json.Encode.object
        [ ( "value", Json.Encode.string value )
        , ( "x", Json.Encode.int x )
        , ( "y", Json.Encode.int y )
        ]


decode : Decode.Decoder WebsocketMessage
decode =
    Decode.map3 WebsocketMessage
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)
        (Decode.field "value" Decode.string)


isWhite : Cell -> Bool
isWhite cell =
    case cell of
        White _ ->
            True

        _ ->
            False


dataFromCrossword : Crossword -> Data
dataFromCrossword crossword =
    let
        index : Int
        index =
            Maybe.withDefault 0 (List.Extra.findIndex isWhite crossword.grid)

        clueId : ClueId
        clueId =
            case List.Extra.getAt index crossword.grid of
                Just (White cell) ->
                    cell.clueId1

                _ ->
                    { direction = Across, number = 1 }

        state : State
        state =
            { clueId = clueId
            , direction = clueId.direction
            , index = index
            }
    in
    { crossword = crossword
    , state = state
    , shiftHeld = False
    , latestString = ""
    }


updateCrossword : Crossword -> Int -> Maybe Char -> Crossword
updateCrossword crossword index newContent =
    { crossword | grid = updateGrid crossword.grid index newContent }


getNextWhiteCell : Data -> Direction -> Bool -> Int
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


moveToNextWhiteCell : Data -> Direction -> Bool -> ( Model, Cmd Msg )
moveToNextWhiteCell model direction backwards =
    let
        nextIndex : Int
        nextIndex =
            getNextWhiteCell model direction backwards

        state : State
        state =
            model.state

        cell : Maybe Cell
        cell =
            List.Extra.getAt nextIndex model.crossword.grid

        cellData : Maybe CellData
        cellData =
            case cell of
                Just (White cd) ->
                    Just cd

                _ ->
                    Nothing

        clueId : ClueId
        clueId =
            case cellData of
                Just cd1 ->
                    if cd1.clueId1.direction == direction then
                        cd1.clueId1

                    else
                        case cd1.clueId2 of
                            Just clue ->
                                clue

                            Nothing ->
                                cd1.clueId1

                Nothing ->
                    state.clueId

        newState : State
        newState =
            { state
                | index = nextIndex
                , clueId = clueId
                , direction = clueId.direction
            }
    in
    ( Success { model | state = newState }, focusTextInput )


getCurrentCellChar : Data -> Maybe Char
getCurrentCellChar model =
    let
        cell : Maybe Cell
        cell =
            elementAtIndex (model.state.index + 1) model.crossword.grid
    in
    case cell of
        Just (White cellData) ->
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


getDownWhiteIndex : Data -> Int
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


getUpWhiteIndex : Data -> Int
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


currentColumnNumber : Data -> Int
currentColumnNumber model =
    getColumnNumber model.crossword.numberOfColumns model.state.index


getColumnNumber : Int -> Int -> Int
getColumnNumber numberOfColumns index =
    modBy numberOfColumns index + 1


currentRowNumber : Data -> Int
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
        White _ ->
            True

        Black ->
            False


updateGrid : List Cell -> Int -> Maybe Char -> List Cell
updateGrid grid index newChar =
    grid
        |> List.Extra.updateIfIndex ((==) index)
            (\item ->
                case item of
                    White cellData ->
                        White { cellData | value = newChar }

                    Black ->
                        Black
            )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Crossword"
    , body = [ htmlView model ]
    }


htmlView : Model -> Html Msg
htmlView model =
    div []
        [ case model of
            Failure err ->
                div []
                    [ text (buildErrorMessage err)
                    ]

            Loading ->
                div []
                    [ text "Loading"
                    ]

            Success data ->
                viewPuzzle data
        ]


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


viewPuzzle : Data -> Html Msg
viewPuzzle model =
    div
        [ style "display" "flex"
        ]
        [ viewGridWithInput model
        , viewCluesSection model Across model.crossword.clues.across
        , viewCluesSection model Down model.crossword.clues.down
        ]


viewCluesSection : Data -> Direction -> List Clue -> Html Msg
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


viewClues : Data -> Direction -> List Clue -> Html Msg
viewClues model direction clues =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
        (List.map (viewClueAndDataAndDirection model direction) clues)


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


viewClueAndDataAndDirection : Data -> Direction -> Clue -> Html Msg
viewClueAndDataAndDirection model direction clue =
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


viewGrid : Data -> Html Msg
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
        (List.indexedMap (viewCellAndData model) model.crossword.grid)


viewGridWithInput : Data -> Html Msg
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


onTextInput : Data -> String -> Msg
onTextInput model string =
    Change model.state.index (List.head (List.reverse (String.toList string)))


getGridTemplate : Data -> String
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


shouldHighlight : Data -> CellData -> Bool
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
        White cellData ->
            div
                [ style "position" "relative"
                ]
                [ div
                    [ style "position" "absolute"
                    , style "z-index" "20"
                    ]
                    [ text
                        (case cellData.number of
                            Just number ->
                                String.fromInt number

                            Nothing ->
                                ""
                        )
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


viewCellAndData : Data -> Int -> Cell -> Html Msg
viewCellAndData model index cell =
    let
        highlight : Bool
        highlight =
            case cell of
                Black ->
                    False

                White cellData ->
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
        , messageReceiver mapWorkerUpdated
        ]


decodeModel : Decode.Value -> Result Error WebsocketMessage
decodeModel modelJson =
    Decode.decodeValue decode modelJson


mapWorkerUpdated : Decode.Value -> Msg
mapWorkerUpdated modelJson =
    case decodeModel modelJson of
        Ok model ->
            Recv model

        Err _ ->
            NoOp


type KeyEventMsg
    = KeyEventUnknown
    | TabPressed
    | BackspacePressed
    | ShiftPressed
    | ShiftReleased
    | LeftPressed
    | RightPressed
    | UpPressed
    | DownPressed


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
            DownPressed

        _ ->
            KeyEventUnknown
