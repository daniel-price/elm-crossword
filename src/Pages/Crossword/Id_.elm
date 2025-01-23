module Pages.Crossword.Id_ exposing (LoadedModel, Model, Msg, page)

import Browser.Dom as Dom
import Browser.Events
import Data.Cell as Cell exposing (Cell)
import Data.Clue as Clue exposing (Clue)
import Data.Crossword as Crossword exposing (Crossword)
import Data.Direction as Direction exposing (Direction(..))
import Data.FilledLetters exposing (FilledLetters)
import Data.Grid as Grid exposing (Coordinate)
import Dict
import Effect exposing (Effect)
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (class, id, value)
import Html.Events exposing (on, onClick, targetValue)
import Json.Decode as JD
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Task
import Util.Build as Build
import View exposing (View)


page : Shared.Model -> Route { id : String } -> Page Model Msg
page _ route =
    Page.new
        { init = init route.params.id
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias LoadedModel =
    { crossword : Crossword
    , selectedCoordinate : ( Int, Int )
    , selectedDirection : Direction
    , filledLetters : FilledLetters
    }


type alias Model =
    WebData LoadedModel


init : String -> () -> ( Model, Effect Msg )
init id () =
    ( Loading
    , Crossword.fetch { id = id, onResponse = \result -> CrosswordFetched id result }
    )



-- UPDATE


type ArrowDirection
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown


type Key
    = Unknown
    | Backspace
    | Arrow ArrowDirection


type CrosswordUpdatedMsg
    = CellSelected Coordinate
    | CellLetterAdded Coordinate Char
    | FilledLettersUpdated FilledLetters
    | KeyDown Key


type Msg
    = NoOp
    | CrosswordFetched String (WebData Crossword)
    | CrosswordUpdated CrosswordUpdatedMsg


focusInput : Effect Msg
focusInput =
    Dom.focus "input"
        |> Task.attempt (\_ -> NoOp)
        |> Effect.sendCmd


setEffect : Effect msg -> model -> ( model, Effect msg )
setEffect effect model =
    ( model, effect )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case ( msg, model ) of
        ( CrosswordFetched id response, Loading ) ->
            response
                |> RemoteData.map
                    (\crossword ->
                        let
                            selectedCoordinate : ( Int, Int )
                            selectedCoordinate =
                                crossword.grid
                                    |> Grid.findCoordinate Cell.isWhite
                                    |> Maybe.withDefault ( 0, 0 )

                            selectedDirection : Direction
                            selectedDirection =
                                crossword.grid
                                    |> Grid.get (Tuple.mapFirst ((+) 1) selectedCoordinate)
                                    |> Maybe.andThen
                                        (\cell ->
                                            if Cell.isWhite cell then
                                                Just Direction.Across

                                            else
                                                Nothing
                                        )
                                    |> Maybe.withDefault Direction.Down
                        in
                        { crossword = crossword
                        , selectedCoordinate = selectedCoordinate
                        , selectedDirection = selectedDirection
                        , filledLetters = Dict.empty
                        }
                    )
                |> setEffect (Effect.batch [ Effect.createWebsocket id, focusInput ])

        ( CrosswordUpdated crosswordUpdatedMsg, Success loadedModel ) ->
            updateCrossword crosswordUpdatedMsg loadedModel
                |> Tuple.mapFirst Success

        _ ->
            model |> setEffect Effect.none


updateCrossword : CrosswordUpdatedMsg -> LoadedModel -> ( LoadedModel, Effect Msg )
updateCrossword msg loadedModel =
    case msg of
        CellSelected coordinate ->
            loadedModel
                |> updateCellSelected coordinate
                |> setEffect focusInput

        CellLetterAdded coordinate letter ->
            loadedModel
                |> (loadedModel.crossword
                        |> Crossword.getNextClueCoordinate loadedModel.selectedCoordinate loadedModel.selectedDirection
                        |> setSelectedCoordinate
                   )
                |> (loadedModel.filledLetters
                        |> Dict.insert coordinate letter
                        |> setFilledLetters
                   )
                |> setEffect (Effect.sendWebsocketMessage coordinate letter)

        FilledLettersUpdated filledLetters ->
            loadedModel
                |> setFilledLetters (Dict.union filledLetters loadedModel.filledLetters)
                |> setEffect Effect.none

        KeyDown key ->
            case key of
                Backspace ->
                    case Dict.get loadedModel.selectedCoordinate loadedModel.filledLetters of
                        Just _ ->
                            loadedModel
                                |> setFilledLetters (Dict.remove loadedModel.selectedCoordinate loadedModel.filledLetters)
                                |> setEffect (Effect.sendWebsocketMessage loadedModel.selectedCoordinate ' ')

                        Nothing ->
                            loadedModel
                                |> setSelectedCoordinate
                                    (loadedModel.crossword
                                        |> Crossword.getPreviousClueCoordinate loadedModel.selectedCoordinate loadedModel.selectedDirection
                                    )
                                |> setEffect Effect.none

                Unknown ->
                    loadedModel
                        |> setEffect Effect.none

                Arrow arrowDirection ->
                    loadedModel
                        |> updateCellSelected
                            (case arrowDirection of
                                ArrowLeft ->
                                    Crossword.getPreviousWhiteCoordinate loadedModel.selectedCoordinate Across loadedModel.crossword

                                ArrowRight ->
                                    Crossword.getNextWhiteCoordinate loadedModel.selectedCoordinate Across loadedModel.crossword

                                ArrowUp ->
                                    Crossword.getPreviousWhiteCoordinate loadedModel.selectedCoordinate Down loadedModel.crossword

                                ArrowDown ->
                                    Crossword.getNextWhiteCoordinate loadedModel.selectedCoordinate Down loadedModel.crossword
                            )
                        |> setEffect Effect.none


updateCellSelected : Coordinate -> LoadedModel -> LoadedModel
updateCellSelected coordinate loadedModel =
    let
        isDirection : Direction -> Bool
        isDirection direction =
            loadedModel.crossword
                |> Crossword.getClueCoordinates coordinate direction
                |> List.any (\c -> c /= coordinate)

        isAcross : Bool
        isAcross =
            isDirection Across

        isDown : Bool
        isDown =
            isDirection Down

        updatedDirection : Direction
        updatedDirection =
            if isAcross && isDown then
                if loadedModel.selectedCoordinate == coordinate then
                    case loadedModel.selectedDirection of
                        Across ->
                            Down

                        Down ->
                            Across

                else
                    loadedModel.selectedDirection

            else if isAcross then
                Across

            else
                Down
    in
    loadedModel
        |> setSelectedCoordinate coordinate
        |> setSelectedDirection updatedDirection


setSelectedCoordinate : Coordinate -> LoadedModel -> LoadedModel
setSelectedCoordinate selectedCoordinate model =
    { model | selectedCoordinate = selectedCoordinate }


setSelectedDirection : Direction -> LoadedModel -> LoadedModel
setSelectedDirection selectedDirection model =
    { model | selectedDirection = selectedDirection }


setFilledLetters : FilledLetters -> LoadedModel -> LoadedModel
setFilledLetters filledLetters model =
    { model | filledLetters = filledLetters |> Dict.filter (\_ letter -> letter /= ' ') }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Effect.subscribeToWebsocket (CrosswordUpdated << FilledLettersUpdated) NoOp
        , keyDownSubscription
        ]


keyDownSubscription : Sub Msg
keyDownSubscription =
    let
        keyDownToMsg : String -> Msg
        keyDownToMsg eventKeyString =
            (case eventKeyString of
                "Backspace" ->
                    Backspace

                "ArrowLeft" ->
                    Arrow ArrowLeft

                "ArrowRight" ->
                    Arrow ArrowRight

                "ArrowUp" ->
                    Arrow ArrowUp

                "ArrowDown" ->
                    Arrow ArrowDown

                _ ->
                    Unknown
            )
                |> KeyDown
                |> CrosswordUpdated
    in
    JD.string
        |> JD.field "key"
        |> JD.map keyDownToMsg
        |> Browser.Events.onKeyDown



-- VIEW


view : Model -> View Msg
view model =
    { title = "Crossword"
    , body =
        case model of
            NotAsked ->
                [ text "Loading..." ]

            Loading ->
                [ text "Loading..." ]

            Failure _ ->
                [ text "Failed to load crosswords" ]

            Success loadedModel ->
                [ viewCrossword loadedModel ]
    }


viewCrossword : LoadedModel -> Html Msg
viewCrossword loadedModel =
    let
        { crossword, selectedCoordinate, selectedDirection } =
            loadedModel

        highlightedCoordinates : List Coordinate
        highlightedCoordinates =
            loadedModel.crossword
                |> Crossword.getClueCoordinates selectedCoordinate selectedDirection

        maybeHighlightedClue : Maybe Clue
        maybeHighlightedClue =
            loadedModel.crossword |> Crossword.getCurrentClue selectedCoordinate selectedDirection

        attributes : List (Html.Attribute Msg)
        attributes =
            [ id "crossword" ]

        children : List (Html Msg)
        children =
            []
                |> Build.add (viewInput selectedCoordinate)
                |> Build.add (Grid.view [ id "grid" ] (viewCell highlightedCoordinates loadedModel) crossword.grid)
                |> Build.add (viewClues maybeHighlightedClue crossword.clues)
    in
    div attributes children


{-| Have an input floating on top of the grid so that the user can type.

    We can't just use onInput on the currently selected cell as switching focus
    isn't fast enough to keep up with the user typing fast.

    Don't use Html.Events.onInput as it stops propogation which leads to weird backspace behaviour on mobile

-}
viewInput : Coordinate -> Html Msg
viewInput selectedCoordinate =
    let
        onInput : (String -> msg) -> Attribute msg
        onInput tagger =
            on "input" (JD.map tagger targetValue)
    in
    input
        [ id "input"
        , onInput
            (\string ->
                String.toList string
                    |> List.reverse
                    |> List.head
                    |> Maybe.map
                        (\char ->
                            char
                                |> Char.toUpper
                                |> CellLetterAdded selectedCoordinate
                                |> CrosswordUpdated
                        )
                    |> Maybe.withDefault NoOp
            )
        , value ""
        ]
        []


viewCell : List Coordinate -> LoadedModel -> Coordinate -> Cell -> Html Msg
viewCell highlightedCoordinates loadedModel coordinate cell =
    let
        isWhite : Bool
        isWhite =
            Cell.isWhite cell

        isHighlighted : Bool
        isHighlighted =
            List.member coordinate highlightedCoordinates

        maybeLetter : Maybe String
        maybeLetter =
            Dict.get coordinate loadedModel.filledLetters
                |> Maybe.map String.fromChar

        attributes : List (Html.Attribute Msg)
        attributes =
            [ class "cell"
            , class
                (if isWhite then
                    "white"

                 else
                    "black"
                )
            ]
                |> Build.addIf (coordinate == loadedModel.selectedCoordinate) (class "cell-selected")
                |> Build.addIf isWhite (onClick (CrosswordUpdated (CellSelected coordinate)))
                |> Build.addIf isHighlighted (class "cell-highlighted")

        children : List (Html Msg)
        children =
            []
                |> Build.addMaybeMap viewCellNumber (Cell.getNumber cell)
                |> Build.addMaybeMap text maybeLetter
    in
    div attributes children


viewCellNumber : Int -> Html Msg
viewCellNumber cellNumber =
    let
        attributes : List (Html.Attribute Msg)
        attributes =
            [ class "cell-number" ]

        children : List (Html Msg)
        children =
            [ text (String.fromInt cellNumber) ]
    in
    div attributes children


viewClues : Maybe Clue -> List Clue -> Html Msg
viewClues maybeHighlightedClue clues =
    let
        acrossClues : List Clue
        acrossClues =
            Clue.getDirectionClues Across clues

        downClues : List Clue
        downClues =
            Clue.getDirectionClues Down clues

        attributes : List (Html.Attribute Msg)
        attributes =
            [ id "clues" ]

        children : List (Html Msg)
        children =
            []
                |> Build.add (viewCluesList Across maybeHighlightedClue acrossClues)
                |> Build.add (viewCluesList Down maybeHighlightedClue downClues)
    in
    div attributes children


viewCluesList : Direction -> Maybe Clue -> List Clue -> Html Msg
viewCluesList direction maybeHighlightedClue clues =
    let
        attributes : List (Html.Attribute Msg)
        attributes =
            [ class "clues-list" ]

        children : List (Html Msg)
        children =
            []
                |> Build.add (viewClueTitle direction)
                |> Build.concat (List.map (viewClue maybeHighlightedClue) clues)
    in
    div attributes children


viewClueTitle : Direction -> Html Msg
viewClueTitle direction =
    div
        [ class "clue-title" ]
        [ text (Direction.toString direction) ]


viewClue : Maybe Clue -> Clue -> Html Msg
viewClue maybeHighlightedClue clue =
    let
        attributes : List (Html.Attribute Msg)
        attributes =
            [ class "clue" ]
                |> Build.addIf (maybeHighlightedClue == Just clue) (id "clue-selected")

        children : List (Html Msg)
        children =
            []
                |> Build.add (viewClueNumber (Clue.getClueNumberString clue))
                |> Build.add (text (Clue.getClueText clue))
    in
    div attributes children


viewClueNumber : String -> Html Msg
viewClueNumber clueNumber =
    div
        [ class "clue-number" ]
        [ text clueNumber ]
