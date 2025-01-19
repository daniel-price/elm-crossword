module Pages.Crossword.Id_ exposing (LoadedModel, Model, Msg, page)

import Data.Cell as Cell exposing (Cell)
import Data.Clue as Clue exposing (Clue)
import Data.Crossword as Crossword exposing (Crossword)
import Data.Direction as Direction exposing (Direction(..))
import Data.Grid as Grid exposing (Coordinate)
import Effect exposing (Effect)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
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
    }


type alias Model =
    WebData LoadedModel


init : String -> () -> ( Model, Effect Msg )
init id () =
    ( Loading
    , Crossword.fetch { id = id, onResponse = \result -> CrosswordFetched result }
    )



-- UPDATE


type CrosswordUpdatedMsg
    = CellSelected Coordinate


type Msg
    = CrosswordFetched (WebData Crossword)
    | CrosswordUpdated CrosswordUpdatedMsg


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case ( msg, model ) of
        ( CrosswordFetched response, Loading ) ->
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
                        }
                    )
                |> noEffect

        ( CrosswordUpdated crosswordUpdatedMsg, Success loadedModel ) ->
            updateCrossword crosswordUpdatedMsg loadedModel
                |> Tuple.mapFirst Success

        _ ->
            model |> noEffect


updateCrossword : CrosswordUpdatedMsg -> LoadedModel -> ( LoadedModel, Effect Msg )
updateCrossword msg loadedModel =
    case msg of
        CellSelected coordinate ->
            loadedModel
                |> setSelectedCoordinate coordinate
                |> noEffect


noEffect : a -> ( a, Effect b )
noEffect a =
    ( a, Effect.none )


setSelectedCoordinate : Coordinate -> LoadedModel -> LoadedModel
setSelectedCoordinate selectedCoordinate model =
    { model | selectedCoordinate = selectedCoordinate }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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
        { crossword } =
            loadedModel

        attributes : List (Html.Attribute Msg)
        attributes =
            [ id "crossword" ]

        children : List (Html Msg)
        children =
            []
                |> Build.add (Grid.view [ id "grid" ] (viewCell loadedModel) crossword.grid)
                |> Build.add (viewClues crossword.clues)
    in
    div attributes children


viewCell : LoadedModel -> Coordinate -> Cell -> Html Msg
viewCell loadedModel coordinate cell =
    let
        isWhite : Bool
        isWhite =
            Cell.isWhite cell

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

        children : List (Html Msg)
        children =
            []
                |> Build.addMaybeMap viewCellNumber (Cell.getNumber cell)
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


viewClues : List Clue -> Html Msg
viewClues clues =
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
                |> Build.add (viewCluesList Across acrossClues)
                |> Build.add (viewCluesList Down downClues)
    in
    div attributes children


viewCluesList : Direction -> List Clue -> Html Msg
viewCluesList direction clues =
    let
        attributes : List (Html.Attribute Msg)
        attributes =
            [ class "clues-list" ]

        children : List (Html Msg)
        children =
            []
                |> Build.add (viewClueTitle direction)
                |> Build.concat (List.map viewClue clues)
    in
    div attributes children


viewClueTitle : Direction -> Html Msg
viewClueTitle direction =
    div
        [ class "clue-title" ]
        [ text (Direction.toString direction) ]


viewClue : Clue -> Html Msg
viewClue clue =
    let
        attributes : List (Html.Attribute Msg)
        attributes =
            [ class "clue" ]

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
