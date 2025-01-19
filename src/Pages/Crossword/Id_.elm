module Pages.Crossword.Id_ exposing (LoadedModel, Model, Msg, page)

import Data.Cell as Cell exposing (Cell)
import Data.Clue as Clue exposing (Clue)
import Data.Crossword as Crossword exposing (Crossword)
import Data.Direction as Direction exposing (Direction(..))
import Data.Grid as Grid
import Effect exposing (Effect)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id)
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
    }


type alias Model =
    WebData LoadedModel


init : String -> () -> ( Model, Effect Msg )
init id () =
    ( Loading
    , Crossword.fetch { id = id, onResponse = \result -> CrosswordFetched result }
    )



-- UPDATE


type Msg
    = CrosswordFetched (WebData Crossword)


update : Msg -> Model -> ( Model, Effect Msg )
update msg _ =
    case msg of
        CrosswordFetched response ->
            response
                |> RemoteData.map (\crossword -> { crossword = crossword })
                |> (\newModel -> ( newModel, Effect.none ))



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

            Success { crossword } ->
                [ viewCrossword crossword ]
    }


viewCrossword : Crossword -> Html Msg
viewCrossword crossword =
    let
        attributes : List (Html.Attribute Msg)
        attributes =
            [ id "crossword" ]

        children : List (Html Msg)
        children =
            []
                |> Build.add (Grid.view [ id "grid" ] viewCell crossword.grid)
                |> Build.add (viewClues crossword.clues)
    in
    div attributes children


viewCell : Cell -> Html Msg
viewCell cell =
    let
        attributes : List (Html.Attribute Msg)
        attributes =
            [ class "cell"
            , class
                (if Cell.isWhite cell then
                    "white"

                 else
                    "black"
                )
            ]

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
