module Pages.Crossword.Id_ exposing (LoadedModel, Model, Msg, page)

import Data.Cell as Cell exposing (Cell)
import Data.Crossword as Crossword exposing (Crossword)
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
    { title = "Pages.Crossword.Id_"
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
            [ Grid.view [ id "class" ] viewCell crossword.grid ]
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
