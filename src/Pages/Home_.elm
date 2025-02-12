module Pages.Home_ exposing (LoadedModel, Model, Msg, page)

import Data.CrosswordInfo as CrosswordInfo exposing (CrosswordInfo)
import Effect exposing (Effect)
import Html exposing (a, div, text)
import Html.Attributes exposing (href)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page sharedModel _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view sharedModel.sessionId
        }



-- INIT


type alias LoadedModel =
    { crosswordInfos : List CrosswordInfo
    }


type alias Model =
    WebData LoadedModel


init : () -> ( Model, Effect Msg )
init () =
    ( Loading
    , CrosswordInfo.fetch { onResponse = \result -> CrosswordInfoFetched result }
    )



-- UPDATE


type Msg
    = CrosswordInfoFetched (WebData (List CrosswordInfo))


update : Msg -> Model -> ( Model, Effect Msg )
update msg _ =
    case msg of
        CrosswordInfoFetched response ->
            response
                |> RemoteData.map (\crosswordInfos -> { crosswordInfos = crosswordInfos })
                |> (\newModel -> ( newModel, Effect.none ))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : String -> Model -> View Msg
view sessionId model =
    { title = "Crosswords"
    , body =
        case model of
            NotAsked ->
                [ text "Loading..." ]

            Loading ->
                [ text "Loading..." ]

            Failure _ ->
                [ text "Failed to load crosswords" ]

            Success { crosswordInfos } ->
                crosswordInfos
                    |> splitBySeries
                    |> List.map
                        (\( series, items ) ->
                            div []
                                [ div [] [ text series ]
                                , div [] (viewLinks sessionId items)
                                ]
                        )
    }


viewLinks : String -> List CrosswordInfo -> List (Html.Html Msg)
viewLinks sessionId items =
    items
        |> List.map (viewLink sessionId)


viewLink : String -> CrosswordInfo -> Html.Html Msg
viewLink sessionId item =
    div [] [ a [ href ("/crossword/" ++ item.series ++ "/" ++ String.fromInt item.seriesNo ++ "/" ++ sessionId) ] [ text item.humanDate ] ]


splitBySeries : List CrosswordInfo -> List ( String, List CrosswordInfo )
splitBySeries crosswordInfos =
    crosswordInfos
        |> List.foldl
            (\crosswordInfo acc ->
                let
                    series : String
                    series =
                        crosswordInfo.series
                in
                case List.head acc of
                    Just ( currentSeries, currentCrosswordInfos ) ->
                        if series == currentSeries then
                            ( series, crosswordInfo :: currentCrosswordInfos ) :: Maybe.withDefault [] (List.tail acc)

                        else
                            ( series, [ crosswordInfo ] ) :: acc

                    Nothing ->
                        [ ( series, [ crosswordInfo ] ) ]
            )
            []
        |> List.map (\( series, items ) -> ( series, items |> List.sortBy (\i -> i.date) |> List.reverse ))
