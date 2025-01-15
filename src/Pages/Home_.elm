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
page _ _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
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


view : Model -> View Msg
view model =
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
                    |> List.map
                        (\item ->
                            div [] [ a [ href ("/crossword/" ++ item.id) ] [ text item.id ] ]
                        )
    }
