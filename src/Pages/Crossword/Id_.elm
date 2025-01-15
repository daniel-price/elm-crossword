module Pages.Crossword.Id_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Task
import View exposing (View)


page : Shared.Model -> Route { id : String } -> Page Model Msg
page _ _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Task.succeed NoOp
        |> Task.perform identity
        |> Effect.sendCmd
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view _ =
    { title = "Pages.Crossword.Id_"
    , body = [ Html.text "/crossword/:id" ]
    }
