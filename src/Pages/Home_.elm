module Pages.Home_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html
import Json.Decode
import Page exposing (Page)
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


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.sendGetRequest { endpoint = "/api/home", decoder = Json.Decode.succeed NoOp, onResponse = \_ -> NoOp }
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view _ =
    { title = "Homepage"
    , body = [ Html.text "Hello, world!" ]
    }
