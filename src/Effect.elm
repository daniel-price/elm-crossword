module Effect exposing
    ( Effect
    , none
    , sendCmd
    , pushRoute, replaceRoute
    , map, toCmd
    , sendGetRequest
    )

{-| This file was generated automatically by running elm-land customize effect
I then fixed the elm-review errors (mostly removing unused functions) - if you need to get these back check out
<https://github.com/elm-land/elm-land/blob/main/projects/cli/src/templates/_elm-land/customizable/Effect.elm>

@docs Effect

@docs none
@docs sendCmd

@docs pushRoute, replaceRoute

@docs map, toCmd

@docs sendGetRequest

-}

import Browser.Navigation
import Dict exposing (Dict)
import Http
import Json.Decode
import RemoteData exposing (WebData)
import Route
import Route.Path
import Shared.Model
import Shared.Msg
import Url exposing (Url)


type Effect msg
    = -- BASICS
      None
    | SendCmd (Cmd msg)
      -- ROUTING
    | PushUrl String
    | ReplaceUrl String
    | SendGetRequest { endpoint : String, decoder : Json.Decode.Decoder msg, onHttpError : Http.Error -> msg }



-- BASICS


{-| Don't send any effect.
-}
none : Effect msg
none =
    None


{-| Send a normal `Cmd msg` as an effect, something like `Http.get` or `Random.generate`.
-}
sendCmd : Cmd msg -> Effect msg
sendCmd =
    SendCmd



-- ROUTING


{-| Set the new route, and make the back button go back to the current route.
-}
pushRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
pushRoute route =
    PushUrl (Route.toString route)


{-| Set the new route, but replace the previous one, so clicking the back
button **won't** go back to the previous route.
-}
replaceRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
replaceRoute route =
    ReplaceUrl (Route.toString route)



-- INTERNALS


{-| Elm Land depends on this function to connect pages and layouts
together into the overall app.
-}
map : (msg1 -> msg2) -> Effect msg1 -> Effect msg2
map fn effect =
    case effect of
        None ->
            None

        SendCmd cmd ->
            SendCmd (Cmd.map fn cmd)

        PushUrl url ->
            PushUrl url

        ReplaceUrl url ->
            ReplaceUrl url

        SendGetRequest data ->
            SendGetRequest
                { endpoint = data.endpoint
                , decoder = Json.Decode.map fn data.decoder
                , onHttpError = data.onHttpError >> fn
                }


{-| Elm Land depends on this function to perform your effects.
-}
toCmd :
    { key : Browser.Navigation.Key
    , url : Url
    , shared : Shared.Model.Model
    , fromSharedMsg : Shared.Msg.Msg -> msg
    , batch : List msg -> msg
    , toCmd : msg -> Cmd msg
    }
    -> Effect msg
    -> Cmd msg
toCmd options effect =
    case effect of
        None ->
            Cmd.none

        SendCmd cmd ->
            cmd

        PushUrl url ->
            Browser.Navigation.pushUrl options.key url

        ReplaceUrl url ->
            Browser.Navigation.replaceUrl options.key url

        SendGetRequest data ->
            Http.request
                { method = "GET"
                , url = options.shared.apiUrl ++ data.endpoint
                , headers = []
                , body = Http.emptyBody
                , expect =
                    Http.expectJson
                        (\httpResult ->
                            case httpResult of
                                Ok msg ->
                                    msg

                                Err httpError ->
                                    data.onHttpError httpError
                        )
                        data.decoder
                , timeout = Just 15000
                , tracker = Nothing
                }


sendGetRequest : { endpoint : String, decoder : Json.Decode.Decoder value, onResponse : WebData value -> msg } -> Effect msg
sendGetRequest options =
    SendGetRequest
        { endpoint = options.endpoint
        , decoder =
            options.decoder
                |> Json.Decode.map Ok
                |> Json.Decode.map (RemoteData.fromResult >> options.onResponse)
        , onHttpError = Err >> RemoteData.fromResult >> options.onResponse
        }
