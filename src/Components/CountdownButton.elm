module Components.CountdownButton exposing (Config, Model, Msg(..), init, subscriptions, update, view)

import Effect exposing (Effect)
import Html exposing (button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time


type Model
    = Model { buttonPressedForSeconds : Maybe Int }


type alias Config msg =
    { text : String
    , onClick : msg
    , color : String
    }


type Msg parentMsg
    = FirstClick { onClickMsg : parentMsg }
    | SecondClick { onClickMsg : parentMsg }
    | Countdown


init : Model
init =
    Model { buttonPressedForSeconds = Nothing }


update :
    { msg : Msg msg
    , model : Model
    , toParentModel : Model -> model
    }
    -> ( model, Effect msg )
update { msg, model, toParentModel } =
    updateButton model msg
        |> Tuple.mapFirst toParentModel


updateButton : Model -> Msg msg -> ( Model, Effect msg )
updateButton model msg =
    case msg of
        FirstClick data ->
            model
                |> setButtonPressedForSeconds (Just 0)
                |> Effect.set (Effect.sendMsg data.onClickMsg)

        SecondClick data ->
            model
                |> setButtonPressedForSeconds Nothing
                |> Effect.set (Effect.sendMsg data.onClickMsg)

        Countdown ->
            model
                |> setButtonPressedForSeconds (incrementButtonPressed model)
                |> Effect.set Effect.none


incrementButtonPressed : Model -> Maybe Int
incrementButtonPressed (Model model) =
    model.buttonPressedForSeconds
        |> Maybe.withDefault 0
        |> (+) 1
        |> (\x ->
                if x >= countdownSeconds then
                    Nothing

                else
                    Just x
           )


setButtonPressedForSeconds : Maybe Int -> Model -> Model
setButtonPressedForSeconds buttonPressedForSeconds (Model model) =
    Model { model | buttonPressedForSeconds = buttonPressedForSeconds }


view :
    { model : Model
    , initial : Config parentMsg
    , clicked : Config parentMsg
    , toParentMsg : Msg parentMsg -> parentMsg
    , additionalAttributes : List (Html.Attribute parentMsg)
    }
    -> Html.Html parentMsg
view settings =
    let
        (Model model) =
            settings.model

        { initial, clicked, toParentMsg, additionalAttributes } =
            settings
    in
    case model.buttonPressedForSeconds of
        Nothing ->
            button
                ([ onClick (FirstClick { onClickMsg = initial.onClick } |> toParentMsg)
                 , style "background" initial.color
                 ]
                    ++ additionalAttributes
                )
                [ text initial.text
                ]

        Just _ ->
            button
                ([ onClick (SecondClick { onClickMsg = clicked.onClick } |> toParentMsg)
                 , style "background" ("linear-gradient(to right, " ++ clicked.color ++ " 50%, " ++ initial.color ++ " 50%)")
                 , style "background-size" "200% 100%"
                 , style "background-position" "left bottom"
                 , style "animation" (String.fromInt countdownSeconds ++ "s linear 0s 1 slideInFromRight")
                 ]
                    ++ additionalAttributes
                )
                [ text clicked.text ]


subscriptions : Model -> (Msg msg -> msg) -> Sub msg
subscriptions (Model { buttonPressedForSeconds }) toMsg =
    case buttonPressedForSeconds of
        Just _ ->
            Sub.map toMsg (Time.every 1000 (\_ -> Countdown))

        Nothing ->
            Sub.none


countdownSeconds : Int
countdownSeconds =
    3
