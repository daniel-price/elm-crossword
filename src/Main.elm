module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Crossword
import CrosswordsList
import Html exposing (Html, h3, text)
import Url exposing (Url)
import Url.Parser as Parser exposing (..)



-- MAIN

main : Program () Model Msg
main =
    Browser.application { init = init
                    , update = update
                    , view = view
                    , subscriptions = subscriptions
                    , onUrlRequest = LinkClicked
                    , onUrlChange = UrlChanged }



-- MODEL
type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    }


type Page
    = NotFoundPage
    | CrosswordPage Crossword.Model
    | CrosswordsListPage CrosswordsList.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        model =
            { route = parseUrl url,
              page = CrosswordsListPage { crosswords = [] }
            , navKey = navKey
            }
    in
    initCurrentPage ( model, Cmd.none )

initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                CrosswordsRoute ->
                    let
                        ( pageModel, pageCmds ) =
                            CrosswordsList.init
                    in
                    ( CrosswordsListPage pageModel, Cmd.map CrosswordsListMsg pageCmds )
                CrosswordRoute id ->
                    let
                        ( pageModel, pageCmds ) =
                            Crossword.init id
                    in
                    ( CrosswordPage pageModel, Cmd.map CrosswordMsg pageCmds )
                NotFoundRoute ->
                    ( NotFoundPage, Cmd.none )

    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )

-- UPDATE

type Msg
    = CrosswordsListMsg CrosswordsList.Msg
        | CrosswordMsg Crossword.Msg
        | LinkClicked UrlRequest
        | UrlChanged Url

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( CrosswordsListMsg subMsg, CrosswordsListPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    CrosswordsList.update subMsg pageModel
            in
            ( { model | page = CrosswordsListPage updatedPageModel }
            , Cmd.map CrosswordsListMsg updatedCmd
            )
        ( CrosswordMsg subMsg, CrosswordPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    Crossword.update subMsg pageModel
            in
            ( { model | page = CrosswordPage updatedPageModel }
            , Cmd.map CrosswordMsg updatedCmd
            )
        ( LinkClicked urlRequest, _ ) ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( model
                            , Nav.pushUrl model.navKey (Url.toString url)
                            )
                        Browser.External url ->
                            ( model
                            , Nav.load url
                            )
        ( UrlChanged url, _ ) ->
                    let
                        newRoute =
                            parseUrl url
                    in
                    ( { model | route = newRoute }, Cmd.none )
                        |> initCurrentPage
        ( _, _ ) ->
            ( model, Cmd.none )


-- VIEW

view : Model -> Document Msg
view model =
    { title = title model
    , body = [ htmlView model ]
    }

title : Model -> String
title model =
    case model.page of
        NotFoundPage -> "Not found"
        CrosswordPage _ -> "Crossword"
        CrosswordsListPage _ -> "Crosswords"

htmlView : Model -> Html Msg
htmlView model =
    case model.page of
        NotFoundPage ->
            notFoundView
        CrosswordPage pageModel ->
                Crossword.htmlView pageModel
                    |> Html.map CrosswordMsg
        CrosswordsListPage pageModel ->
                CrosswordsList.view pageModel
                    |> Html.map CrosswordsListMsg


notFoundView : Html msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found!" ]

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        NotFoundPage ->
            Sub.none
        CrosswordPage pageModel ->
            Sub.map (\a -> CrosswordMsg a) (Crossword.subscriptions pageModel)
        CrosswordsListPage _ ->
            Sub.none

--Routing

type Route
    = CrosswordsRoute
    | CrosswordRoute Crossword.CrosswordId
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    Parser.oneOf
        [ Parser.map CrosswordsRoute top
        , Parser.map CrosswordRoute (s "crossword" </> string)
        ]


parseUrl : Url -> Route
parseUrl url =
    case parse matchers url of
        Just route ->
            route
        Nothing ->
            NotFoundRoute
