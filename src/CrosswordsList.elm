module CrosswordsList exposing (CrosswordMetadata, Model, Msg(..), init, update, view)

import Constants exposing (apiUrlCrosswords)
import Html exposing (Html, a, div, li, text)
import Html.Attributes
import Http
import Json.Decode as Decode exposing (Decoder, field, int, map3, string)
import List
import Types exposing (CrosswordId)



-- MODEL


type alias Model =
    { crosswords : List CrosswordMetadata }


type alias CrosswordMetadata =
    { id : CrosswordId
    , series : String
    , seriesNo : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { crosswords = [ { id = "test", series = "cryptic", seriesNo = 123 } ]
      }
    , getCrosswordList
    )



-- UPDATE


type Msg
    = GetCrosswordsList (Result Http.Error (List CrosswordMetadata))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetCrosswordsList result ->
            case result of
                Ok crosswords ->
                    ( { crosswords = crosswords }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        (List.map viewCrosswordLink model.crosswords)


viewCrosswordLink : CrosswordMetadata -> Html Msg
viewCrosswordLink crossword =
    li
        [ Html.Attributes.style "display" "flex" ]
        [ text crossword.series
        , text (String.fromInt crossword.seriesNo)
        , viewLink ("/crossword/" ++ crossword.id)
        ]


viewLink : String -> Html msg
viewLink path =
    div [] [ a [ Html.Attributes.href path ] [ text path ] ]



-- HTTP


getCrosswordList : Cmd Msg
getCrosswordList =
    Http.get
        { url = apiUrlCrosswords
        , expect = Http.expectJson GetCrosswordsList (Decode.list crosswordMetadataDecoder)
        }


crosswordMetadataDecoder : Decoder CrosswordMetadata
crosswordMetadataDecoder =
    map3 CrosswordMetadata
        (field "id" string)
        (field "series" string)
        (field "seriesNo" int)
