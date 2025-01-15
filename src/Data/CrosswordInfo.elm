module Data.CrosswordInfo exposing (CrosswordInfo, decoder, fetch)

import Effect exposing (Effect)
import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (WebData)


type alias CrosswordInfo =
    { id : String
    , series : String
    , seriesNo : Int
    , date : Int
    }


{-| TODO: ideally we wouldn't export this function just to unit test it - can we test the
--| fetch function instead?
-}
decoder : JD.Decoder CrosswordInfo
decoder =
    JD.succeed CrosswordInfo
        |> required "id" JD.string
        |> required "series" JD.string
        |> required "seriesNo" JD.int
        |> required "date" JD.int


fetch : { onResponse : WebData (List CrosswordInfo) -> msg } -> Effect msg
fetch options =
    Effect.sendGetRequest
        { endpoint = "crosswords"
        , onResponse = options.onResponse
        , decoder = JD.list decoder
        }
