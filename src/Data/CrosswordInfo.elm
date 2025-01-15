module Data.CrosswordInfo exposing (CrosswordInfo, decoder, fetch)

import Effect exposing (Effect)
import Json.Decode as JD
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
    JD.map4 CrosswordInfo
        (JD.field "id" JD.string)
        (JD.field "series" JD.string)
        (JD.field "seriesNo" JD.int)
        (JD.field "date" JD.int)


fetch : { onResponse : WebData (List CrosswordInfo) -> msg } -> Effect msg
fetch options =
    Effect.sendGetRequest
        { endpoint = "crosswords"
        , onResponse = options.onResponse
        , decoder = JD.list decoder
        }
