module Data.Crossword exposing (Crossword, decoder, fetch)

import Data.Cell as Cell exposing (Cell)
import Data.Grid as Grid exposing (Grid)
import Effect exposing (Effect)
import Json.Decode as JD
import RemoteData exposing (WebData)


type alias Crossword =
    { grid : Grid Cell
    }



-- DECODERS


{-| TODO: ideally we wouldn't export this function just to unit test it - can we test the
--| fetch function instead?
-}
decoder : JD.Decoder Crossword
decoder =
    Grid.decoder "cells" Cell.decoder
        |> JD.map (\grid -> { grid = grid })


fetch : { id : String, onResponse : WebData Crossword -> msg } -> Effect msg
fetch options =
    Effect.sendGetRequest
        { endpoint = "crossword/" ++ options.id
        , onResponse = options.onResponse
        , decoder = decoder
        }
