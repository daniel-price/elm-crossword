module Data.Crossword exposing (Crossword, decoder, fetch)

import Data.Cell as Cell exposing (Cell)
import Data.Clue as Clue
import Data.Grid as Grid exposing (Grid)
import Effect exposing (Effect)
import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (WebData)


type alias Crossword =
    { grid : Grid Cell
    , clues : List Clue.Clue
    }



-- DECODERS


{-| TODO: ideally we wouldn't export this function just to unit test it - can we test the
--| fetch function instead?
-}
decoder : JD.Decoder Crossword
decoder =
    Grid.decoder "cells" Cell.decoder
        |> JD.map (\grid clues -> { grid = grid, clues = clues })
        |> required "clues" (JD.list Clue.decoder)


fetch : { id : String, onResponse : WebData Crossword -> msg } -> Effect msg
fetch options =
    Effect.sendGetRequest
        { endpoint = "crossword/" ++ options.id
        , onResponse = options.onResponse
        , decoder = decoder
        }
