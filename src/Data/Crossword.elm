module Data.Crossword exposing (Crossword, decoder, fetch, getClueCoordinates)

import Data.Cell as Cell exposing (Cell)
import Data.Clue as Clue
import Data.Direction exposing (Direction(..))
import Data.Grid as Grid exposing (Coordinate, Grid)
import Effect exposing (Effect)
import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)
import List.Extra exposing (Step(..))
import RemoteData exposing (WebData)


type alias Crossword =
    { grid : Grid Cell
    , clues : List Clue.Clue
    }


getClueCoordinates : Coordinate -> Direction -> Crossword -> List Coordinate
getClueCoordinates coordinate direction crossword =
    let
        rowOrColumnCoordinates : List Coordinate
        rowOrColumnCoordinates =
            case direction of
                Across ->
                    Grid.getRowCoordinates
                        coordinate
                        crossword.grid

                Down ->
                    Grid.getColumnCoordinates
                        coordinate
                        crossword.grid
    in
    rowOrColumnCoordinates
        |> List.Extra.stoppableFoldl
            (\coord acc ->
                case Grid.get coord crossword.grid of
                    Just cell ->
                        if Cell.isWhite cell then
                            Continue (acc ++ [ coord ])

                        else if List.member coordinate acc then
                            Stop acc

                        else
                            Continue []

                    Nothing ->
                        Stop acc
            )
            []



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
