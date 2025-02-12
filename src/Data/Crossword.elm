module Data.Crossword exposing (Crossword, decoder, fetch, getAllWhiteCoordinates, getClueCoordinates, getCurrentClue, getFirstClueCoordinate, getNextClueCoordinate, getNextWhiteCoordinate, getPreviousClueCoordinate, getPreviousWhiteCoordinate)

import Data.Cell as Cell exposing (Cell)
import Data.Clue as Clue exposing (Clue)
import Data.Direction exposing (Direction(..))
import Data.Grid as Grid exposing (Coordinate, Grid)
import Effect exposing (Effect)
import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)
import List.Extra exposing (Step(..))
import RemoteData exposing (WebData)
import Util.List


type alias Crossword =
    { grid : Grid Cell
    , clues : List Clue
    , series : String
    , seriesNo : String
    , date : String
    , setter : String
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


getCurrentClue : Coordinate -> Direction -> Crossword -> Maybe Clue
getCurrentClue coordinate direction crossword =
    crossword
        |> getClueCoordinates coordinate direction
        |> List.head
        |> Maybe.andThen (\c -> Grid.get c crossword.grid)
        |> Maybe.andThen Cell.getNumber
        |> Maybe.andThen
            (\cellNumber ->
                List.Extra.find (\clue -> Clue.getNumber clue == cellNumber && Clue.getDirection clue == direction)
                    crossword.clues
            )


getNextClueCoordinate : Coordinate -> Direction -> Crossword -> Coordinate
getNextClueCoordinate coordinate direction crossword =
    crossword
        |> getClueCoordinates coordinate direction
        |> Util.List.getNextItem False coordinate


getPreviousClueCoordinate : Coordinate -> Direction -> Crossword -> Coordinate
getPreviousClueCoordinate coordinate direction crossword =
    crossword
        |> getClueCoordinates coordinate direction
        |> List.reverse
        |> Util.List.getNextItem False coordinate


getWhiteRowOrColumnCoordinates : Coordinate -> Direction -> Crossword -> List Coordinate
getWhiteRowOrColumnCoordinates coordinate direction crossword =
    crossword.grid
        |> (case direction of
                Across ->
                    Grid.getRowCoordinates coordinate

                Down ->
                    Grid.getColumnCoordinates coordinate
           )
        |> List.filter
            (\coord ->
                Grid.get coord crossword.grid
                    |> Maybe.map Cell.isWhite
                    |> Maybe.withDefault False
            )


getNextWhiteCoordinate : Coordinate -> Direction -> Crossword -> Coordinate
getNextWhiteCoordinate coordinate direction crossword =
    crossword
        |> getWhiteRowOrColumnCoordinates coordinate direction
        |> Util.List.getNextItem True coordinate


getPreviousWhiteCoordinate : Coordinate -> Direction -> Crossword -> Coordinate
getPreviousWhiteCoordinate coordinate direction crossword =
    crossword
        |> getWhiteRowOrColumnCoordinates coordinate direction
        |> List.reverse
        |> Util.List.getNextItem True coordinate


getAllWhiteCoordinates : Crossword -> List Coordinate
getAllWhiteCoordinates crossword =
    crossword.grid
        |> Grid.filterCoordinates Cell.isWhite



-- DECODERS


{-| TODO: ideally we wouldn't export this function just to unit test it - can we test the
--| fetch function instead?
-}
decoder : JD.Decoder Crossword
decoder =
    Grid.decoder "cells" Cell.decoder
        |> JD.map (\grid clues series seriesNo date setter -> { grid = grid, clues = clues, series = series, seriesNo = seriesNo, date = date, setter = setter })
        |> required "clues" (JD.list Clue.decoder)
        |> required "series" JD.string
        |> required "seriesNo" JD.string
        |> required "date" JD.string
        |> required "setter" JD.string


fetch : { series : String, id : String, onResponse : WebData Crossword -> msg } -> Effect msg
fetch options =
    Effect.sendGetRequest
        { endpoint = "crossword/" ++ options.series ++ "/" ++ options.id
        , onResponse = options.onResponse
        , decoder = decoder
        }


getFirstClueCoordinate : Clue -> Crossword -> Maybe Coordinate
getFirstClueCoordinate clue crossword =
    Grid.findCoordinate (\cell -> Cell.getNumber cell == Just (Clue.getNumber clue)) crossword.grid
