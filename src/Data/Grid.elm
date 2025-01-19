module Data.Grid exposing (Coordinate, Grid, decoder, findCoordinate, get, test_new, view)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)
import List.Extra


type Grid a
    = Grid { numberOfRows : Int, items : List a }


type alias Coordinate =
    ( Int, Int )


getIndexCoordinates : Grid a -> Int -> Coordinate
getIndexCoordinates (Grid { numberOfRows }) index =
    ( remainderBy numberOfRows index, index // numberOfRows )


findCoordinate : (a -> Bool) -> Grid a -> Maybe Coordinate
findCoordinate predicate grid =
    let
        (Grid { items }) =
            grid
    in
    List.Extra.findIndex predicate items |> Maybe.map (getIndexCoordinates grid)


get : Coordinate -> Grid a -> Maybe a
get ( x, y ) (Grid { numberOfRows, items }) =
    let
        index : Int
        index =
            y * numberOfRows + x
    in
    List.Extra.getAt index items



-- DECODERS


decoder : String -> JD.Decoder a -> JD.Decoder (Grid a)
decoder field itemDecoder =
    JD.succeed (\numberOfRows items -> Grid { numberOfRows = numberOfRows, items = items })
        |> required "numberOfRows" JD.int
        |> required field (JD.list itemDecoder)



-- VIEW


view : List (Html.Attribute msg) -> (Coordinate -> a -> Html msg) -> Grid a -> Html msg
view additionalAttributes viewItem grid =
    let
        (Grid { numberOfRows, items }) =
            grid

        attributes : List (Html.Attribute msg)
        attributes =
            additionalAttributes
                ++ [ style "display" "grid"
                   , style "grid-template-columns" ("repeat(" ++ String.fromInt numberOfRows ++ ", 1fr)")
                   ]

        children : List (Html msg)
        children =
            List.indexedMap
                (\i ->
                    let
                        coordinate : Coordinate
                        coordinate =
                            getIndexCoordinates (Grid { numberOfRows = numberOfRows, items = items }) i
                    in
                    viewItem coordinate
                )
                items
    in
    div attributes children



-- TEST HELPERS


test_new : Int -> List a -> Grid a
test_new numberOfRows items =
    Grid { numberOfRows = numberOfRows, items = items }
