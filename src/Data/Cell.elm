module Data.Cell exposing (Cell, decoder, isWhite, test_newBlack, test_newWhite)

import Json.Decode as JD


type Cell
    = Black
    | White


isWhite : Cell -> Bool
isWhite cell =
    case cell of
        White ->
            True

        Black ->
            False



-- DECODERS


decoder : JD.Decoder Cell
decoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\t ->
                case t of
                    "White" ->
                        JD.succeed White

                    "Black" ->
                        JD.succeed Black

                    _ ->
                        JD.fail "Invalid cell type"
            )



-- TEST HELPERS


test_newWhite : Cell
test_newWhite =
    White


test_newBlack : Cell
test_newBlack =
    Black
