module Data.Cell exposing (Cell, decoder, getNumber, isWhite, test_newBlack, test_newWhite)

import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)


type Cell
    = Black
    | White (Maybe Int)


isWhite : Cell -> Bool
isWhite cell =
    case cell of
        White _ ->
            True

        Black ->
            False


getNumber : Cell -> Maybe Int
getNumber cell =
    case cell of
        White number ->
            number

        Black ->
            Nothing



-- DECODERS


decoder : JD.Decoder Cell
decoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\t ->
                case t of
                    "White" ->
                        JD.succeed
                            (\number -> White number)
                            |> required "number" (JD.maybe JD.int)

                    "Black" ->
                        JD.succeed Black

                    _ ->
                        JD.fail "Invalid cell type"
            )



-- TEST HELPERS


test_newWhite : Maybe Int -> Cell
test_newWhite maybeNumber =
    White maybeNumber


test_newBlack : Cell
test_newBlack =
    Black
