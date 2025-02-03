module Data.Cell exposing (Cell, decoder, getLetter, getNumber, isWhite, test_newBlack, test_newWhite)

import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)


type Cell
    = Black
    | White (Maybe Int) Char


isWhite : Cell -> Bool
isWhite cell =
    case cell of
        White _ _ ->
            True

        Black ->
            False


getNumber : Cell -> Maybe Int
getNumber cell =
    case cell of
        White number _ ->
            number

        Black ->
            Nothing


getLetter : Cell -> Maybe Char
getLetter cell =
    case cell of
        White _ letter ->
            Just letter

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
                            (\number letterString ->
                                let
                                    letter : Char
                                    letter =
                                        String.toList letterString
                                            |> List.head
                                            |> Maybe.withDefault ' '
                                in
                                White number letter
                            )
                            |> required "number" (JD.maybe JD.int)
                            |> required "letter" JD.string

                    "Black" ->
                        JD.succeed Black

                    _ ->
                        JD.fail "Invalid cell type"
            )



-- TEST HELPERS


test_newWhite : Maybe Int -> Char -> Cell
test_newWhite maybeNumber letter =
    White maybeNumber letter


test_newBlack : Cell
test_newBlack =
    Black
