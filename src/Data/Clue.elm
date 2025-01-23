module Data.Clue exposing (Clue, decoder, getDirection, getDirectionClues, getNumber, getNumberString, getText, test_new)

import Data.Direction as Direction exposing (Direction)
import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)


type Clue
    = Clue
        { id : Int
        , direction : Direction
        , text : String
        , length : List Int
        }


getDirectionClues : Direction -> List Clue -> List Clue
getDirectionClues cluesDirection clues =
    clues
        |> List.filter (\(Clue { direction }) -> direction == cluesDirection)
        |> List.sortBy (\(Clue { id }) -> id)


getNumberString : Clue -> String
getNumberString (Clue { id }) =
    String.fromInt id


getNumber : Clue -> Int
getNumber (Clue { id }) =
    id


getText : Clue -> String
getText (Clue { text }) =
    text


getDirection : Clue -> Direction
getDirection (Clue { direction }) =
    direction



-- DECODERS


decoder : JD.Decoder Clue
decoder =
    JD.succeed (\id direction text length -> Clue { id = id, direction = direction, text = text, length = length })
        |> required "number" JD.int
        |> required "direction" Direction.decoder
        |> required "text" JD.string
        |> required "length" (JD.list JD.int)



-- TEST HELPERS


test_new : Int -> Direction -> String -> List Int -> Clue
test_new id direction text length =
    Clue
        { id = id
        , direction = direction
        , text = text
        , length = length
        }
