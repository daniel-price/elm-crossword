module Data.Direction exposing (Direction(..), decoder, toString)

import Json.Decode as JD


type Direction
    = Across
    | Down


toString : Direction -> String
toString direction =
    case direction of
        Across ->
            "Across"

        Down ->
            "Down"



-- DECODERS


decoder : JD.Decoder Direction
decoder =
    JD.string
        |> JD.andThen
            (\str ->
                case str of
                    "across" ->
                        JD.succeed Across

                    "down" ->
                        JD.succeed Down

                    _ ->
                        JD.fail "Invalid direction"
            )
