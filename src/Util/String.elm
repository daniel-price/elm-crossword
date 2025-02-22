module Util.String exposing (capitalizeFirstLetter)


capitalizeFirstLetter : String -> String
capitalizeFirstLetter string =
    String.uncons string
        |> Maybe.map (\( head, tail ) -> String.cons (Char.toUpper head) tail)
        |> Maybe.withDefault ""
