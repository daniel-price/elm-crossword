module Data.Grid exposing (Grid, decoder, test_new, view)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)


type Grid a
    = Grid { numberOfRows : Int, items : List a }



-- DECODERS


decoder : String -> JD.Decoder a -> JD.Decoder (Grid a)
decoder field itemDecoder =
    JD.succeed (\numberOfRows items -> Grid { numberOfRows = numberOfRows, items = items })
        |> required "numberOfRows" JD.int
        |> required field (JD.list itemDecoder)



-- VIEW


view : List (Html.Attribute msg) -> (a -> Html msg) -> Grid a -> Html msg
view additionalAttributes viewItem (Grid { numberOfRows, items }) =
    let
        attributes : List (Html.Attribute msg)
        attributes =
            additionalAttributes
                ++ [ style "display" "grid"
                   , style "grid-template-columns" ("repeat(" ++ String.fromInt numberOfRows ++ ", 1fr)")
                   ]

        children : List (Html msg)
        children =
            List.map viewItem items
    in
    div attributes children



-- TEST HELPERS


test_new : Int -> List a -> Grid a
test_new numberOfRows items =
    Grid { numberOfRows = numberOfRows, items = items }
