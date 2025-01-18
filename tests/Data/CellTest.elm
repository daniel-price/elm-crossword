module Data.CellTest exposing (suite)

import Data.Cell as Cell exposing (Cell)
import Expect
import Json.Decode as JD
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Cell"
        [ describe "decode"
            [ test "should decode white cell json" <|
                \_ ->
                    let
                        input : String
                        input =
                            """
{
    "type":"White",
    "number":null
}
"""

                        result : Result JD.Error Cell
                        result =
                            JD.decodeString Cell.decoder input
                    in
                    Expect.equal result (Ok (Cell.test_newWhite Nothing))
            , test "should decode numbered white cell json" <|
                \_ ->
                    let
                        input : String
                        input =
                            """
{
    "type":"White",
    "number":1
}
"""

                        result : Result JD.Error Cell
                        result =
                            JD.decodeString Cell.decoder input
                    in
                    Expect.equal result (Ok (Cell.test_newWhite (Just 1)))
            , test "should decode black cell json" <|
                \_ ->
                    let
                        input : String
                        input =
                            """
{
    "type":"Black"
}
"""

                        result : Result JD.Error Cell
                        result =
                            JD.decodeString Cell.decoder input
                    in
                    Expect.equal result (Ok Cell.test_newBlack)
            ]
        , describe "isWhite"
            [ test "should return true for White cell" <|
                \_ ->
                    Expect.equal (Cell.isWhite (Cell.test_newWhite (Just 1))) True
            , test "should return false for Black cell" <|
                \_ ->
                    Expect.equal (Cell.isWhite Cell.test_newBlack) False
            ]
        ]
