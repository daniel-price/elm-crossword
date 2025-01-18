module Data.DirectionTest exposing (suite)

import Data.Direction as Direction exposing (Direction(..))
import Expect
import Json.Decode as JD
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Direction"
        [ describe "decode"
            [ test "should decode down" <|
                \_ ->
                    let
                        input : String
                        input =
                            """
"down"
                            """

                        result : Result JD.Error Direction
                        result =
                            JD.decodeString Direction.decoder input
                    in
                    Expect.equal result (Ok Down)
            , test "should decode across" <|
                \_ ->
                    let
                        input : String
                        input =
                            """
"across"
                            """

                        result : Result JD.Error Direction
                        result =
                            JD.decodeString Direction.decoder input
                    in
                    Expect.equal result (Ok Across)
            ]
        , describe "toString"
            [ test "should convert down to string" <|
                \_ ->
                    Expect.equal (Direction.toString Down) "Down"
            , test "should convert across to string" <|
                \_ ->
                    Expect.equal (Direction.toString Across) "Across"
            ]
        ]
