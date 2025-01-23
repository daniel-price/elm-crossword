module Data.ClueTest exposing (suite)

import Data.Clue as Clue exposing (Clue)
import Data.Direction exposing (Direction(..))
import Expect
import Json.Decode as JD
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Clue"
        [ describe "decode"
            [ test "should decode json" <|
                \_ ->
                    let
                        input : String
                        input =
                            """
{
    "number":1,
    "text":"Cross-talking Liam's chance to act in Shakespearean troupe (4,11)",
    "direction":"down",
    "length":[
       15
    ]
}
"""

                        result : Result JD.Error Clue
                        result =
                            JD.decodeString Clue.decoder input
                    in
                    Expect.equal result (Ok (Clue.test_new 1 Down "Cross-talking Liam's chance to act in Shakespearean troupe (4,11)" [ 15 ]))
            ]
        , describe
            "getClueNumber"
            [ test "should return the clue number" <|
                \_ ->
                    let
                        clue : Clue
                        clue =
                            Clue.test_new 1 Down "Cross-talking Liam's chance to act in Shakespearean troupe (4,11)" [ 15 ]
                    in
                    Expect.equal (Clue.getClueNumber clue) 1
            ]
        , describe
            "getClueNumberString"
            [ test "should return the clue number as a string" <|
                \_ ->
                    let
                        clue : Clue
                        clue =
                            Clue.test_new 1 Down "Cross-talking Liam's chance to act in Shakespearean troupe (4,11)" [ 15 ]
                    in
                    Expect.equal (Clue.getClueNumberString clue) "1"
            ]
        , describe
            "getClueText"
            [ test "should return the clue text" <|
                \_ ->
                    let
                        clue : Clue
                        clue =
                            Clue.test_new 1 Down "Cross-talking Liam's chance to act in Shakespearean troupe (4,11)" [ 15 ]
                    in
                    Expect.equal (Clue.getClueText clue) "Cross-talking Liam's chance to act in Shakespearean troupe (4,11)"
            ]
        , describe
            "getDirection"
            [ test "should return the clue direction" <|
                \_ ->
                    let
                        clue : Clue
                        clue =
                            Clue.test_new 1 Down "Cross-talking Liam's chance to act in Shakespearean troupe (4,11)" [ 15 ]
                    in
                    Expect.equal (Clue.getDirection clue) Down
            ]
        ]
