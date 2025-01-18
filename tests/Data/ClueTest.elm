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
        ]
