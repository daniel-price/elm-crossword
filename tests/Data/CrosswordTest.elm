module Data.CrosswordTest exposing (suite)

import Data.Cell as Cell
import Data.Clue as Clue
import Data.Crossword as Crossword exposing (Crossword)
import Data.Direction exposing (Direction(..))
import Data.Grid as Grid
import Expect
import Json.Decode as JD
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Crossword"
        [ describe "decoder"
            [ test "should decode json" <|
                \_ ->
                    let
                        input : String
                        input =
                            """
{
    "numberOfRows":1,
    "cells":[
      {
         "type":"White",
         "number":null
      },
      {
         "type":"Black"
      }
    ],
    "clues":[
      {
         "number":1,
         "text":"Cross-talking Liam's chance to act in Shakespearean troupe (4,11)",
         "direction":"down",
         "length":[
            15
         ]
      },
      {
         "number":2,
         "text":"Saw the end of Raab â€” appreciate that's uplifting, basically (2,6)",
         "direction":"across",
         "length":[
            8
         ]
      }
    ]
}
"""

                        result : Result JD.Error Crossword
                        result =
                            JD.decodeString Crossword.decoder input
                    in
                    Expect.equal result
                        (Ok
                            (Crossword
                                (Grid.test_new 1
                                    [ Cell.test_newWhite Nothing
                                    , Cell.test_newBlack
                                    ]
                                )
                                [ Clue.test_new 1 Down "Cross-talking Liam's chance to act in Shakespearean troupe (4,11)" [ 15 ]
                                , Clue.test_new 2 Across "Saw the end of Raab â€” appreciate that's uplifting, basically (2,6)" [ 8 ]
                                ]
                            )
                        )
            ]
        ]
