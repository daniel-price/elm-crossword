module Data.CrosswordTest exposing (suite)

import Data.Cell as Cell
import Data.Crossword as Crossword exposing (Crossword)
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
                            )
                        )
            ]
        ]
