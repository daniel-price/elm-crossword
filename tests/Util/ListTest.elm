module Util.ListTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Util.List as List


suite : Test
suite =
    describe "List"
        [ describe "getNextItem"
            [ test "should get the next item after the passed in item if it is in the list" <|
                \_ ->
                    let
                        result : String
                        result =
                            List.getNextItem "clueName" [ "clueNumber", "clueName", "clue" ]
                    in
                    Expect.equal result "clue"
            , test "should get the original item if it is the last in the list" <|
                \_ ->
                    let
                        result : String
                        result =
                            List.getNextItem "clue" [ "clueNumber", "clueName", "clue" ]
                    in
                    Expect.equal result "clue"
            ]
        ]
