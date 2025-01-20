module Util.ListTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Util.List as List


suite : Test
suite =
    describe "List"
        [ describe "dropUntilMember"
            [ test "should remove items up to and including the member if member is found" <|
                \_ ->
                    let
                        result : List String
                        result =
                            List.dropUntilMember "clueName" [ "clueNumber", "clueName", "clue" ]
                    in
                    Expect.equal result [ "clue" ]
            , test "should not remove any items if the member is not found" <|
                \_ ->
                    let
                        result : List String
                        result =
                            List.dropUntilMember "clue" [ "clueNumber", "clueName" ]
                    in
                    Expect.equal result [ "clueNumber", "clueName" ]
            ]
        ]
