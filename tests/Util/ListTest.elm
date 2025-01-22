module Util.ListTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Util.List as List


suite : Test
suite =
    describe "List"
        [ describe "getNextItem"
            [ test "no looping - should get the next item after the passed in item if it is in the list" <|
                \_ ->
                    let
                        result : String
                        result =
                            List.getNextItem False "clueName" [ "clueNumber", "clueName", "clue" ]
                    in
                    Expect.equal result "clue"
            , test "no looping - should get the original item if it is the last in the list" <|
                \_ ->
                    let
                        result : String
                        result =
                            List.getNextItem False "clue" [ "clueNumber", "clueName", "clue" ]
                    in
                    Expect.equal result "clue"
            , test "looping - should get the first item in the list if the passed in item is the last in the list" <|
                \_ ->
                    let
                        result : String
                        result =
                            List.getNextItem True "clue" [ "clueNumber", "clueName", "clue" ]
                    in
                    Expect.equal result "clueNumber"
            , test "looping - should get the next item after the passed in item if it is in the list" <|
                \_ ->
                    let
                        result : String
                        result =
                            List.getNextItem True "clueName" [ "clueNumber", "clueName", "clue" ]
                    in
                    Expect.equal result "clue"
            ]
        ]
