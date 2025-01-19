module Util.BuildTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Util.Build as Build


suite : Test
suite =
    describe "Build"
        [ describe "add"
            [ test "should add the value to the end of the list" <|
                \_ ->
                    let
                        result : List String
                        result =
                            Build.add "clue" [ "clueNumber", "clueName" ]
                    in
                    Expect.equal result [ "clueNumber", "clueName", "clue" ]
            ]
        , describe "addMaybe"
            [ test "should add the mapped value to the end of the list if it is a Just value" <|
                \_ ->
                    let
                        result : List String
                        result =
                            Build.addMaybeMap (\a -> "not " ++ a) (Just "clue") [ "clueNumber", "clueName" ]
                    in
                    Expect.equal result [ "clueNumber", "clueName", "not clue" ]
            , test "should not add the mapped value to the end of the list if the value is Nothing" <|
                \_ ->
                    let
                        result : List String
                        result =
                            Build.addMaybeMap (\a -> "not " ++ a) Nothing [ "clueNumber", "clueName" ]
                    in
                    Expect.equal result [ "clueNumber", "clueName" ]
            ]
        , describe "concat"
            [ test "should concatenate the second list to the first list" <|
                \_ ->
                    let
                        result : List String
                        result =
                            [ "clueNumber", "clueName" ]
                                |> Build.concat [ "clue" ]
                    in
                    Expect.equal result [ "clueNumber", "clueName", "clue" ]
            ]
        , describe "addIf"
            [ test "should add the value to the end of the list if the condition is true" <|
                \_ ->
                    let
                        result : List String
                        result =
                            [ "clueNumber", "clueName" ] |> Build.addIf True "clue"
                    in
                    Expect.equal result [ "clueNumber", "clueName", "clue" ]
            , test "should not add the value to the end of the list if the condition is false" <|
                \_ ->
                    let
                        result : List String
                        result =
                            [ "clueNumber", "clueName" ] |> Build.addIf False "clue"
                    in
                    Expect.equal result [ "clueNumber", "clueName" ]
            ]
        ]
