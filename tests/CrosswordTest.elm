module CrosswordTest exposing (getColumnNumberTests, getRowNumberTests)

import Expect
import Crossword exposing (getColumnNumber, getRowNumber)
import Test exposing (Test, describe, test)


getColumnNumberTests : Test
getColumnNumberTests =
    describe "getColumnNumber"
        [ describe "when number of columns is 1"
            [ describe "and index is 0"
                [ test "column number is 1" <|
                    \_ ->
                        getColumnNumber 1 0
                            |> Expect.equal 1
                ]
            , describe "and index is 1"
                [ test "column number is 1" <|
                    \_ ->
                        getColumnNumber 1 1
                            |> Expect.equal 1
                ]
            , describe "and index is 2"
                [ test "column number is 1" <|
                    \_ ->
                        getColumnNumber 1 2
                            |> Expect.equal 1
                ]
            , describe "and index is 3"
                [ test "column number is 1" <|
                    \_ ->
                        getColumnNumber 1 3
                            |> Expect.equal 1
                ]
            ]
        , describe "when number of columns is 2"
            [ describe "and index is 0"
                [ test "column number is 1" <|
                    \_ ->
                        getColumnNumber 2 0
                            |> Expect.equal 1
                ]
            , describe "and index is 1"
                [ test "column number is 2" <|
                    \_ ->
                        getColumnNumber 2 1
                            |> Expect.equal 2
                ]
            , describe "and index is 2"
                [ test "column number is 1" <|
                    \_ ->
                        getColumnNumber 2 2
                            |> Expect.equal 1
                ]
            , describe "and index is 3"
                [ test "column number is 2" <|
                    \_ ->
                        getColumnNumber 2 3
                            |> Expect.equal 2
                ]
            ]
        , describe "when number of columns is 3"
            [ describe "and index is 0"
                [ test "column number is 1" <|
                    \_ ->
                        getColumnNumber 3 0
                            |> Expect.equal 1
                ]
            , describe "and index is 1"
                [ test "column number is 2" <|
                    \_ ->
                        getColumnNumber 3 1
                            |> Expect.equal 2
                ]
            , describe "and index is 2"
                [ test "column number is 3" <|
                    \_ ->
                        getColumnNumber 3 2
                            |> Expect.equal 3
                ]
            , describe "and index is 3"
                [ test "column number is 1" <|
                    \_ ->
                        getColumnNumber 3 3
                            |> Expect.equal 1
                ]
            ]
        ]


getRowNumberTests : Test
getRowNumberTests =
    describe "getRowNumber"
        [ describe "when number of columns is 1"
            [ describe "and index is 0"
                [ test "row number is 1" <|
                    \_ ->
                        getRowNumber 1 0
                            |> Expect.equal 1
                ]
            , describe "and index is 1"
                [ test "row number is 1" <|
                    \_ ->
                        getRowNumber 1 1
                            |> Expect.equal 2
                ]
            , describe "and index is 2"
                [ test "row number is 1" <|
                    \_ ->
                        getRowNumber 1 2
                            |> Expect.equal 3
                ]
            , describe "and index is 3"
                [ test "row number is 1" <|
                    \_ ->
                        getRowNumber 1 3
                            |> Expect.equal 4
                ]
            ]
        , describe "when number of columns is 2"
            [ describe "and index is 0"
                [ test "row number is 1" <|
                    \_ ->
                        getRowNumber 2 0
                            |> Expect.equal 1
                ]
            , describe "and index is 1"
                [ test "row number is 2" <|
                    \_ ->
                        getRowNumber 2 1
                            |> Expect.equal 1
                ]
            , describe "and index is 2"
                [ test "row number is 1" <|
                    \_ ->
                        getRowNumber 2 2
                            |> Expect.equal 2
                ]
            , describe "and index is 3"
                [ test "row number is 2" <|
                    \_ ->
                        getRowNumber 2 3
                            |> Expect.equal 2
                ]
            ]
        , describe "when number of columns is 3"
            [ describe "and index is 0"
                [ test "row number is 1" <|
                    \_ ->
                        getRowNumber 3 0
                            |> Expect.equal 1
                ]
            , describe "and index is 1"
                [ test "row number is 2" <|
                    \_ ->
                        getRowNumber 3 1
                            |> Expect.equal 1
                ]
            , describe "and index is 2"
                [ test "row number is 3" <|
                    \_ ->
                        getRowNumber 3 2
                            |> Expect.equal 1
                ]
            , describe "and index is 3"
                [ test "row number is 1" <|
                    \_ ->
                        getRowNumber 3 3
                            |> Expect.equal 2
                ]
            ]
        ]
