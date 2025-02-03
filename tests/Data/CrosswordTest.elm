module Data.CrosswordTest exposing (suite)

import Data.Cell as Cell
import Data.Clue as Clue exposing (Clue)
import Data.Crossword as Crossword exposing (Crossword, getCurrentClue)
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
         "number":null,
         "letter":"A"
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
                                    [ Cell.test_newWhite Nothing 'A'
                                    , Cell.test_newBlack
                                    ]
                                )
                                [ Clue.test_new 1 Down "Cross-talking Liam's chance to act in Shakespearean troupe (4,11)" [ 15 ]
                                , Clue.test_new 2 Across "Saw the end of Raab â€” appreciate that's uplifting, basically (2,6)" [ 8 ]
                                ]
                            )
                        )
            ]
        , describe "getClueCoordinates"
            [ test "should return coordinates for a clue" <|
                \_ ->
                    let
                        crossword : Crossword
                        crossword =
                            Crossword
                                (Grid.test_new 3
                                    [ Cell.test_newWhite (Just 1) 'A'
                                    , Cell.test_newWhite Nothing 'A'
                                    , Cell.test_newWhite Nothing 'A'
                                    , Cell.test_newBlack
                                    , Cell.test_newWhite (Just 2) 'A'
                                    , Cell.test_newWhite Nothing 'A'
                                    , Cell.test_newWhite Nothing 'A'
                                    , Cell.test_newWhite Nothing 'A'
                                    , Cell.test_newBlack
                                    ]
                                )
                                []

                        result : List Grid.Coordinate
                        result =
                            Crossword.getClueCoordinates ( 1, 1 ) Across crossword
                    in
                    Expect.equal result
                        [ ( 1, 1 ), ( 2, 1 ) ]
            ]
        , let
            grid : Grid.Grid Cell.Cell
            grid =
                Grid.test_new 3
                    [ Cell.test_newWhite (Just 1) 'A'
                    , Cell.test_newWhite Nothing 'A'
                    , Cell.test_newWhite Nothing 'A'
                    , Cell.test_newBlack
                    , Cell.test_newWhite (Just 2) 'A'
                    , Cell.test_newWhite Nothing 'A'
                    , Cell.test_newWhite Nothing 'A'
                    , Cell.test_newWhite Nothing 'A'
                    , Cell.test_newBlack
                    ]
          in
          describe "getNextClueCoordinate"
            [ test "should get next coordinate in across clue" <|
                \_ ->
                    let
                        result : Grid.Coordinate
                        result =
                            Crossword.getNextClueCoordinate ( 1, 1 )
                                Across
                                (Crossword
                                    grid
                                    []
                                )
                    in
                    Expect.equal result
                        ( 2, 1 )
            , test "should get next coordinate in down clue" <|
                \_ ->
                    let
                        result : Grid.Coordinate
                        result =
                            Crossword.getNextClueCoordinate ( 1, 1 )
                                Down
                                (Crossword
                                    grid
                                    []
                                )
                    in
                    Expect.equal result
                        ( 1, 2 )
            ]
        , let
            grid : Grid.Grid Cell.Cell
            grid =
                Grid.test_new 3
                    [ Cell.test_newWhite (Just 1) 'A'
                    , Cell.test_newWhite Nothing 'A'
                    , Cell.test_newWhite Nothing 'A'
                    , Cell.test_newBlack
                    , Cell.test_newWhite (Just 2) 'A'
                    , Cell.test_newWhite Nothing 'A'
                    , Cell.test_newWhite Nothing 'A'
                    , Cell.test_newWhite Nothing 'A'
                    , Cell.test_newBlack
                    ]
          in
          describe "getPreviousClueCoordinate"
            [ test "should get previous coordinate in across clue" <|
                \_ ->
                    let
                        result : Grid.Coordinate
                        result =
                            Crossword.getPreviousClueCoordinate ( 1, 1 )
                                Across
                                (Crossword
                                    grid
                                    []
                                )
                    in
                    Expect.equal result
                        ( 1, 1 )
            , test "should get previous coordinate in down clue" <|
                \_ ->
                    let
                        result : Grid.Coordinate
                        result =
                            Crossword.getPreviousClueCoordinate ( 1, 1 )
                                Down
                                (Crossword
                                    grid
                                    []
                                )
                    in
                    Expect.equal result
                        ( 1, 0 )
            ]
        , describe "getNextWhiteCoordinate"
            [ test "should get next white coordinate down" <|
                \_ ->
                    let
                        result : Grid.Coordinate
                        result =
                            Crossword.getNextWhiteCoordinate ( 1, 1 )
                                Down
                                (Crossword
                                    (Grid.test_new 3
                                        [ Cell.test_newWhite (Just 1) 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newBlack
                                        , Cell.test_newWhite (Just 2) 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newBlack
                                        ]
                                    )
                                    []
                                )
                    in
                    Expect.equal result
                        ( 1, 2 )
            , test "should get next white coordinate accross" <|
                \_ ->
                    let
                        result : Grid.Coordinate
                        result =
                            Crossword.getNextWhiteCoordinate ( 1, 1 )
                                Across
                                (Crossword
                                    (Grid.test_new 3
                                        [ Cell.test_newWhite (Just 1) 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newBlack
                                        , Cell.test_newWhite (Just 2) 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newBlack
                                        ]
                                    )
                                    []
                                )
                    in
                    Expect.equal result
                        ( 2, 1 )
            ]
        , describe "getPreviousWhiteCoordinate"
            [ test "should get previous white coordinate down" <|
                \_ ->
                    let
                        result : Grid.Coordinate
                        result =
                            Crossword.getPreviousClueCoordinate ( 1, 1 )
                                Down
                                (Crossword
                                    (Grid.test_new 3
                                        [ Cell.test_newWhite (Just 1) 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newBlack
                                        , Cell.test_newWhite (Just 2) 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newBlack
                                        ]
                                    )
                                    []
                                )
                    in
                    Expect.equal result
                        ( 1, 0 )
            , test "should get previous white coordinate accross" <|
                \_ ->
                    let
                        result : Grid.Coordinate
                        result =
                            Crossword.getPreviousClueCoordinate ( 1, 1 )
                                Across
                                (Crossword
                                    (Grid.test_new 3
                                        [ Cell.test_newWhite (Just 1) 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newBlack
                                        , Cell.test_newWhite (Just 2) 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newWhite Nothing 'A'
                                        , Cell.test_newBlack
                                        ]
                                    )
                                    []
                                )
                    in
                    Expect.equal result
                        ( 1, 1 )
            ]
        , describe "getCurrentClue"
            [ test "should return the current clue" <|
                \_ ->
                    let
                        crossword : Crossword
                        crossword =
                            Crossword
                                (Grid.test_new 3
                                    [ Cell.test_newWhite (Just 1) 'A'
                                    , Cell.test_newWhite Nothing 'A'
                                    , Cell.test_newWhite Nothing 'A'
                                    , Cell.test_newBlack
                                    , Cell.test_newWhite (Just 2) 'A'
                                    , Cell.test_newWhite Nothing 'A'
                                    , Cell.test_newWhite Nothing 'A'
                                    , Cell.test_newWhite Nothing 'A'
                                    , Cell.test_newBlack
                                    ]
                                )
                                [ Clue.test_new 1 Down "Clue 1" [ 1 ]
                                , Clue.test_new 2 Across "Clue 2" [ 1 ]
                                ]

                        result : Maybe Clue
                        result =
                            getCurrentClue ( 1, 1 ) Across crossword
                    in
                    Expect.equal result
                        (Just (Clue.test_new 2 Across "Clue 2" [ 1 ]))
            ]
        ]
