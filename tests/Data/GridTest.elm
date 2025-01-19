module Data.GridTest exposing (suite)

import Data.Grid as Grid exposing (Grid)
import Expect
import Json.Decode as JD
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Grid"
        [ describe "decoder"
            [ test "should decode json" <|
                \_ ->
                    let
                        input : String
                        input =
                            """
{
    "numberOfRows":2,
    "fieldName":[1,2]
}
"""

                        result : Result JD.Error (Grid Int)
                        result =
                            JD.decodeString (Grid.decoder "fieldName" JD.int) input
                    in
                    Expect.equal result
                        (Ok
                            (Grid.test_new 2
                                [ 1
                                , 2
                                ]
                            )
                        )
            ]
        , describe "findCoordinate"
            [ test "should find first coordinate matching predicate" <|
                \_ ->
                    let
                        grid : Grid Int
                        grid =
                            Grid.test_new 2
                                [ 1
                                , 1
                                , 2
                                , 2
                                , 3
                                , 3
                                ]

                        result : Maybe Grid.Coordinate
                        result =
                            Grid.findCoordinate ((==) 2) grid
                    in
                    Expect.equal result (Just ( 0, 1 ))
            ]
        ]
