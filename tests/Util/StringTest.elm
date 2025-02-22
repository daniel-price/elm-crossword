module Util.StringTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Util.String as String


suite : Test
suite =
    describe "String"
        [ describe "capitalizeFirstLetter "
            [ test "should capitalize the first letter of the string" <|
                \_ ->
                    let
                        result : String
                        result =
                            String.capitalizeFirstLetter "clue"
                    in
                    Expect.equal result "Clue"
            ]
        ]
