module Example exposing (suite)

import Expect
import Test exposing (Test, test)


suite : Test
suite =
    test "placeholder" <|
        \_ ->
            let
                num : Int
                num =
                    1
            in
            Expect.equal num 1
