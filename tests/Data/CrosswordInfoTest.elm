module Data.CrosswordInfoTest exposing (suite)

import Data.CrosswordInfo as CrosswordInfo
import Expect
import Json.Decode as JD
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "CrosswordInfo"
        [ describe "decoder"
            [ test "should decode json" <|
                \_ ->
                    let
                        input : String
                        input =
                            """
{
   "id":"b854d0a4-cea1-4a6e-b655-84247001958d",
   "series":"cryptic",
   "seriesNo":29128,
   "date":1689897600000
}
"""

                        result : Result JD.Error CrosswordInfo.CrosswordInfo
                        result =
                            JD.decodeString CrosswordInfo.decoder input
                    in
                    Expect.equal result
                        (Ok
                            { id = "b854d0a4-cea1-4a6e-b655-84247001958d"
                            , series = "cryptic"
                            , seriesNo = 29128
                            , date = 1689897600000
                            }
                        )
            ]
        ]
