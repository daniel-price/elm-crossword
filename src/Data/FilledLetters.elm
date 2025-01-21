module Data.FilledLetters exposing (FilledLetters)

import Data.Grid exposing (Coordinate)
import Dict exposing (Dict)


type alias FilledLetters =
    Dict Coordinate Char
