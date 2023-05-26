module Types exposing (Cell(..), CellData, Clue, ClueId, Clues, Crossword, CrosswordId, Data, Direction(..), Model(..), State, WebsocketMessage)

import Http


type Direction
    = Across
    | Down


type alias ClueId =
    { direction : Direction, number : Int }


type alias Clue =
    { text : String, number : Int }


type alias CellData =
    { clueId1 : ClueId
    , clueId2 : Maybe ClueId
    , value : Maybe Char -- move to state
    , number : Maybe Int
    }


type Cell
    = White CellData
    | Black


type alias WebsocketMessage =
    { x : Int, y : Int, value : String }


type alias Clues =
    { across : List Clue
    , down : List Clue
    }


type alias Crossword =
    { grid : List Cell
    , clues : Clues
    , numberOfColumns : Int
    , numberOfRows : Int
    }


type alias State =
    { clueId : ClueId
    , direction : Direction
    , index : Int
    }


type alias Data =
    { crossword : Crossword
    , state : State
    , shiftHeld : Bool
    , latestString : String
    }


type Model
    = Success Data
    | Loading
    | Failure Http.Error


type alias CrosswordId =
    String
