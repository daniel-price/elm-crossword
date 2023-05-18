module Types exposing (Cell(..), CellData, Clue, ClueId, Clues, Crossword, Direction(..), Model, State)


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
    }


type Cell
    = Item CellData
    | NumberedItem Int CellData
    | Black


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


type alias Model =
    { crossword : Crossword
    , state : State
    , shiftHeld : Bool
    , latestString : String
    }
