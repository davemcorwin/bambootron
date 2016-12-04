module Utils exposing (..)

import Dict exposing (Dict)


type alias Data =
    Dict ( Int, Int ) String


type alias Cell =
    { row : Int
    , column : Int
    }


type alias Location =
    { top : Int
    , left : Int
    }


type alias Range =
    { startRow : Int
    , endRow : Int
    , startColumn : Int
    , endColumn : Int
    }


type alias SheetLayout =
    { numCols : Int
    , numRows : Int
    , dfltColWidth : Int
    , dfltRowHeight : Int
    , colHeaderColWidth : Int
    , totalWidth : Int
    , totalHeight : Int
    , gridGap : Int
    , rowHeaderData : Data
    , colHeaderData : Data
    }


alpha : Int -> String
alpha idx =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        |> String.slice (idx - 1) idx


cell2Tuple : Cell -> ( Int, Int )
cell2Tuple cell =
    ( cell.row, cell.column )


tuple2Cell : ( Int, Int ) -> Cell
tuple2Cell tuple =
    Cell (Tuple.first tuple) (Tuple.second tuple)
