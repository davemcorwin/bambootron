module Utils exposing (..)


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
    }


type Direction
    = Left
    | Right
    | Up
    | Down


alpha : Int -> String
alpha idx =
    "ZABCDEFGHIJKLMNOPQRSTUVWXY"
        |> String.slice idx (idx + 1)


toBaseAlpha : Int -> String
toBaseAlpha idx =
    let
        log =
            logBase 26.0 (toFloat idx)

        divisor =
            (//) idx 26

        remainder =
            rem idx 26
    in
        if log > 1.0 then
            (toBaseAlpha divisor) ++ (alpha remainder)
        else
            (alpha remainder)
