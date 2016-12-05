module Defaults exposing (..)

import Utils exposing (SheetLayout)


defaults : SheetLayout
defaults =
    let
        numCols =
            26

        numRows =
            100

        dfltColWidth =
            100

        dfltRowHeight =
            35

        colHeaderColWidth =
            51

        gridGap =
            1
    in
        { numCols = numCols
        , numRows = numRows
        , dfltColWidth = dfltColWidth
        , dfltRowHeight = dfltRowHeight
        , colHeaderColWidth = colHeaderColWidth
        , totalWidth = (dfltColWidth + gridGap) * numCols + colHeaderColWidth
        , totalHeight = (dfltRowHeight + gridGap) * (numRows + gridGap)
        , gridGap = gridGap
        }
