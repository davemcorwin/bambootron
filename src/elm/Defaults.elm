module Defaults exposing (..)

import Dict exposing (Dict)
import Utils exposing (SheetLayout, alpha)


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

        colHeaderData =
            List.range 1 numRows
                |> List.map (\idx -> ( ( idx, 1 ), toString idx ))
                |> Dict.fromList

        rowHeaderData =
            List.range 1 numCols
                |> List.map (\idx -> ( ( 1, idx ), alpha idx ))
                |> Dict.fromList
    in
        { numCols = numCols
        , numRows = numRows
        , dfltColWidth = dfltColWidth
        , dfltRowHeight = dfltRowHeight
        , colHeaderColWidth = colHeaderColWidth
        , totalWidth = (dfltColWidth + gridGap) * numCols + colHeaderColWidth
        , totalHeight = (dfltRowHeight + gridGap) * (numRows + gridGap)
        , gridGap = gridGap
        , rowHeaderData = rowHeaderData
        , colHeaderData = colHeaderData
        }
