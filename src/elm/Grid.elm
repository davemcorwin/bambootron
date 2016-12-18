module Grid exposing (..)

import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import StyleHelper exposing (..)
import Dict exposing (Dict)
import Tuple exposing (first, second)


type alias Grid =
    { colWidth : Int
    , colHeaderWidth : Int
    , rowHeight : Int
    , numRows : Int
    , numCols : Int
    , gridGap : Int
    }


type alias Cell =
    ( Int, Int )


type alias Data =
    Dict Cell String


row : Cell -> Int
row cell =
    first cell


col : Cell -> Int
col cell =
    second cell


foo : Data -> (Cell -> String -> Html msg) -> List (Html msg)
foo data cellMap =
    data
        |> Dict.map cellMap
        |> Dict.values


rowHeader : Grid -> Data -> (Cell -> msg) -> Html msg
rowHeader grid data msg =
    div
        [ class "row-header"
        , styles
            [ cssWidth (totalWidth grid)
            , cssHeight (grid.rowHeight + grid.gridGap)
            , cssTop 0
            , marginLeft (grid.colHeaderWidth + grid.gridGap)
            ]
        ]
        [ div
            [ class "grid"
            , styles
                [ gridTemplateColumns grid.numCols grid.colWidth
                , gridTemplateRows 1 grid.rowHeight
                ]
            ]
            (foo data (\cell value -> headerCell cell value msg))
        ]


colHeader : Grid -> Data -> (Cell -> msg) -> Html msg
colHeader grid data msg =
    div
        [ class "col-header"
        , styles
            [ cssWidth (grid.colWidth + grid.gridGap)
            , cssHeight (totalHeight grid)
            , cssTop (grid.rowHeight + grid.gridGap)
            , marginLeft 0
            ]
        ]
        [ div
            [ class "grid"
            , styles
                [ gridTemplateColumns 1 grid.colHeaderWidth
                , gridTemplateRows grid.numRows grid.rowHeight
                ]
            ]
            (foo data (\cell value -> headerCell cell value msg))
        ]


body : Grid -> List (Html msg) -> Html msg
body grid children =
    div
        [ class "data"
        , styles
            [ cssWidth (totalWidth grid)
            , cssHeight (totalHeight grid)
            , cssTop (grid.rowHeight + grid.gridGap)
            , marginLeft (grid.colHeaderWidth + grid.gridGap)
            ]
        ]
        [ div
            [ class "grid"
            , styles
                [ gridTemplateColumns grid.numCols grid.colWidth
                , gridTemplateRows grid.numRows grid.rowHeight
                ]
            ]
            children
        ]


cornerCell : Grid -> msg -> Html msg
cornerCell grid msg =
    div
        [ class "corner-cell"
        , styles
            [ cssWidth grid.colHeaderWidth
            , cssHeight (grid.rowHeight + grid.gridGap)
            ]
        , onClick msg
        ]
        []


headerCell : Cell -> String -> (Cell -> msg) -> Html msg
headerCell cell value msg =
    div
        [ class "header-cell"
        , styles
            [ gridRow (row cell) (row cell)
            , gridColumn (col cell) (col cell)
            ]
        , onClick (msg cell)
        ]
        [ text value ]


totalWidth : Grid -> Int
totalWidth grid =
    (grid.colWidth + grid.gridGap) * grid.numCols + grid.colHeaderWidth


totalHeight : Grid -> Int
totalHeight grid =
    (grid.rowHeight + grid.gridGap) * (grid.numRows + grid.gridGap)


grid : Grid -> Html.Attribute msg
grid { colWidth, rowHeight, numRows, numCols } =
    styles
        [ gridTemplateColumns numCols colWidth
        , gridTemplateRows numRows rowHeight
        ]
