module Cell exposing (..)

import Basics as B
import Tuple exposing (first, second, mapFirst, mapSecond)


type alias Cell =
    ( Int, Int )


new : Int -> Int -> Cell
new row col =
    ( row, col )


equal : Cell -> Cell -> Bool
equal cell1 cell2 =
    ((first cell1) == (first cell2)) && ((second cell1) == (second cell2))


min : Cell -> Cell -> Cell
min cell1 cell2 =
    if (row cell1) < (row cell2) then
        cell1
    else if (row cell2) < (row cell1) then
        cell2
    else if (col cell1) < (col cell2) then
        cell1
    else if (col cell2) < (col cell1) then
        cell2
    else
        cell1


max : Cell -> Cell -> Cell
max cell1 cell2 =
    if (row cell1) < (row cell2) then
        cell2
    else if (row cell2) < (row cell1) then
        cell1
    else if (col cell1) < (col cell2) then
        cell2
    else if (col cell2) < (col cell1) then
        cell1
    else
        cell1


row : Cell -> Int
row cell =
    first cell


col : Cell -> Int
col cell =
    second cell


left : Cell -> Cell
left cell =
    mapSecond (\col -> B.max 1 (col - 1)) cell


right : Cell -> Cell
right cell =
    mapSecond (\col -> B.min 26 (col + 1)) cell


down : Cell -> Cell
down cell =
    mapFirst (\row -> B.min 100 (row + 1)) cell


up : Cell -> Cell
up cell =
    mapFirst (\row -> B.max 1 (row - 1)) cell
