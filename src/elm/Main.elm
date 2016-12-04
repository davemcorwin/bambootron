port module Main exposing (main)

import Html exposing (Html, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, onClick, onDoubleClick, onFocus, onInput, onMouseDown, onMouseEnter, onMouseUp, onWithOptions, Options)
import Css exposing (..)
import StyleHelper exposing (..)
import Dict exposing (Dict)
import Defaults exposing (defaults)
import Utils exposing (..)


type Msg
    = NoOp
    | ActivateCell Int Int
    | CellInput Int Int String
    | EditCell Int Int
    | DragEnd Int Int
    | DragMove Int Int
    | DragStart Int Int
      -- | FocusError Dom.Error
      -- | FocusSuccess
    | KeyDown ( String, Bool )
    | Select Range



-- Helpers


type alias HtmlContainer =
    List (Html Msg) -> Html Msg


data2HeaderCells : Data -> (Cell -> Range) -> List (Html Msg)
data2HeaderCells data selectRange =
    data
        |> Dict.map
            (\cellTuple value ->
                let
                    cell =
                        tuple2Cell cellTuple

                    cmd =
                        Select (selectRange cell)
                in
                    headerCell cell value cmd
            )
        |> Dict.values



-- Model


type alias Model =
    { sheetLayout : SheetLayout
    , dragging : Bool
    , editing : Bool
    , activeCell : Cell
    , selection : Range
    , data : Data
    }


init : ( Model, Cmd Msg )
init =
    ( { sheetLayout = defaults
      , dragging = False
      , editing = False
      , activeCell = Cell 1 1
      , selection = Range 1 1 1 1
      , data = Dict.empty
      }
    , Cmd.none
    )



-- Util


gridLayoutContainer : String -> Int -> Int -> Location -> HtmlContainer
gridLayoutContainer className gridWidth gridHeight location =
    div
        [ class className
        , styles
            [ cssWidth gridWidth
            , cssHeight gridHeight
            , cssTop location.top
            , marginLeft (px (toFloat location.left))
            ]
        ]


gridLayout : Int -> Int -> Int -> Int -> HtmlContainer
gridLayout numRows numCols rowHeight colWidth =
    div
        [ class "grid"
        , styles
            [ gridTemplateColumns numCols colWidth
            , gridTemplateRows numRows rowHeight
            ]
        ]



-- Cells


dataCell : Int -> Int -> Cell -> Maybe String -> Html Msg
dataCell row col activeCell data =
    div
        [ class "data-cell"
        , styles
            [ gridRow row row
            , gridColumn col col
            ]
        , contenteditable (activeCell.row == row && activeCell.column == col)
        , onDoubleClick (EditCell row col)
        , onMouseDown (DragStart row col)
        , onMouseUp (DragEnd row col)
        , onMouseEnter (DragMove row col)
          -- , onFocus (ActivateCell row col)
          -- , onInput (\content -> CellInput row col content)
          -- , value (Maybe.withDefault "" data)
        ]
        []



-- dataCell : Int -> Int -> Maybe String -> Html Msg
-- dataCell row col data =
--     input
--         [ type_ "Html.text"
--           -- , id ("input-" ++ (toString row) ++ "-" ++ (toString col))
--         , class "data-cell"
--         , styles
--             [ gridRow row row
--             , gridColumn col col
--             ]
--         , onDoubleClick (EditCell row col)
--         , onMouseDown (DragStart row col)
--         , onMouseUp (DragEnd row col)
--         , onMouseEnter (DragMove row col)
--         , onFocus (ActivateCell row col)
--         , onInput (\content -> CellInput row col content)
--         , value (Maybe.withDefault "" data)
--         ]
--         []


headerCell : Cell -> String -> Msg -> Html Msg
headerCell cell value msg =
    div
        [ class "header-cell"
        , styles
            [ gridRow cell.row cell.row
            , gridColumn cell.column cell.column
            ]
        , onClick msg
        ]
        [ Html.text value ]


cornerCell : SheetLayout -> Html Msg
cornerCell sheetLayout =
    div
        [ class "corner-cell"
        , styles
            [ cssWidth (sheetLayout.colHeaderColWidth)
            , cssHeight (sheetLayout.dfltRowHeight + sheetLayout.gridGap)
            ]
        , onClick (Select (Range 1 sheetLayout.numRows 1 sheetLayout.numCols))
        ]
        []


selectionCell : Cell -> Bool -> Html Msg
selectionCell cell isActive =
    div
        [ class
            (if isActive then
                "active-cell"
             else
                "selection-cell"
            )
        , styles
            [ gridRow cell.row cell.row
            , gridColumn cell.column cell.column
            ]
        ]
        []


rowHeaderContainer : SheetLayout -> HtmlContainer
rowHeaderContainer sheetLayout =
    gridLayoutContainer
        "row-header"
        sheetLayout.totalWidth
        (sheetLayout.dfltRowHeight + 1)
        (Location 0 sheetLayout.colHeaderColWidth)


rowHeader : SheetLayout -> HtmlContainer
rowHeader sheetLayout =
    gridLayout 1 sheetLayout.numCols sheetLayout.dfltRowHeight sheetLayout.dfltColWidth


rowHeaderCells : SheetLayout -> List (Html Msg)
rowHeaderCells sheetLayout =
    data2HeaderCells
        sheetLayout.rowHeaderData
        (\cell -> Range 1 sheetLayout.numRows cell.column cell.column)


colHeaderContainer : SheetLayout -> HtmlContainer
colHeaderContainer sheetLayout =
    gridLayoutContainer "col-header"
        sheetLayout.colHeaderColWidth
        sheetLayout.totalHeight
        (Location (sheetLayout.dfltRowHeight + sheetLayout.gridGap) 0)


colHeader : SheetLayout -> HtmlContainer
colHeader sheetLayout =
    gridLayout sheetLayout.numRows 1 sheetLayout.dfltRowHeight sheetLayout.colHeaderColWidth


colCells : SheetLayout -> List (Html Msg)
colCells sheetLayout =
    data2HeaderCells
        sheetLayout.colHeaderData
        (\cell -> Range cell.row cell.row 1 sheetLayout.numCols)



-- Ranges


dataCells : Cell -> SheetLayout -> Data -> List (Html Msg)
dataCells activeCell sheetLayout data =
    List.concatMap
        (\row ->
            List.map
                (\col -> dataCell row col activeCell (Dict.get ( row, col ) data))
                (List.range 1 sheetLayout.numCols)
        )
        (List.range 1 sheetLayout.numRows)


selectionCells : Cell -> Range -> List (Html Msg)
selectionCells activeCell selection =
    let
        { endColumn, endRow, startColumn, startRow } =
            selection
    in
        List.concatMap
            (\row ->
                List.map
                    (\col ->
                        selectionCell (Cell row col) (row == activeCell.row && col == activeCell.column)
                    )
                    (List.range startColumn endColumn)
            )
            (List.range startRow endRow)


selectionRange : Range -> Html Msg
selectionRange selection =
    div
        [ class "selection-range"
        , styles
            [ gridRow selection.startRow (selection.endRow + 1)
            , gridColumn selection.startColumn (selection.endColumn + 1)
            ]
        ]
        []



-- Main


rowsColsContainer : SheetLayout -> HtmlContainer
rowsColsContainer sheetLayout =
    gridLayoutContainer
        "data"
        sheetLayout.totalWidth
        sheetLayout.totalHeight
        (Location
            (sheetLayout.dfltRowHeight + 1)
            ((sheetLayout.dfltColWidth // 2) + 1)
        )


rowsCols : SheetLayout -> HtmlContainer
rowsCols sheetLayout =
    gridLayout
        sheetLayout.numRows
        sheetLayout.numCols
        sheetLayout.dfltRowHeight
        sheetLayout.dfltColWidth



-- Sheet


sheet : Model -> Html Msg
sheet model =
    let
        { activeCell, data, sheetLayout, selection } =
            model

        -- sheetWidth =
        --     sheetLayout.dfltColWidth
        --         |> (+) sheetLayout.gridGap
        --         |> (*) sheetLayout.numCols
        --         |> (+) sheetLayout.colHeaderColWidth
        --
        -- sheetHeight =
        --     sheetLayout.dfltRowHeight
        --         |> (+) sheetLayout.gridGap
        --         |> (*) (sheetLayout.numRows + sheetLayout.gridGap)
    in
        div
            [ id "sheet"
            , styles
                [ cssWidth sheetLayout.totalWidth
                , cssHeight sheetLayout.totalHeight
                ]
            ]
            [ rowsColsContainer sheetLayout
                [ rowsCols sheetLayout
                    (List.concat
                        [ dataCells activeCell sheetLayout data
                        , selectionCells activeCell selection
                        , [ selectionRange selection ]
                        ]
                    )
                ]
            , cornerCell sheetLayout
            , rowHeaderContainer sheetLayout
                [ rowHeader sheetLayout
                    (rowHeaderCells sheetLayout)
                ]
            , colHeaderContainer sheetLayout
                [ colHeader sheetLayout
                    (colCells sheetLayout)
                ]
            ]



-- View


view : Model -> Html Msg
view model =
    sheet model



-- domFocus : Result Dom.Error value -> Msg
-- domFocus result =
--     case result of
--         Ok value ->
--             FocusSuccess
--
--         Err error ->
--             FocusError error
-- domFocusTask : Cell -> Task Dom.Error ()
-- domFocusTask cell =
--     (Dom.focus ("input-" ++ (toString cell.row) ++ "-" ++ (toString cell.column)))
--
--
-- focusCmd : Cell -> Cmd Msg
-- focusCmd cell =
--     Task.perform FocusSuccess (domFocusTask cell)


activateCell : Cell -> Model -> Model
activateCell cell model =
    { model | activeCell = cell }


selectRange : Range -> Model -> Model
selectRange range model =
    { model | selection = range }


updateContent : Int -> Int -> String -> Model -> Model
updateContent row col content model =
    { model | data = (Dict.insert ( row, col ) content model.data) }


updateHelper : Model -> ( Model, Cmd Msg )
updateHelper model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ activeCell, selection } as model) =
    case msg of
        ActivateCell row col ->
            updateHelper
                (model
                    |> activateCell (Cell row col)
                    |> selectRange (Range row row col col)
                )

        CellInput row col content ->
            updateHelper
                (model
                    |> updateContent row col content
                )

        -- FocusSuccess ->
        --     updateHelper model
        EditCell row col ->
            updateHelper
                { model
                    | activeCell = Cell row col
                    , dragging = False
                    , editing = True
                    , selection = Range row row col col
                }

        KeyDown ( key, shiftKey ) ->
            let
                cell =
                    case key of
                        "ArrowLeft" ->
                            Cell activeCell.row (Basics.max 1 (activeCell.column - 1))

                        "ArrowUp" ->
                            Cell (Basics.max 1 (activeCell.row - 1)) activeCell.column

                        "ArrowRight" ->
                            Cell activeCell.row (Basics.min model.sheetLayout.numCols (activeCell.column + 1))

                        "Tab" ->
                            case shiftKey of
                                True ->
                                    Cell activeCell.row (Basics.max 1 (activeCell.column - 1))

                                False ->
                                    Cell activeCell.row (Basics.min model.sheetLayout.numCols (activeCell.column + 1))

                        "ArrowDown" ->
                            Cell (Basics.min model.sheetLayout.numRows (activeCell.row + 1)) activeCell.column

                        _ ->
                            activeCell
            in
                updateHelper
                    { model
                        | activeCell = cell
                        , selection = Range cell.row cell.row cell.column cell.column
                    }

        DragStart row col ->
            updateHelper
                { model
                    | dragging = True
                    , activeCell = Cell row col
                    , selection = Range row row col col
                }

        DragMove row col ->
            updateHelper
                (case model.dragging of
                    False ->
                        model

                    True ->
                        { model
                            | selection =
                                Range
                                    (Basics.min activeCell.row row)
                                    (Basics.max activeCell.row row)
                                    (Basics.min activeCell.column col)
                                    (Basics.max activeCell.column col)
                        }
                )

        DragEnd row col ->
            updateHelper
                { model | dragging = False }

        Select range ->
            updateHelper
                { model
                    | activeCell = Cell range.startRow range.startColumn
                    , selection = range
                }

        NoOp ->
            updateHelper model



-- Subscriptions


port keys : (( String, Bool ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ keys KeyDown
        ]



--
-- App


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
