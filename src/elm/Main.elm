port module Main exposing (main)

import Html exposing (Html, div, input)
import Html.Lazy exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, onClick, onDoubleClick, onFocus, onInput, onMouseDown, onMouseEnter, onMouseUp, onWithOptions, Options)
import Css exposing (..)
import StyleHelper exposing (..)
import String
import Dict exposing (Dict)


-- Helpers


styles : List Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


strPx : Int -> String
strPx value =
    toString value


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


type alias HtmlContainer =
    List (Html Msg) -> Html Msg


type alias Cell =
    { row : Int
    , column : Int
    }


type alias Location =
    { top : Int
    , left : Int
    }


type alias Data =
    Dict ( Int, Int ) String


type alias Range =
    { startRow : Int
    , endRow : Int
    , startColumn : Int
    , endColumn : Int
    }


type alias Defaults =
    { numCols : Int
    , numRows : Int
    , dfltColWidth : Int
    , dfltRowHeight : Int
    , colHeaderColWidth : Int
    , totalWidth : Int
    , totalHeight : Int
    , rowHeaderData : Data
    , colHeaderData : Data
    }


type alias Model =
    { defaults : Defaults
    , dragging : Bool
    , editing : Bool
    , activeCell : Cell
    , selection : Range
    , data : Data
    }


init : ( Model, Cmd Msg )
init =
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

        colHeaderData =
            List.range 1 numRows
                |> List.map (\idx -> ( ( idx, 1 ), toString idx ))
                |> Dict.fromList

        rowHeaderData =
            List.range 1 numCols
                |> List.map (\idx -> ( ( 1, idx ), alpha idx ))
                |> Dict.fromList

        defaults =
            { numCols = numCols
            , numRows = numRows
            , dfltColWidth = dfltColWidth
            , dfltRowHeight = dfltRowHeight
            , colHeaderColWidth = colHeaderColWidth
            , totalWidth = (dfltColWidth + 1) * numCols + colHeaderColWidth
            , totalHeight = (dfltRowHeight + 1) * (numRows + 1)
            , rowHeaderData = rowHeaderData
            , colHeaderData = colHeaderData
            }
    in
        ( { defaults = defaults
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
            [ Css.width (px (toFloat gridWidth))
            , Css.height (px (toFloat gridHeight))
            , Css.top (px (toFloat location.top))
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


cornerCell : Defaults -> Html Msg
cornerCell dflts =
    div
        [ class "corner-cell"
        , styles
            [ Css.width (px (toFloat (dflts.colHeaderColWidth)))
            , Css.height (px (toFloat (dflts.dfltRowHeight + 1)))
            ]
        , onClick (Select (Range 1 dflts.numRows 1 dflts.numCols))
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


rowHeaderContainer : Defaults -> HtmlContainer
rowHeaderContainer dflts =
    gridLayoutContainer
        "row-header"
        dflts.totalWidth
        (dflts.dfltRowHeight + 1)
        (Location 0 dflts.colHeaderColWidth)


rowHeader : Defaults -> HtmlContainer
rowHeader dflts =
    gridLayout 1 dflts.numCols dflts.dfltRowHeight dflts.dfltColWidth


rowHeaderCells : Defaults -> List (Html Msg)
rowHeaderCells dflts =
    data2HeaderCells
        dflts.rowHeaderData
        (\cell -> Range 1 dflts.numCols cell.column cell.column)


colHeaderContainer : Defaults -> HtmlContainer
colHeaderContainer dflts =
    gridLayoutContainer "col-header"
        dflts.colHeaderColWidth
        dflts.totalHeight
        (Location (dflts.dfltRowHeight + 1) 0)


colHeader : Defaults -> HtmlContainer
colHeader dflts =
    gridLayout dflts.numRows 1 dflts.dfltRowHeight dflts.colHeaderColWidth


colCells : Defaults -> List (Html Msg)
colCells dflts =
    data2HeaderCells
        dflts.colHeaderData
        (\cell -> Range cell.row cell.row 1 dflts.numRows)



-- Ranges


dataCells : Cell -> Defaults -> Data -> List (Html Msg)
dataCells activeCell dflts data =
    List.concatMap
        (\row ->
            List.map
                (\col -> dataCell row col activeCell (Dict.get ( row, col ) data))
                (List.range 1 dflts.numCols)
        )
        (List.range 1 dflts.numRows)


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
                        lazy2 selectionCell (Cell row col) (row == activeCell.row && col == activeCell.column)
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


rowsColsContainer : Defaults -> HtmlContainer
rowsColsContainer dflts =
    gridLayoutContainer
        "data"
        dflts.totalWidth
        dflts.totalHeight
        (Location
            (dflts.dfltRowHeight + 1)
            ((dflts.dfltColWidth // 2) + 1)
        )


rowsCols : Defaults -> HtmlContainer
rowsCols dflts =
    gridLayout
        dflts.numRows
        dflts.numCols
        dflts.dfltRowHeight
        dflts.dfltColWidth



-- Sheet


sheet : Model -> Html Msg
sheet model =
    let
        { activeCell, data, defaults, selection } =
            model
    in
        div
            [ id "sheet"
            , styles
                [ Css.width (px (toFloat ((defaults.dfltColWidth + 1) * defaults.numCols + defaults.colHeaderColWidth)))
                , Css.height (px (toFloat ((defaults.dfltRowHeight + 1) * (defaults.numRows + 1))))
                ]
            ]
            [ rowsColsContainer defaults
                [ rowsCols defaults
                    (List.concat
                        [ dataCells activeCell defaults data
                        , selectionCells activeCell selection
                        , [ selectionRange selection ]
                        ]
                    )
                ]
            , cornerCell defaults
            , rowHeaderContainer defaults
                [ rowHeader defaults
                    (rowHeaderCells defaults)
                ]
            , colHeaderContainer defaults
                [ colHeader defaults
                    (colCells defaults)
                ]
            ]



-- View


view : Model -> Html Msg
view model =
    sheet model



-- Update


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
                            Cell activeCell.row (Basics.min model.defaults.numCols (activeCell.column + 1))

                        "Tab" ->
                            case shiftKey of
                                True ->
                                    Cell activeCell.row (Basics.max 1 (activeCell.column - 1))

                                False ->
                                    Cell activeCell.row (Basics.min model.defaults.numCols (activeCell.column + 1))

                        "ArrowDown" ->
                            Cell (Basics.min model.defaults.numRows (activeCell.row + 1)) activeCell.column

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
