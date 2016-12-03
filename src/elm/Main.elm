port module Main exposing (main)

import Html exposing (Html, div, input)
import Html.Lazy exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, onClick, onDoubleClick, onFocus, onInput, onMouseDown, onMouseEnter, onMouseUp, onWithOptions, Options)
import Css exposing (..)
import StyleHelper exposing (..)
import String
import Dict exposing (Dict)


styles : List Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


strPx : Int -> String
strPx value =
    toString value



-- Model


rowsFun : Int -> Int -> String
rowsFun rowHeight numRows =
    rowHeight
        |> List.repeat numRows
        |> List.map strPx
        |> String.join " "


colsFun : Int -> Int -> String
colsFun colWidth numCols =
    colWidth
        |> List.repeat numCols
        |> List.map strPx
        |> String.join " "


type alias Data =
    Dict ( Int, Int ) String


type alias Cell =
    { row : Int
    , column : Int
    }


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
    , rows : String
    , columns : String
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

        defaults =
            { numCols = numCols
            , numRows = numRows
            , dfltColWidth = dfltColWidth
            , dfltRowHeight = dfltRowHeight
            , colHeaderColWidth = 50
            , rows = rowsFun dfltRowHeight numRows
            , columns = colsFun dfltColWidth numCols
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


alpha : Int -> String
alpha idx =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        |> String.slice idx (idx + 1)


gridLayout : String -> String -> List (Html Msg) -> Html Msg
gridLayout rows cols children =
    div
        [ class "grid"
        , styles
            [ gridTemplateColumns cols
            , gridTemplateRows rows
            ]
        ]
        children



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


headerCell : Int -> Int -> String -> Msg -> Html Msg
headerCell row col value msg =
    div
        [ class "header-cell"
        , styles
            [ gridRow row row
            , gridColumn col col
            ]
        , onClick msg
        ]
        [ Html.text value ]


cornerCell : Defaults -> Html Msg
cornerCell defaults =
    let
        { colHeaderColWidth, dfltRowHeight } =
            defaults
    in
        div
            [ class "corner-cell"
            , styles
                [ Css.width (px (toFloat (colHeaderColWidth + 1)))
                , Css.height (px (toFloat (dfltRowHeight + 1)))
                ]
            , onClick SelectAll
            ]
            []


selectionCell : Int -> Int -> Bool -> Html Msg
selectionCell row col isActive =
    div
        [ class
            (if isActive then
                "active-cell"
             else
                "selection-cell"
            )
        , styles
            [ gridRow row row
            , gridColumn col col
            ]
        ]
        []



-- Headers


rowHeader : Defaults -> Html Msg
rowHeader defaults =
    let
        { colHeaderColWidth, columns, dfltColWidth, dfltRowHeight, numCols } =
            defaults

        cells =
            List.range 1 numCols
                |> List.map (\col -> headerCell 1 col (alpha (col - 1)) (SelectColumn col))
    in
        div
            [ class "row-header"
            , styles
                [ Css.width (px (toFloat ((dfltColWidth + 1) * numCols + colHeaderColWidth + 1)))
                , Css.height (px (toFloat (dfltRowHeight + 1)))
                , marginLeft (px (toFloat (dfltColWidth // 2 + 1)))
                ]
            ]
            [ gridLayout (strPx dfltRowHeight) columns cells ]


colHeader : Defaults -> Html Msg
colHeader defaults =
    let
        { colHeaderColWidth, dfltColWidth, dfltRowHeight, numRows, rows } =
            defaults

        cells =
            List.range 1 numRows
                |> List.map (\row -> headerCell row 1 (toString row) (SelectRow row))
    in
        div
            [ class "col-header"
            , styles
                [ Css.height (px (toFloat (dfltRowHeight * (numRows + 1))))
                , Css.width (px (toFloat (colHeaderColWidth + 1)))
                ]
            ]
            [ gridLayout rows (strPx (dfltColWidth // 2)) cells ]



-- Ranges


dataCells : Cell -> Defaults -> Data -> List (Html Msg)
dataCells activeCell defaults data =
    let
        { dfltColWidth, dfltRowHeight, numCols, numRows, rows, columns } =
            defaults
    in
        List.concatMap
            (\row ->
                List.map
                    (\col -> dataCell row col activeCell (Dict.get ( row, col ) data))
                    (List.range 1 numCols)
            )
            (List.range 1 numRows)


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
                        lazy3 selectionCell row col (row == activeCell.row && col == activeCell.column)
                    )
                    (List.range startColumn endColumn)
            )
            (List.range startRow endRow)


selectionRange : Range -> Html Msg
selectionRange selection =
    let
        { endColumn, endRow, startColumn, startRow } =
            selection
    in
        div
            [ class "selection-range"
            , styles
                [ gridRow startRow (endRow + 1)
                , gridColumn startColumn (endColumn + 1)
                ]
            ]
            []



-- Main


foo : Model -> Html Msg
foo model =
    let
        { activeCell, data, defaults, selection } =
            model

        { columns, dfltColWidth, dfltRowHeight, rows } =
            defaults
    in
        div
            [ class "data"
            , styles
                [ left (px (toFloat ((dfltColWidth // 2) + 1)))
                , top (px (toFloat (dfltRowHeight + 1)))
                ]
            ]
            [ gridLayout rows
                columns
                (List.concat
                    [ dataCells activeCell defaults data
                    , selectionCells activeCell selection
                    , [ selectionRange selection ]
                    ]
                )
            ]



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
                [ Css.width (px (toFloat ((defaults.dfltColWidth + 1) * defaults.numCols + defaults.colHeaderColWidth + 1)))
                , Css.height (px (toFloat ((defaults.dfltRowHeight + 1) * (defaults.numRows + 1))))
                ]
            ]
            [ lazy foo model
            , lazy cornerCell defaults
            , lazy rowHeader defaults
            , lazy colHeader defaults
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
    | SelectAll
    | SelectColumn Int
    | SelectRow Int



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

        SelectAll ->
            updateHelper
                { model
                    | activeCell = Cell 1 1
                    , selection =
                        Range 1 model.defaults.numRows 1 model.defaults.numCols
                }

        SelectColumn col ->
            updateHelper
                { model
                    | activeCell = Cell 1 col
                    , selection =
                        Range 1 model.defaults.numRows col col
                }

        SelectRow row ->
            updateHelper
                { model
                    | activeCell = Cell row 1
                    , selection =
                        Range row row 1 model.defaults.numCols
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
