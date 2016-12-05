port module Main exposing (main)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, classList, id, value)
import Html.Events exposing (keyCode, onClick, onDoubleClick, onFocus, onInput, onMouseDown, onMouseEnter, onMouseUp, onWithOptions, Options)
import StyleHelper exposing (..)
import Dict exposing (Dict)
import Defaults exposing (defaults)
import Utils exposing (..)
import Task exposing (Task)
import Dom
import Css exposing (color, textShadow)
import Cell exposing (Cell)
import Basics exposing (max, min)


type alias Data =
    Dict Cell String



-- Helpers


type alias HtmlContainer =
    List (Html Msg) -> Html Msg


data2HeaderCells : Data -> (Cell -> Msg) -> List (Html Msg)
data2HeaderCells data cmdMsg =
    data
        |> Dict.map
            (\cell value ->
                headerCell cell value (cmdMsg cell)
            )
        |> Dict.values



-- Model


type alias Model =
    { sheetLayout : SheetLayout
    , dragging : Bool
    , editing : Bool
    , activeCell : Cell
    , selectionEnd : Cell
    , data : Data
    , rowHeaderData : Data
    , colHeaderData : Data
    }


init : ( Model, Cmd Msg )
init =
    let
        colHeaderData =
            List.range 1 defaults.numRows
                |> List.map (\idx -> ( ( idx, 1 ), toString idx ))
                |> Dict.fromList

        rowHeaderData =
            List.range 1 defaults.numCols
                |> List.map (\idx -> ( ( 1, idx ), alpha idx ))
                |> Dict.fromList
    in
        ( { sheetLayout = defaults
          , dragging = False
          , editing = False
          , activeCell = Cell.new 1 1
          , selectionEnd = Cell.new 1 1
          , data = Dict.empty
          , rowHeaderData = rowHeaderData
          , colHeaderData = colHeaderData
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
            , marginLeft location.left
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
    input
        [ class "data-cell"
        , id ((toString row) ++ "-" ++ (toString col))
        , styles
            [ gridRow row row
            , gridColumn col col
            , Css.color Css.transparent
            , Css.textShadow4 (Css.px 0) (Css.px 0) (Css.px 0) (Css.rgb 165 170 178)
            ]
        , onDoubleClick (EditCell row col)
        , onMouseDown (DragStart row col)
        , onMouseUp (DragEnd row col)
        , onMouseEnter (DragMove row col)
        , onInput (\content -> CellInput row col content)
        , value (Maybe.withDefault "" data)
        ]
        []


headerCell : Cell -> String -> Msg -> Html Msg
headerCell cell value cmd =
    div
        [ class "header-cell"
        , styles
            [ gridRow (Cell.row cell) (Cell.row cell)
            , gridColumn (Cell.col cell) (Cell.col cell)
            ]
        , onClick cmd
        ]
        [ text value ]


cornerCell : SheetLayout -> Html Msg
cornerCell sheetLayout =
    div
        [ class "corner-cell"
        , styles
            [ cssWidth (sheetLayout.colHeaderColWidth)
            , cssHeight (sheetLayout.dfltRowHeight + sheetLayout.gridGap)
            ]
        , onClick SelectAll
        ]
        []


selectionCell : Cell -> Bool -> Html Msg
selectionCell cell isActive =
    div
        [ classList
            [ ( "active-cell", isActive )
            , ( "selection-cell", not isActive )
            ]
        , styles
            [ gridRow (Cell.row cell) (Cell.row cell)
            , gridColumn (Cell.col cell) (Cell.col cell)
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


rowHeaderCells : SheetLayout -> Data -> List (Html Msg)
rowHeaderCells sheetLayout rowHeaderData =
    data2HeaderCells
        rowHeaderData
        (\cell -> SelectCol << Cell.col <| cell)


colHeaderContainer : SheetLayout -> HtmlContainer
colHeaderContainer sheetLayout =
    gridLayoutContainer "col-header"
        sheetLayout.colHeaderColWidth
        sheetLayout.totalHeight
        (Location (sheetLayout.dfltRowHeight + sheetLayout.gridGap) 0)


colHeader : SheetLayout -> HtmlContainer
colHeader sheetLayout =
    gridLayout sheetLayout.numRows 1 sheetLayout.dfltRowHeight sheetLayout.colHeaderColWidth


colHeaderCells : SheetLayout -> Data -> List (Html Msg)
colHeaderCells sheetLayout colHeaderData =
    data2HeaderCells
        colHeaderData
        (\cell -> SelectRow << Cell.row <| cell)



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


selectionCells : Cell -> Cell -> List (Html Msg)
selectionCells activeCell selectionEnd =
    let
        start =
            Cell.min activeCell selectionEnd

        end =
            Cell.max activeCell selectionEnd
    in
        List.concatMap
            (\row ->
                List.map
                    (\col ->
                        selectionCell (Cell.new row col) (Cell.equal (Cell.new row col) activeCell)
                    )
                    (List.range (Cell.col start) (Cell.col end))
            )
            (List.range (Cell.row start) (Cell.row end))


selectionRange : Cell -> Cell -> Html Msg
selectionRange activeCell selectionEnd =
    let
        start =
            Cell.min activeCell selectionEnd

        end =
            Cell.max activeCell selectionEnd
    in
        div
            [ class "selection-range"
            , styles
                [ gridRow (Cell.row start) ((Cell.row end) + 1)
                , gridColumn (Cell.col start) ((Cell.col end) + 1)
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
        { activeCell, data, sheetLayout, rowHeaderData, colHeaderData, selectionEnd } =
            model
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
                        , selectionCells activeCell selectionEnd
                        , [ selectionRange activeCell selectionEnd ]
                        ]
                    )
                ]
            , cornerCell sheetLayout
            , rowHeaderContainer sheetLayout
                [ rowHeader sheetLayout
                    (rowHeaderCells sheetLayout rowHeaderData)
                ]
            , colHeaderContainer sheetLayout
                [ colHeader sheetLayout
                    (colHeaderCells sheetLayout colHeaderData)
                ]
            ]



-- View


view : Model -> Html Msg
view model =
    sheet model



-- Commands


setFocus : Cell -> Cmd Msg
setFocus cell =
    Task.attempt SetFocus (Dom.focus ((toString (Cell.row cell)) ++ "-" ++ (toString (Cell.col cell))))


logError : String -> Cmd Msg
logError msg =
    let
        foo =
            Debug.log msg 1
    in
        Cmd.none


updateContent : Int -> Int -> String -> Model -> Model
updateContent row col content model =
    { model | data = (Dict.insert ( row, col ) content model.data) }


updateActiveCell : Model -> (Cell -> Cell) -> ( Model, Cmd Msg )
updateActiveCell model moveCell =
    let
        cell =
            moveCell model.activeCell
    in
        ( { model
            | activeCell = cell
            , selectionEnd = cell
          }
        , setFocus cell
        )


updateSelection : Model -> (Cell -> Cell) -> ( Model, Cmd Msg )
updateSelection model moveCell =
    ( { model
        | selectionEnd = moveCell model.selectionEnd
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | CellInput Int Int String
    | EditCell Int Int
    | DragEnd Int Int
    | DragMove Int Int
    | DragStart Int Int
    | SetFocus (Result Dom.Error ())
    | KeyDown ( String, Bool )
    | SelectCol Int
    | SelectRow Int
    | SelectAll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ activeCell, selectionEnd, sheetLayout } as model) =
    case msg of
        CellInput row col content ->
            ( updateContent row col content model
            , Cmd.none
            )

        SetFocus result ->
            case result of
                Ok ok ->
                    ( model, Cmd.none )

                Err (Dom.NotFound msg) ->
                    ( model, logError ("Could not find element to focus: " ++ msg) )

        EditCell row col ->
            ( { model
                | activeCell = Cell.new row col
                , dragging = False
                , editing = True
                , selectionEnd = Cell.new row col
              }
            , Cmd.none
            )

        KeyDown ( "ArrowLeft", True ) ->
            updateSelection model Cell.left

        KeyDown ( "ArrowLeft", False ) ->
            updateActiveCell model Cell.left

        KeyDown ( "ArrowRight", True ) ->
            updateSelection model Cell.right

        KeyDown ( "ArrowRight", False ) ->
            updateActiveCell model Cell.right

        KeyDown ( "ArrowUp", True ) ->
            updateSelection model Cell.up

        KeyDown ( "ArrowUp", False ) ->
            updateActiveCell model Cell.up

        KeyDown ( "ArrowDown", True ) ->
            updateSelection model Cell.down

        KeyDown ( "ArrowDown", False ) ->
            updateActiveCell model Cell.down

        KeyDown ( "Enter", shiftKey ) ->
            updateActiveCell model Cell.down

        KeyDown ( "Tab", True ) ->
            updateActiveCell model Cell.left

        KeyDown ( "Tab", False ) ->
            updateActiveCell model Cell.right

        KeyDown ( _, _ ) ->
            ( model, Cmd.none )

        DragStart row col ->
            let
                cell =
                    Cell.new row col
            in
                ( { model
                    | dragging = True
                    , activeCell = cell
                    , selectionEnd = cell
                  }
                , setFocus cell
                )

        DragMove row col ->
            case model.dragging of
                False ->
                    ( model, Cmd.none )

                True ->
                    ( { model | selectionEnd = Cell.new row col }
                    , Cmd.none
                    )

        DragEnd row col ->
            ( { model | dragging = False }
            , Cmd.none
            )

        SelectRow row ->
            ( { model
                | activeCell = (Cell.new row 1)
                , selectionEnd = (Cell.new row sheetLayout.numCols)
              }
            , setFocus (Cell.new row 1)
            )

        SelectCol col ->
            ( { model
                | activeCell = (Cell.new 1 col)
                , selectionEnd = (Cell.new sheetLayout.numRows col)
              }
            , setFocus (Cell.new 1 col)
            )

        SelectAll ->
            ( { model
                | activeCell = Cell.new 1 1
                , selectionEnd = Cell.new sheetLayout.numRows sheetLayout.numCols
              }
            , setFocus (Cell.new 1 1)
            )

        NoOp ->
            ( model, Cmd.none )



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
