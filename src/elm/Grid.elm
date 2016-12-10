module Grid exposing (..)

import Html exposing (Html)
import StyleHelper exposing (..)


type alias Anchor =
    { top : Int
    , left : Int
    }


type alias Container =
    { width : Int
    , height : Int
    , anchor : Anchor
    }


type alias Grid =
    { colWidth : Int
    , rowHeight : Int
    , numRows : Int
    , numCols : Int
    }


container : Container -> Html.Attribute msg
container { width, height, anchor } =
    styles
        [ cssWidth width
        , cssHeight height
        , cssTop anchor.top
        , marginLeft anchor.left
        ]


grid : Grid -> Html.Attribute msg
grid { colWidth, rowHeight, numRows, numCols } =
    styles
        [ gridTemplateColumns numCols colWidth
        , gridTemplateRows numRows rowHeight
        ]
