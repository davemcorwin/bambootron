module StyleHelper exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Css exposing (..)


-- Helpers


styles : List Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


cssWidth : Int -> Mixin
cssWidth =
    Css.width << px << toFloat


cssHeight : Int -> Mixin
cssHeight =
    Css.height << px << toFloat


cssTop : Int -> Mixin
cssTop =
    Css.top << px << toFloat


marginLeft : Int -> Mixin
marginLeft =
    Css.marginLeft << px << toFloat



-- Additional Properties


zIndex : Int -> Mixin
zIndex i =
    property "z-index" <| toString i


gridTemplateColumns : Int -> Int -> Mixin
gridTemplateColumns numCols colWidth =
    property "grid-template-columns"
        (List.repeat numCols colWidth
            |> List.map (\colWidth -> (toString colWidth) ++ "px")
            |> String.join " "
        )


gridTemplateRows : Int -> Int -> Mixin
gridTemplateRows numRows rowHeight =
    property "grid-template-rows"
        (List.repeat numRows rowHeight
            |> List.map (\rowHeight -> (toString rowHeight) ++ "px")
            |> String.join " "
        )


gridRow : Int -> Int -> Mixin
gridRow start end =
    property "grid-row" <| ((toString start) ++ " / " ++ (toString end))


gridColumn : Int -> Int -> Mixin
gridColumn start end =
    property "grid-column" <| ((toString start) ++ " / " ++ (toString end))


pointerEvents : String -> Mixin
pointerEvents b =
    property "pointer-events" <| b
