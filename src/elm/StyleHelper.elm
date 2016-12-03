module StyleHelper exposing (..)

import Css exposing (..)


zIndex : Int -> Mixin
zIndex i =
    property "z-index" <| toString i


gridTemplateColumns : String -> Mixin
gridTemplateColumns b =
    property "grid-template-columns" <| b


gridTemplateRows : String -> Mixin
gridTemplateRows b =
    property "grid-template-rows" <| b


gridRow : Int -> Int -> Mixin
gridRow start end =
    property "grid-row" <| ((toString start) ++ " / " ++ (toString end))


gridColumn : Int -> Int -> Mixin
gridColumn start end =
    property "grid-column" <| ((toString start) ++ " / " ++ (toString end))


pointerEvents : String -> Mixin
pointerEvents b =
    property "pointer-events" <| b
