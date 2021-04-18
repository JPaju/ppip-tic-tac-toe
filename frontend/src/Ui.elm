module Ui exposing (black, grey, notAllowed, red)

import Element exposing (Attribute, Color)
import Html.Attributes


red : Color
red =
    Element.rgb255 249 28 114


grey : Color
grey =
    Element.rgb255 224 224 224


black : Color
black =
    Element.rgb255 0 0 0


notAllowed : Attribute msg
notAllowed =
    Element.htmlAttribute <| Html.Attributes.style "cursor" "not-allowed"
