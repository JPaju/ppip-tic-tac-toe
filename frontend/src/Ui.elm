module Ui exposing (black, blue, button, grey, notAllowed, red, white)

import Element exposing (Attribute, Color, Element, el)
import Html.Attributes
import Widget exposing (textButton)
import Widget.Material as Material



---- COLOR ----


red : Color
red =
    Element.rgb255 249 28 114


blue : Color
blue =
    Element.rgb255 95 171 220


grey : Color
grey =
    Element.rgb255 224 224 224


black : Color
black =
    Element.rgb255 0 0 0


white : Color
white =
    Element.rgb255 255 255 255



---- CURSOR ----


notAllowed : Attribute msg
notAllowed =
    Element.htmlAttribute <| Html.Attributes.style "cursor" "not-allowed"



---- BUTTON ----


type alias ButtonConfig msg =
    { label : String
    , enabled : Bool
    , onClick : msg
    }


button : List (Attribute msg) -> ButtonConfig msg -> Element msg
button attrs { label, enabled, onClick } =
    el attrs <|
        textButton (Material.containedButton Material.defaultPalette)
            { text = label
            , onPress =
                if enabled then
                    Just onClick

                else
                    Nothing
            }
