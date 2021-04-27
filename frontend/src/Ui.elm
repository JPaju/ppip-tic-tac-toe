module Ui exposing (SliderConfig, black, blue, button, grey, intSlider, loadingSpinner, notAllowed, pageHeaderStyle, red, select, white)

import Array
import Element exposing (Attribute, Color, Element, centerX, el, height, px, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Util
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



---- TEXT ----


pageHeaderStyle : List (Attribute msg)
pageHeaderStyle =
    [ Font.semiBold, Font.size 32 ]



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



---- SELECT ----


select : List a -> Maybe a -> (a -> String) -> (a -> msg) -> Element msg
select values maybeSelected toLabel toMsg =
    let
        selectedIndex =
            Maybe.andThen (Util.flip Util.indexOf values) maybeSelected

        selectConf =
            { selected = selectedIndex
            , options =
                values
                    |> List.map
                        (\value ->
                            { text = toLabel value
                            , icon = always Element.none
                            }
                        )
            , onSelect = \index -> Array.get index (Array.fromList values) |> Maybe.map toMsg
            }
    in
    selectConf
        |> Widget.select
        |> Widget.buttonRow
            { elementRow = Material.buttonRow
            , content = Material.toggleButton Material.defaultPalette
            }



---- SLIDER ----


type alias SliderConfig number msg =
    { onChange : number -> msg
    , label : String
    , value : number
    , min : number
    , max : number
    , step : Maybe number
    }


intSlider : List (Attribute msg) -> SliderConfig Int msg -> Element msg
intSlider attributes { onChange, label, value, min, max, step } =
    let
        thumb =
            Input.thumb [ height (px 16), width (px 16), Border.rounded 8, Background.color blue ]
    in
    Input.slider
        (attributes
            ++ [ Element.height (Element.px 30)
               , Element.behindContent
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 2)
                        , Element.centerY
                        , Background.color grey
                        , Border.rounded 2
                        ]
                        Element.none
                    )
               ]
        )
        { onChange = round >> onChange
        , label =
            Input.labelAbove [ centerX, Font.size 16 ]
                (text label)
        , min = toFloat min
        , max = toFloat max
        , step = Maybe.map toFloat step
        , value = toFloat value
        , thumb = thumb
        }



---- SPINNER ----


spinnerStyles : Widget.ProgressIndicatorStyle msg
spinnerStyles =
    Material.progressIndicator Material.defaultPalette


loadingSpinner : List (Attribute msg) -> Element msg
loadingSpinner attrs =
    Widget.circularProgressIndicator spinnerStyles Nothing
        |> el attrs
