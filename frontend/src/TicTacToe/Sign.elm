module TicTacToe.Sign exposing (Sign(..), change, decoder, encoder, isCircle, isCross, toString, view)

import Element exposing (Attribute, Element, centerX, centerY, el, text)
import Element.Font as Font
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Sign
    = X
    | O


isCross : Sign -> Bool
isCross sign =
    sign == X


isCircle : Sign -> Bool
isCircle sign =
    sign == O


change : Sign -> Sign
change sign =
    case sign of
        X ->
            O

        O ->
            X


encoder : Sign -> Decode.Value
encoder sign =
    Encode.string <|
        case sign of
            X ->
                "X"

            O ->
                "O"


decoder : Decoder Sign
decoder =
    Decode.string
        |> Decode.andThen
            (\signString ->
                case String.toUpper signString of
                    "X" ->
                        Decode.succeed X

                    "O" ->
                        Decode.succeed O

                    _ ->
                        Decode.fail ("Invalid sign: " ++ signString)
            )


toString : Sign -> String
toString sign =
    String.fromChar <|
        case sign of
            X ->
                '❌'

            O ->
                '⭕'


view : List (Attribute msg) -> Sign -> Element msg
view attributes sign =
    sign
        |> toString
        |> text
        |> el (attributes ++ [ centerY, centerX, Font.size 36 ])
