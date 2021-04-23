module TicTacToe exposing (Board, Mark, Sign(..), changeTurn, initBoard, placeMark, viewBoard, viewSign)

import Element exposing (Attribute, Element, centerX, centerY, column, el, height, mouseOver, none, pointer, px, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Matrix exposing (Coordinate, Matrix)
import Ui


type Sign
    = X
    | O


type alias Mark =
    { sign : Sign
    , location : Coordinate
    }


type Board
    = Board (Matrix (Maybe Sign))


initBoard : Int -> Board
initBoard boardSize =
    Matrix.square boardSize (always Nothing) |> Board


placeMark : Mark -> Board -> Board
placeMark { sign, location } (Board board) =
    Matrix.set location (Just sign) board
        |> Board


changeTurn : Sign -> Sign
changeTurn sign =
    case sign of
        X ->
            O

        O ->
            X



---- VIEW ----


viewBoard : List (Attribute msg) -> (Coordinate -> msg) -> Board -> Element msg
viewBoard attributes toMsg (Board board) =
    column attributes
        (board
            |> Matrix.getRowsWithCoordinates
            |> List.map viewRow
            |> List.map (Element.map toMsg)
        )


viewSign : Sign -> Element msg
viewSign sign =
    let
        icon =
            case sign of
                X ->
                    '❌'

                O ->
                    '⭕'
    in
    icon
        |> String.fromChar
        |> text
        |> el [ centerY, centerX, Font.size 36 ]



---- PRIVATE FUNCTIONS ----


viewRow : List ( Maybe Sign, Coordinate ) -> Element Coordinate
viewRow columns =
    columns
        |> List.map boardCell
        |> row []


boardCell : ( Maybe Sign, Coordinate ) -> Element Coordinate
boardCell ( maybeSign, coord ) =
    let
        size =
            px 100

        cellStyles =
            [ height size, width size, Border.width 1 ]
    in
    case maybeSign of
        Just sign ->
            markedCell cellStyles sign

        Nothing ->
            emptyCell cellStyles coord


emptyCell : List (Attribute Coordinate) -> Coordinate -> Element Coordinate
emptyCell attributes coordinate =
    Input.button (pointer :: mouseOver [ Background.color Ui.grey ] :: attributes)
        { onPress = Just coordinate
        , label = none
        }


markedCell : List (Attribute msg) -> Sign -> Element msg
markedCell attributes sign =
    Input.button (Ui.notAllowed :: attributes)
        { onPress = Nothing
        , label = viewSign sign
        }
