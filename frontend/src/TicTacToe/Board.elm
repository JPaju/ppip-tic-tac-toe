module TicTacToe.Board exposing
    ( Board
    , create
    , decoder
    , placeMark
    , view
    )

import Dict
import Element exposing (Attribute, Element, column, height, mouseOver, none, pointer, px, row, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Json.Decode as Decode exposing (Decoder, int, list)
import Json.Decode.Pipeline exposing (required)
import TicTacToe.Coordinate as Coordinate exposing (Coordinate)
import TicTacToe.Mark as Mark exposing (Mark)
import TicTacToe.Matrix as Matrix exposing (Matrix)
import TicTacToe.Sign as Sign exposing (Sign(..))
import Ui
import Util


type Board
    = Board (Matrix (Maybe Sign))


create : Matrix.Dimensions -> List Mark -> Board
create dimensions marks =
    marks
        |> List.map (\{ location, sign } -> ( location, sign ))
        |> Matrix.fromList dimensions
        |> Board


placeMark : Mark -> Board -> Board
placeMark { sign, location } (Board board) =
    Matrix.set location (Just sign) board
        |> Board


---- VIEW ----


view : List (Attribute msg) -> (Coordinate -> msg) -> Board -> Element msg
view attributes toMsg (Board board) =
    column attributes
        (board
            |> Matrix.getRowsWithCoordinates
            |> List.map viewRow
            |> List.map (Element.map toMsg)
        )



---- DECODE ----


decoder : Decoder Board
decoder =
    let
        dimensionDecoder =
            Decode.succeed Matrix.Dimensions
                |> required "height" int
                |> required "width" int
    in
    Decode.succeed create
        |> required "dimensions" dimensionDecoder
        |> required "marks" (list Mark.decoder)



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
        , label = Sign.view [] sign
        }
