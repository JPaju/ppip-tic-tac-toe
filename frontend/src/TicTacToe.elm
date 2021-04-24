module TicTacToe exposing
    ( Board
    , Mark
    , Sign(..)
    , boardDecoder
    , changeTurn
    , createBoard
    , markDecoder
    , markEncoder
    , placeMark
    , signDecoder
    , viewBoard
    , viewSign
    )

import Element exposing (Attribute, Element, centerX, centerY, column, el, height, mouseOver, none, pointer, px, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (mark)
import Json.Decode as Decode exposing (Decoder, int, list)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
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


createBoard : Matrix.Dimensions -> List Mark -> Board
createBoard dimensions marks =
    marks
        |> List.map (\{ location, sign } -> ( location, sign ))
        |> Matrix.fromList dimensions
        |> Board


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


viewSign : List (Attribute msg) -> Sign -> Element msg
viewSign attributes sign =
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
        |> el (attributes ++ [ centerY, centerX, Font.size 36 ])



---- ENCODE ----


markEncoder : Mark -> Encode.Value
markEncoder { sign, location } =
    Encode.object
        [ ( "coordinate", coordinateEncoder location )
        , ( "sign", signEncoder sign )
        ]


coordinateEncoder : Coordinate -> Encode.Value
coordinateEncoder ( x, y ) =
    Encode.object
        [ ( "x", Encode.int x )
        , ( "y", Encode.int y )
        ]


signEncoder : Sign -> Decode.Value
signEncoder sign =
    Encode.string <|
        case sign of
            X ->
                "X"

            O ->
                "O"



---- DECODE ----


boardDecoder : Decoder Board
boardDecoder =
    let
        dimensionsDecoder =
            Decode.succeed Matrix.Dimensions
                |> required "height" int
                |> required "width" int

        marksDecoder =
            list markDecoder
    in
    Decode.succeed createBoard
        |> required "dimensions" dimensionsDecoder
        |> required "marks" marksDecoder



-- Decode.succeed (Matrix.create (Matrix.Dimensions 3 5) (always Nothing))
--     |> Decode.map Board


markDecoder : Decoder Mark
markDecoder =
    Decode.succeed Mark
        |> required "sign" signDecoder
        |> required "coordinate" coordinateDecoder


coordinateDecoder : Decoder Coordinate
coordinateDecoder =
    Decode.succeed Tuple.pair
        |> required "x" Decode.int
        |> required "y" Decode.int


signDecoder : Decoder Sign
signDecoder =
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
        , label = viewSign [] sign
        }
