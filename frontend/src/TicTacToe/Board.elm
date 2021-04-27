module TicTacToe.Board exposing
    ( Board
    , checkWinner
    , create
    , decoder
    , isFull
    , marksInRowRequiredToWin
    , overlay
    , placeMark
    , view
    )

import Element exposing (Attribute, Element, alpha, centerX, centerY, column, el, fill, height, mouseOver, none, paragraph, pointer, px, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode exposing (Decoder, int, list)
import Json.Decode.Pipeline exposing (required)
import TicTacToe.Coordinate exposing (Coordinate)
import TicTacToe.Mark as Mark exposing (Mark)
import TicTacToe.Matrix as Matrix exposing (Dimensions, Matrix)
import TicTacToe.Sign as Sign exposing (Sign(..))
import Ui
import Util


type Board
    = Board (Matrix (Maybe Sign))



---- CREATE & MODIFY ----


create : Dimensions -> List Mark -> Board
create dimensions marks =
    marks
        |> List.map (\{ location, sign } -> ( location, sign ))
        |> Matrix.fromList dimensions
        |> Board


placeMark : Mark -> Board -> Board
placeMark { sign, location } (Board board) =
    Matrix.set location (Just sign) board
        |> Board



---- RULES ----


isFull : Board -> Bool
isFull board =
    let
        matrix =
            getMatrix board

        markCount =
            matrix
                |> Matrix.getAllElements
                |> List.filter Util.hasValue
                |> List.length
    in
    markCount == Matrix.getCapacity matrix


checkWinner : Int -> Board -> Maybe Sign
checkWinner neededToWin board =
    let
        isWinner =
            \sign -> hasMarksInRow sign neededToWin board
    in
    if isWinner X then
        Just X

    else if isWinner O then
        Just O

    else
        Nothing


marksInRowRequiredToWin : Dimensions -> Int
marksInRowRequiredToWin { height, width } =
    let
        smallerDimension =
            min height width
    in
    if smallerDimension == 3 then
        3

    else if smallerDimension < 6 then
        4

    else
        5



---- VIEW ----


view : List (Attribute msg) -> (Coordinate -> msg) -> Board -> Element msg
view attributes toMsg (Board board) =
    column attributes
        (board
            |> Matrix.getRowsWithCoordinates
            |> List.map viewRow
            |> List.map (Element.map toMsg)
        )


overlay : String -> Element msg
overlay label =
    el
        [ height fill
        , width fill
        , Background.color Ui.grey
        , alpha 0.8
        ]
        (paragraph
            [ centerX, centerY, Font.size 28, Font.center ]
            [ text label ]
        )



---- DECODE ----


decoder : Decoder Board
decoder =
    let
        dimensionDecoder =
            Decode.succeed Dimensions
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


getMatrix : Board -> Matrix (Maybe Sign)
getMatrix (Board matrix) =
    matrix


hasMarksInRow : Sign -> Int -> Board -> Bool
hasMarksInRow signToCheck count board =
    let
        isCorrectSign =
            \sign -> sign == signToCheck

        matrix =
            getMatrix board

        vertically =
            Matrix.nConsecutiveVertically count (Maybe.map isCorrectSign >> Maybe.withDefault False)

        horizontally =
            Matrix.nConsecutiveHorizontally count (Maybe.map isCorrectSign >> Maybe.withDefault False)

        diagonally =
            Matrix.nConsecutiveDiagonally count (Maybe.map isCorrectSign >> Maybe.withDefault False)
    in
    vertically matrix || horizontally matrix || diagonally matrix
