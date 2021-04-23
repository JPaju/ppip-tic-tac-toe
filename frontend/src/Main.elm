module Main exposing (..)

import Browser
import Element exposing (Attribute, Element, centerX, centerY, column, el, fill, height, mouseOver, none, pointer, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as Json exposing (decodeValue, int)
import Json.Decode.Pipeline exposing (required)
import Matrix exposing (Coordinate, Matrix)
import Ports
import Ui



---- GAME ----


type Mark
    = X
    | O


type alias Board =
    Matrix (Maybe Mark)


type GameResult
    = Tie
    | Won Mark


type Game
    = Finished Board GameResult
    | OnGoing
        { board : Board
        , turn : Mark
        }


getBoard : Game -> Board
getBoard game =
    case game of
        OnGoing { board } ->
            board

        Finished board _ ->
            board



---- MODEL ----


type alias Model =
    { game : Game }


initGame : Int -> Game
initGame boardSize =
    OnGoing
        { board = Matrix.square boardSize (always Nothing)
        , turn = X
        }


init : ( Model, Cmd Msg )
init =
    ( { game = initGame 5 }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = BoardClicked Matrix.Coordinate
    | ResetGame
    | ReceivedMark Mark Coordinate
    | CommunicationError String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BoardClicked coordinate ->
            ( { model | game = updateGame model.game coordinate }
            , Ports.sendCoordinate coordinate
            )

        ResetGame ->
            ( { model | game = initGame 5 }, Cmd.none )

        ReceivedMark mark coordinate ->
            ( { model | game = updateGame model.game coordinate }, Cmd.none )

        CommunicationError desc ->
            Debug.log "Invalid data:" ( model, Cmd.none )


updateGame : Game -> Coordinate -> Game
updateGame game coordinate =
    case game of
        OnGoing { board, turn } ->
            OnGoing
                { turn = changeTurn turn
                , board = placeMark turn coordinate board
                }

        (Finished _ _) as finished ->
            finished


placeMark : Mark -> Coordinate -> Board -> Board
placeMark mark coordinate board =
    Matrix.set coordinate (Just mark) board


changeTurn : Mark -> Mark
changeTurn mark =
    case mark of
        X ->
            O

        O ->
            X



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [ height fill, width fill ] <|
        column [ centerX, centerY, spacing 50 ]
            [ gameHeader model.game
            , getBoard model.game |> viewBoard []
            , Ui.button [ centerX ] { label = "Reset", enabled = True, onClick = ResetGame }
            ]


gameHeader : Game -> Element msg
gameHeader game =
    row [ centerX, Font.size 38 ] <|
        case game of
            OnGoing { turn } ->
                [ el [] (text "Turn: "), viewMark turn ]

            Finished _ result ->
                case result of
                    Won winner ->
                        [ viewMark winner, text " won!" ]

                    Tie ->
                        [ el [] (text "Tie!") ]


viewBoard : List (Attribute Msg) -> Board -> Element Msg
viewBoard attributes board =
    column attributes
        (board
            |> Matrix.getRowsWithCoordinates
            |> List.map viewRow
        )


viewRow : List ( Maybe Mark, Coordinate ) -> Element Msg
viewRow columns =
    columns
        |> List.map boardCell
        |> row []


boardCell : ( Maybe Mark, Coordinate ) -> Element Msg
boardCell ( maybeMark, coord ) =
    let
        size =
            px 100

        cellStyles =
            [ height size, width size, Border.width 1 ]
    in
    case maybeMark of
        Just mark ->
            markedCell cellStyles mark

        Nothing ->
            emptyCell cellStyles coord


emptyCell : List (Attribute Msg) -> Coordinate -> Element Msg
emptyCell attributes coordinate =
    Input.button (pointer :: mouseOver [ Background.color Ui.grey ] :: attributes)
        { onPress = Just (BoardClicked coordinate)
        , label = none
        }


markedCell : List (Attribute msg) -> Mark -> Element msg
markedCell attributes mark =
    Input.button (Ui.notAllowed :: attributes)
        { onPress = Nothing
        , label = viewMark mark
        }


viewMark : Mark -> Element msg
viewMark mark =
    let
        icon =
            case mark of
                X ->
                    '❌'

                O ->
                    '⭕'
    in
    icon
        |> String.fromChar
        |> text
        |> el [ centerY, centerX, Font.size 36 ]



---- DECODERS ---


decodeReceivedCoordinate : Json.Value -> Msg
decodeReceivedCoordinate json =
    case decodeValue testDecoder json of
        Ok value ->
            value

        Err error ->
            CommunicationError "Whoops"


testDecoder : Json.Decoder Msg
testDecoder =
    Json.succeed ReceivedMark
        |> required "sign" markDecoder
        |> required "coordinate" coordinateDecoder


coordinateDecoder : Json.Decoder Coordinate
coordinateDecoder =
    Json.succeed Tuple.pair
        |> required "x" Json.int
        |> required "y" Json.int


markDecoder : Json.Decoder Mark
markDecoder =
    Json.string
        |> Json.andThen
            (\markString ->
                case String.toUpper markString of
                    "X" ->
                        Json.succeed X

                    "O" ->
                        Json.succeed O

                    _ ->
                        Json.fail ("Invalid sign: " ++ markString)
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveCoordinate decodeReceivedCoordinate



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = always init
        , update = update
        , subscriptions = subscriptions
        }
