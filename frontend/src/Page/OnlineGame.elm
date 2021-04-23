module Page.OnlineGame exposing (Model(..), Msg, init, subscriptions, update, view)

import Element exposing (Element, alignTop, centerX, centerY, column, el, spacing, text)
import Element.Font exposing (center)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Matrix exposing (Coordinate, Matrix)
import Ports exposing (sendCoordinate)
import Process
import Task
import TicTacToe exposing (Board, Mark, Sign(..), changeTurn, placeMark)
import Ui


type Msg
    = GameStarted
    | BoardClicked Coordinate
    | ReceivedMark Mark
    | CommunicationError String


type Winner
    = You
    | Opponent


type alias Game =
    { board : Board
    , yourSign : Sign
    , hasTurn : Sign
    }


type Model
    = SearchingOpponent
    | GameOn Game
    | GameEnded Board Winner


boardSize =
    5


loadingDelaySeconds =
    2 * 1000


init : ( Model, Cmd Msg )
init =
    ( SearchingOpponent, Process.sleep loadingDelaySeconds |> Task.perform (always GameStarted) )


initGame : Game
initGame =
    { board = TicTacToe.initBoard boardSize
    , yourSign = O
    , hasTurn = O
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GameStarted, _ ) ->
            ( GameOn initGame, Cmd.none )

        ( BoardClicked coordinate, GameOn ({ yourSign, hasTurn, board } as game) ) ->
            if hasTurn == yourSign then
                ( GameOn
                    { game
                        | hasTurn = TicTacToe.changeTurn hasTurn
                        , board = TicTacToe.placeMark { sign = yourSign, location = coordinate } board
                    }
                , Ports.sendCoordinate coordinate
                )

            else
                ( model, Cmd.none )

        ( ReceivedMark mark, GameOn ({ hasTurn, board } as game) ) ->
            ( GameOn
                { game
                    | hasTurn = changeTurn hasTurn
                    , board = TicTacToe.placeMark mark board
                }
            , Cmd.none
            )

        ( CommunicationError err, _ ) ->
            ( Debug.log ("Error: " ++ err) model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Element Msg
view model =
    column [ centerX, centerY, spacing 50 ] <|
        case model of
            SearchingOpponent ->
                [ el [ centerX, alignTop ] (text "Searching opponents")
                , el [ centerX, centerY ] (text "Wait a second or two")
                ]

            GameOn _ ->
                [ el [ centerX, alignTop ] (text "Now Playing")
                , el [ centerX, centerY ] (text "Game started!")
                ]

            GameEnded _ _ ->
                [ el [ centerX, alignTop ] (text "Header")
                , el [ centerX, centerY ] (text "Game ended")
                ]


subscriptions : Sub Msg
subscriptions =
    Ports.receiveCoordinate decodeReceivedMark



---- DECODER ----


decodeReceivedMark : Decode.Value -> Msg
decodeReceivedMark json =
    case Decode.decodeValue markDecoder json of
        Ok mark ->
            ReceivedMark mark

        Err error ->
            CommunicationError (Decode.errorToString error)


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
