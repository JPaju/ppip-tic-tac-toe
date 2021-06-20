module Page.OnlineGame exposing (Model(..), Msg, init, subscriptions, update, view)

import Element exposing (Attribute, Element, alignRight, alignTop, centerX, centerY, column, el, fill, inFront, none, row, scale, spacing, text, width)
import Element.Font as Font
import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Decode.Pipeline exposing (required)
import Ports
import TicTacToe.Board as Board exposing (Board)
import TicTacToe.Coordinate exposing (Coordinate)
import TicTacToe.Mark as Mark exposing (Mark)
import TicTacToe.Sign as Sign exposing (Sign(..))
import Ui
import Util


type GameResult
    = Won
    | Lost
    | Draw


type alias Game =
    { board : Board
    , yourSign : Sign
    , hasTurn : Sign
    }


type Model
    = Connecting
    | SearchingOpponent
    | GameOn Game
    | FinishedGame Board GameResult
    | Error String


type Msg
    = NewGameClicked
    | CellClicked Coordinate
    | ConnectedToServer
    | GameStarted Game
    | ReceivedMark Mark
    | GameEnded Board GameResult
    | CommunicationError String


init : ( Model, Cmd Msg )
init =
    ( Connecting
    , Ports.connectionPort "connect"
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameStarted game ->
            ( GameOn game, Cmd.none )

        CellClicked coordinate ->
            case model of
                GameOn ({ yourSign, hasTurn, board } as game) ->
                    if hasTurn == yourSign then
                        ( GameOn
                            { game
                                | hasTurn = Sign.change hasTurn
                                , board = Board.placeMark (Mark yourSign coordinate) board
                            }
                        , Mark yourSign coordinate
                            |> Mark.encoder
                            |> Ports.sendMark
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReceivedMark mark ->
            case model of
                GameOn ({ board } as game) ->
                    ( GameOn
                        { game
                            | hasTurn = Sign.change mark.sign
                            , board = Board.placeMark mark board
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        CommunicationError err ->
            ( Error err, Cmd.none )

        ConnectedToServer ->
            ( SearchingOpponent, Cmd.none )

        GameEnded board result ->
            ( FinishedGame board result, Cmd.none )

        NewGameClicked ->
            init



---- VIEW ----


view : Model -> Element Msg
view model =
    column [ centerX, centerY, spacing 50 ] <|
        case model of
            Connecting ->
                viewLoading [ centerX ] "Connecting to the server"

            SearchingOpponent ->
                viewLoading [ centerX ] "Searching opponents"

            GameOn { board, yourSign, hasTurn } ->
                let
                    yourTurn =
                        yourSign == hasTurn

                    overlay =
                        if yourTurn then
                            none

                        else
                            Board.overlay "Waiting for the opponent to make a move"
                in
                [ viewTurn yourTurn
                , board |> Board.view [ inFront overlay ] CellClicked
                , viewMarks yourSign
                ]

            FinishedGame board winner ->
                let
                    winnerText =
                        case winner of
                            Won ->
                                "You Won! 🏆"

                            Lost ->
                                "You lost! 😞"

                            Draw ->
                                "It's a Draw! 🤷"
                in
                [ el (centerX :: alignTop :: Ui.pageHeaderStyle) (text "Game ended")
                , board |> Board.view [ inFront (Board.overlay winnerText) ] CellClicked
                , Ui.button [ centerX ] { label = "New game", onClick = NewGameClicked, enabled = True }
                ]

            Error err ->
                Debug.log ("Error: " ++ err)
                    [ el (centerX :: centerY :: Ui.pageHeaderStyle) (text "Whoops... Something went wrong")
                    , el [ centerX, Font.size 100 ] (text "💥")
                    ]


viewLoading : List (Attribute msg) -> String -> List (Element msg)
viewLoading attributes message =
    [ el (Ui.pageHeaderStyle ++ attributes) (text message)
    , Ui.loadingSpinner [ centerX ]
    ]


viewTurn : Bool -> Element Msg
viewTurn yourTurn =
    let
        headerText =
            if yourTurn then
                "Your turn"

            else
                "Opponent's turn"
    in
    el (centerX :: alignTop :: Ui.pageHeaderStyle) (text headerText)


viewMarks : Sign -> Element Msg
viewMarks yourSign =
    let
        markRow label sign =
            row [ width fill ]
                [ el [ width fill ] (text label)
                , Sign.view [ scale 0.5, alignRight ] sign
                ]
    in
    column [ centerX ]
        [ markRow "You: " yourSign
        , markRow "Opponent: " (Sign.change yourSign)
        ]


subscriptions : Sub Msg
subscriptions =
    Ports.receiveGameMessage decodeGameMessage



---- DECODE ----


decodeGameMessage : Decode.Value -> Msg
decodeGameMessage json =
    Decode.decodeValue gameMsgDecoder json
        |> Util.foldResult (Decode.errorToString >> CommunicationError) identity


gameMsgDecoder : Decoder Msg
gameMsgDecoder =
    Decode.oneOf
        [ connectedDecoder
        , gameStartedDecoder
        , newMarkDecoder
        , gameEndedDecoder
        ]


connectedDecoder : Decoder Msg
connectedDecoder =
    field "waiting" string
        |> Decode.andThen (\_ -> Decode.succeed ConnectedToServer)


gameStartedDecoder : Decoder Msg
gameStartedDecoder =
    let
        gameDecoder =
            Decode.succeed Game
                |> required "board" Board.decoder
                |> required "yourSign" Sign.decoder
                |> required "nowHasTurn" Sign.decoder
    in
    Decode.map GameStarted gameDecoder


newMarkDecoder : Decoder Msg
newMarkDecoder =
    Decode.succeed ReceivedMark
        |> required "newMark" Mark.decoder


gameEndedDecoder : Decoder Msg
gameEndedDecoder =
    Decode.succeed GameEnded
        |> required "board" Board.decoder
        |> required "result" resultDecoder


resultDecoder : Decoder GameResult
resultDecoder =
    string
        |> Decode.andThen
            (\resultString ->
                case String.toUpper resultString of
                    "WON" ->
                        Decode.succeed Won

                    "LOST" ->
                        Decode.succeed Lost

                    "DRAW" ->
                        Decode.succeed Draw

                    _ ->
                        Decode.fail ("Invalid result: " ++ resultString)
            )
