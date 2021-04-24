module Page.OnlineGame exposing (Model(..), Msg, init, subscriptions, update, view)

import Element exposing (Attribute, Element, alignRight, alignTop, alpha, centerX, centerY, column, el, fill, height, inFront, none, paragraph, row, scale, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Decode.Pipeline exposing (required)
import Matrix exposing (Coordinate)
import Ports
import TicTacToe exposing (Board, Mark, Sign(..), boardDecoder, changeTurn, markDecoder, markEncoder, signDecoder, viewSign)
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
    , Ports.debugPort "connect"
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
                                | hasTurn = TicTacToe.changeTurn hasTurn
                                , board = TicTacToe.placeMark (Mark yourSign coordinate) board
                            }
                        , Mark yourSign coordinate
                            |> markEncoder
                            |> Ports.sendMark
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReceivedMark mark ->
            case model of
                GameOn ({ hasTurn, board } as game) ->
                    ( GameOn
                        { game
                            | hasTurn = changeTurn hasTurn
                            , board = TicTacToe.placeMark mark board
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        CommunicationError err ->
            ( Error err, Cmd.none )

        ConnectedToServer ->
            ( SearchingOpponent, Ports.debugPort "searchOpponent" )

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
                            boardOverlay "Waiting for the opponent to make a move"
                in
                [ viewTurn yourTurn
                , board |> TicTacToe.viewBoard [ inFront overlay ] CellClicked
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
                , board |> TicTacToe.viewBoard [ inFront (boardOverlay winnerText) ] CellClicked
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
                , viewSign [ scale 0.5, alignRight ] sign
                ]
    in
    column [ centerX ]
        [ markRow "You: " yourSign
        , markRow "Opponent: " (changeTurn yourSign)
        ]


boardOverlay : String -> Element msg
boardOverlay label =
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
                |> required "board" boardDecoder
                |> required "yourSign" signDecoder
                |> required "nowHasTurn" signDecoder
    in
    Decode.map GameStarted gameDecoder


newMarkDecoder : Decoder Msg
newMarkDecoder =
    Decode.succeed ReceivedMark
        |> required "newMark" markDecoder


gameEndedDecoder : Decoder Msg
gameEndedDecoder =
    Decode.succeed GameEnded
        |> required "board" boardDecoder
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

                    "TIE" ->
                        Decode.succeed Draw

                    _ ->
                        Decode.fail ("Invalid result: " ++ resultString)
            )
