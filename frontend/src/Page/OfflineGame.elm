module Page.OfflineGame exposing (Model, Msg, init, update, view)

import Element exposing (Element, centerX, centerY, column, el, inFront, row, spacing, text)
import TicTacToe.Board as Board exposing (Board)
import TicTacToe.Coordinate exposing (Coordinate)
import TicTacToe.Matrix as Matrix
import TicTacToe.Sign as Sign exposing (Sign(..))
import Ui



-- TODO: Let user choose the size of the board


type Game
    = Finished Board GameResult
    | OnGoing
        { board : Board
        , hasTurn : Sign
        }


type GameResult
    = Tie
    | Won Sign


type Msg
    = BoardClicked Coordinate
    | ResetGame


type alias Model =
    Game


initGame : Int -> Game
initGame boardSize =
    OnGoing
        { board = Board.create (Matrix.Dimensions boardSize boardSize) []
        , hasTurn = X
        }


init : Int -> Model
init boardSize =
    initGame boardSize



---- UPDATE ----


update : Msg -> Model -> Model
update msg game =
    case msg of
        BoardClicked coordinate ->
            case game of
                OnGoing { board, hasTurn } ->
                    let
                        newBoard =
                            Board.placeMark { sign = hasTurn, location = coordinate } board
                    in
                    updateGame newBoard hasTurn

                (Finished _ _) as finished ->
                    finished

        ResetGame ->
            initGame 5


updateGame : Board -> Sign -> Game
updateGame board hasTurn =
    let
        boardFull =
            Board.isFull board
    in
    if boardFull then
        Finished board Tie

    else
        case Board.checkWinner board of
            Just winner ->
                Finished board (Won winner)

            Nothing ->
                OnGoing
                    { hasTurn = Sign.change hasTurn
                    , board = board
                    }



---- VIEW ----


view : Model -> Element Msg
view game =
    column [ centerX, centerY, spacing 50 ]
        [ gameHeader game
        , case game of
            OnGoing { board } ->
                Board.view [] BoardClicked board

            Finished board result ->
                Board.view [ inFront (gameEndedText result |> Board.overlay) ] BoardClicked board
        , Ui.button [ centerX ] { label = "Reset", enabled = True, onClick = ResetGame }
        ]


gameHeader : Game -> Element msg
gameHeader game =
    row (centerX :: Ui.pageHeaderStyle) <|
        case game of
            OnGoing { hasTurn } ->
                [ el [] (text "Turn: "), Sign.view [] hasTurn ]

            Finished _ result ->
                case result of
                    Won winner ->
                        [ Sign.view [] winner, text " won!" ]

                    Tie ->
                        [ el [] (text "Tie!") ]


gameEndedText : GameResult -> String
gameEndedText result =
    let
        resultText =
            case result of
                Tie ->
                    "It's a Tie"

                Won X ->
                    "Crosses won!"

                Won O ->
                    "Circles were victorious"
    in
    "Game ended! " ++ resultText
