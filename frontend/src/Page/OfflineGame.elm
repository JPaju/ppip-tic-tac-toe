module Page.OfflineGame exposing (..)

import Element exposing (Element, centerX, centerY, column, el, row, spacing, text)
import Element.Font as Font
import Matrix exposing (Coordinate)
import TicTacToe exposing (Board, Sign(..), changeTurn, createBoard, placeMark)
import Ui


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


getBoard : Game -> Board
getBoard game =
    case game of
        OnGoing { board } ->
            board

        Finished board _ ->
            board


type alias Model =
    { game : Game }


initGame : Int -> Game
initGame boardSize =
    OnGoing
        { board = createBoard (Matrix.Dimensions boardSize boardSize) []
        , hasTurn = X
        }


init : Int -> Model
init boardSize =
    { game = initGame boardSize }


update : Msg -> Model -> Model
update msg model =
    case msg of
        BoardClicked coordinate ->
            { model | game = updateGame model.game coordinate }

        ResetGame ->
            { model | game = initGame 5 }


updateGame : Game -> Coordinate -> Game
updateGame game coordinate =
    case game of
        OnGoing { board, hasTurn } ->
            OnGoing
                { hasTurn = changeTurn hasTurn
                , board = placeMark { sign = hasTurn, location = coordinate } board
                }

        (Finished _ _) as finished ->
            finished


view : Model -> Element Msg
view model =
    column [ centerX, centerY, spacing 50 ]
        [ gameHeader model.game
        , getBoard model.game |> TicTacToe.viewBoard [] BoardClicked
        , Ui.button [ centerX ] { label = "Reset", enabled = True, onClick = ResetGame }
        ]


gameHeader : Game -> Element msg
gameHeader game =
    row (centerX :: Ui.pageHeaderStyle) <|
        case game of
            OnGoing { hasTurn } ->
                [ el [] (text "Turn: "), TicTacToe.viewSign [] hasTurn ]

            Finished _ result ->
                case result of
                    Won winner ->
                        [ TicTacToe.viewSign [] winner, text " won!" ]

                    Tie ->
                        [ el [] (text "Tie!") ]