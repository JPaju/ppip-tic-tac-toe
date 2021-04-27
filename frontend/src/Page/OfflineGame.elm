module Page.OfflineGame exposing (Model, Msg, init, update, view)

import Element exposing (Element, alignTop, centerX, centerY, column, el, fill, inFront, maximum, minimum, row, spacing, text, width)
import TicTacToe.Board as Board exposing (Board)
import TicTacToe.Coordinate exposing (Coordinate)
import TicTacToe.Matrix exposing (Dimensions)
import TicTacToe.Sign as Sign exposing (Sign(..))
import Ui
import Util exposing (average)


type alias BoardSettings =
    { startingSign : Sign
    , boardDimensions : Dimensions
    }


type Game
    = Over Board GameResult
    | Playing
        { board : Board
        , hasTurn : Sign
        }


type GameResult
    = Draw
    | Won Sign


type Model
    = StartPage BoardSettings
    | Game Game


type Msg
    = StartingSignChanged Sign
    | BoardDimensionsChanged Dimensions
    | StartGameClicked
    | BoardClicked Coordinate
    | ResetGameClicked


minBoardSize =
    3


maxBoardSize =
    15


init : Model
init =
    let
        boardSize =
            [ minBoardSize, maxBoardSize ]
                |> average
                |> round
    in
    StartPage
        { startingSign = X
        , boardDimensions = { height = boardSize, width = boardSize }
        }



---- UPDATE ----


update : Msg -> Model -> Model
update msg model =
    case model of
        StartPage settings ->
            case msg of
                StartingSignChanged sign ->
                    StartPage { settings | startingSign = sign }

                BoardDimensionsChanged dimensions ->
                    StartPage { settings | boardDimensions = dimensions }

                StartGameClicked ->
                    Playing
                        { board = Board.create settings.boardDimensions []
                        , hasTurn = settings.startingSign
                        }
                        |> Game

                _ ->
                    StartPage settings

        Game game ->
            case msg of
                ResetGameClicked ->
                    init

                BoardClicked coordinate ->
                    Game (updateGame coordinate game)

                _ ->
                    model


updateGame : Coordinate -> Game -> Game
updateGame clickedCoordinate game =
    case game of
        Playing { board, hasTurn } ->
            let
                newBoard =
                    Board.placeMark { sign = hasTurn, location = clickedCoordinate } board

                boardFull =
                    Board.isFull newBoard
            in
            if boardFull then
                Over newBoard Draw

            else
                case Board.checkWinner newBoard of
                    Just winner ->
                        Over newBoard (Won winner)

                    Nothing ->
                        Playing
                            { hasTurn = Sign.change hasTurn
                            , board = newBoard
                            }

        (Over _ _) as over ->
            over



---- VIEW ----


view : Model -> Element Msg
view model =
    column [ centerX, centerY, spacing 70 ] <|
        case model of
            StartPage { boardDimensions, startingSign } ->
                [ el (centerX :: alignTop :: Ui.pageHeaderStyle) (text "Game settings")
                , boardDimensionSliders boardDimensions BoardDimensionsChanged
                , selectStartSign startingSign
                , Ui.button [ centerX ] { label = "Start game", enabled = True, onClick = StartGameClicked }
                ]

            Game game ->
                [ gameHeader game
                , case game of
                    Playing { board } ->
                        Board.view [] BoardClicked board

                    Over board result ->
                        Board.view [ inFront (gameEndedText result |> Board.overlay) ] BoardClicked board
                , Ui.button [ centerX ] { label = "Reset", enabled = True, onClick = ResetGameClicked }
                ]


boardDimensionSliders : Dimensions -> (Dimensions -> msg) -> Element msg
boardDimensionSliders dimensions toMsg =
    let
        createSlider label value onChange =
            Ui.intSlider []
                { onChange = onChange
                , label = label ++ " " ++ String.fromInt value
                , value = value
                , min = minBoardSize
                , max = maxBoardSize
                , step = Just 1
                }
    in
    column
        [ width (fill |> minimum 200 |> maximum 300)
        , spacing 30
        ]
        [ createSlider "Height" dimensions.height (\newValue -> { dimensions | height = newValue } |> toMsg)
        , createSlider "Width" dimensions.width (\newValue -> { dimensions | width = newValue } |> toMsg)
        ]


selectStartSign : Sign -> Element Msg
selectStartSign selectedSign =
    column [ centerX, spacing 20 ]
        [ el [] (text "Select starting sign ")
        , el [ centerX ] (Ui.select [ X, O ] (Just selectedSign) Sign.toString StartingSignChanged)
        ]


gameHeader : Game -> Element msg
gameHeader game =
    row (centerX :: Ui.pageHeaderStyle) <|
        case game of
            Playing { hasTurn } ->
                [ el [] (text "Turn: "), Sign.view [] hasTurn ]

            Over _ result ->
                case result of
                    Won winner ->
                        [ Sign.view [] winner, text " won!" ]

                    Draw ->
                        [ el [] (text "Draw!") ]


gameEndedText : GameResult -> String
gameEndedText result =
    let
        resultText =
            case result of
                Draw ->
                    "It's a Draw!"

                Won X ->
                    "Crosses won!"

                Won O ->
                    "Circles were victorious"
    in
    "Game ended! " ++ resultText
