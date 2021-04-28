module Page.OfflineGame exposing (Model, Msg, init, update, view)

import Element exposing (Attribute, Element, alignTop, centerX, centerY, column, el, fill, inFront, maximum, minimum, row, spacing, text, width)
import Element.Font as Font
import TicTacToe.Board as Board exposing (Board)
import TicTacToe.Coordinate exposing (Coordinate)
import TicTacToe.Matrix exposing (Dimensions)
import TicTacToe.Sign as Sign exposing (Sign(..))
import Ui


type alias BoardSettings =
    { startingSign : Sign
    , dimensions : Dimensions
    }


type Game
    = Over Board GameResult
    | Playing
        { board : Board
        , hasTurn : Sign
        , marksRequiredToWin : Int
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
            5
    in
    StartPage
        { startingSign = X
        , dimensions = { height = boardSize, width = boardSize }
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
                    StartPage { settings | dimensions = dimensions }

                StartGameClicked ->
                    Playing
                        { board = Board.create settings.dimensions []
                        , hasTurn = settings.startingSign
                        , marksRequiredToWin = Board.marksInRowRequiredToWin settings.dimensions
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
        Playing ({ board, hasTurn, marksRequiredToWin } as gameState) ->
            let
                newBoard =
                    Board.placeMark { sign = hasTurn, location = clickedCoordinate } board

                boardFull =
                    Board.isFull newBoard
            in
            if boardFull then
                Over newBoard Draw

            else if Board.hasWon marksRequiredToWin newBoard hasTurn then
                Over newBoard (Won hasTurn)

            else
                Playing
                    { gameState
                        | hasTurn = Sign.change hasTurn
                        , board = newBoard
                    }

        (Over _ _) as over ->
            over



---- VIEW ----


view : Model -> Element Msg
view model =
    column [ centerX, centerY, spacing 70 ] <|
        case model of
            StartPage { dimensions, startingSign } ->
                [ el (centerX :: alignTop :: Ui.pageHeaderStyle) (text "Game settings")
                , boardDimensionSliders dimensions BoardDimensionsChanged
                , selectStartSign startingSign
                , Ui.button [ centerX ] { label = "Start game", enabled = True, onClick = StartGameClicked }
                ]

            Game game ->
                [ gameHeader game
                , case game of
                    Playing { board, marksRequiredToWin } ->
                        column [ spacing 15 ]
                            [ Board.view [] BoardClicked board
                            , marksRequiredToWinText [ Font.size 20, centerX ] marksRequiredToWin
                            ]

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
        [ width (fill |> minimum 200 |> maximum 300), spacing 30 ]
        [ createSlider "Height" dimensions.height (\newValue -> { dimensions | height = newValue } |> toMsg)
        , createSlider "Width" dimensions.width (\newValue -> { dimensions | width = newValue } |> toMsg)
        , marksRequiredToWinText [ centerX, Font.size 16 ] (Board.marksInRowRequiredToWin dimensions)
        ]


marksRequiredToWinText : List (Attribute msg) -> Int -> Element msg
marksRequiredToWinText attributes amount =
    el attributes (text (String.fromInt amount ++ " marks in a row to win"))


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
