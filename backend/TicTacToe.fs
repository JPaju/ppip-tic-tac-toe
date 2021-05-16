namespace TicTacToe

module Bilu =
    let todo<'T> : 'T = raise (System.NotImplementedException())

module Game =

    type Sign =
        | X
        | O

    let changeSign sign =
        match sign with
        | X -> O
        | O -> X

    type Result =
        | Winner of Sign
        | Draw

    type Coordinate = int * int

    type Mark = Sign * Coordinate

    type Dimensions = { height: int; width: int }

    type Board =
        { dimensions: Dimensions
          marks: Mark list }

    let defaultDimensions = { height = 5; width = 5 }

    let emptyBoard =
        { dimensions = defaultDimensions
          marks = [] }

    type GameState =
        | NoPlayers
        | OnePlayer of Sign
        | GameOn of Board * Sign
        | GameEnded of Board * Result

    type InMessage =
        | PlayerConnected
        | PlayerDisconnected
        | MarkReceived of Mark

module Blah =
    open Game

    let private isInBoard (_, (x, y)) board =
        let dimensions = board.dimensions
        dimensions.height > y && dimensions.width > x

    let private isOccupied (board: Board) (coordinate: Coordinate) =
        let coordinates = List.map snd board.marks
        List.exists ((=) coordinate) coordinates

    let private placeMark (mark: Mark) (board: Board) : Board =
        let marks = board.marks
        let coordinate = snd mark

        if
            isInBoard mark board
            && not (isOccupied board coordinate)
        then
            { board with marks = mark :: marks }
        else
            board

    let private hasPlayerWon (player: Sign) (board: Board) =
        (new System.Random()).NextDouble() < 0.1

    let private getCapacity board =
        board.dimensions.height * board.dimensions.width

    let private isFull (board: Board) : bool =
        (getCapacity board) = board.marks.Length

    // let test =
    //     placeMark
    //         (X, (-34, 999999999))
    //         { marks = []
    //           dimensions = defaultDimensions }

    let updateGame (message: InMessage) (game: GameState) : GameState =
        match (message, game) with
        | (PlayerConnected, NoPlayers) -> OnePlayer X
        | (PlayerConnected, OnePlayer sign) -> GameOn(emptyBoard, changeSign sign)
        | (MarkReceived mark, GameOn (board, sign)) ->
            let newBoard = placeMark mark board

            if hasPlayerWon sign newBoard then
                GameEnded(newBoard, (Winner sign))
            elif isFull newBoard then
                GameEnded(newBoard, Draw)
            else
                GameOn(newBoard, changeSign sign)
        | (_, _) -> game
