namespace TicTacToe.Core

module Game =

    type Sign =
        | X
        | O

    type Result =
        | Winner of Sign
        | Draw

    type Coordinate = int * int

    type Mark = Sign * Coordinate

    type Dimensions = { height: int; width: int }

    type Board =
        { dimensions: Dimensions
          marks: Mark list }

    type State =
        | OnGoing of Board * Sign
        | Ended of Board * Result


module Board =
    open Game

    let defaultDimensions = { height = 5; width = 5 }

    let empty =
        { dimensions = defaultDimensions
          marks = [] }

    let getCapacity board =
        board.dimensions.height * board.dimensions.width

    let isFull (board: Board) : bool =
        (getCapacity board) = board.marks.Length

    let contains ((x, y): Coordinate) board =
        let dimensions = board.dimensions
        dimensions.height > y && dimensions.width > x

    let isOccupied (coordinate: Coordinate) (board: Board) =
        let coordinates = List.map snd board.marks
        List.exists ((=) coordinate) coordinates


module Play =
    open Game

    let init sign = OnGoing(Board.empty, sign)

    let changeSign sign =
        match sign with
        | X -> O
        | O -> X

    let placeMark (mark: Mark) (board: Board) : Board =
        let marks = board.marks
        let coordinate = snd mark

        if
            Board.contains coordinate board
            && not (Board.isOccupied coordinate board)
        then
            { board with marks = mark :: marks }
        else
            board

    // TODO Implement
    let hasPlayerWon (player: Sign) (board: Board) =
        (new System.Random()).NextDouble() < 0.1


    let update (newMark: Mark) (game: State) : State =
        match game with
        | OnGoing (board, hasTurn) ->
            // TODO Check that newMark and hasTurn are the same
            let newBoard = placeMark newMark board

            if hasPlayerWon hasTurn newBoard then
                Ended(newBoard, (Winner hasTurn))
            elif Board.isFull newBoard then
                Ended(newBoard, Draw)
            else
                OnGoing(newBoard, changeSign hasTurn)

        | Ended (_, _) -> game
