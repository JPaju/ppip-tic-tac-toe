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

    type Playing = Board * Sign
    type Finished = Board * Result

    type State =
        | GameOn of Playing
        | GameOver of Finished

module Coordinate =
    let up (x, y) = (x, y + 1)
    let down (x, y) = (x, y - 1)
    let right (x, y) = (x + 1, y)
    let left (x, y) = (x - 1, y)


module Board =
    open Game

    let square size =
        let dimensions = { height = size; width = size }
        { dimensions = dimensions; marks = [] }

    let private getCapacity board =
        board.dimensions.height * board.dimensions.width

    let isFull (board: Board) : bool =
        (getCapacity board) = board.marks.Length

    let contains ((x, y): Coordinate) board =
        let dimensions = board.dimensions
        dimensions.height > y && dimensions.width > x

    let isOccupied (coordinate: Coordinate) (board: Board) =
        let coordinates = List.map snd board.marks
        List.exists ((=) coordinate) coordinates

module Rules =
    open Game

    let private hasNInRow (toNextCoordinate: Coordinate -> Coordinate) (n: int) (coordinates: Coordinate list) : bool =

        let allCoordinates = Set.ofList coordinates

        let rec hasInRowHelper (count: int) (coordinate: Coordinate) : bool =
            if (count = n) then
                true
            else
                let nextCoordinate = toNextCoordinate coordinate

                let nextHasValue =
                    allCoordinates |> Set.contains nextCoordinate

                nextHasValue
                && (hasInRowHelper (count + 1) nextCoordinate)

        let rec loop (coordinates: Coordinate list) : bool =
            match coordinates with
            | currentCoordinate :: rest -> hasInRowHelper 1 currentCoordinate || loop rest
            | _ -> false

        loop coordinates

    let private hasNVertically = hasNInRow Coordinate.up

    let private hasNHorizontally = hasNInRow Coordinate.right

    let private hasNDiagonally (n: int) (coordinates: Coordinate list) =
        hasNInRow (Coordinate.up >> Coordinate.right) n coordinates
        || hasNInRow (Coordinate.down >> Coordinate.right) n coordinates

    let marksInRowRequiredToWin { height = height; width = width } =
        let smallerDimension = min height width

        if smallerDimension = 3 then 3
        else if smallerDimension < 6 then 4
        else 5

    let hasPlayerWon (player: Sign) (board: Board) =
        let playerCoordinates =
            board.marks
            |> List.filter (fst >> ((=) player))
            |> List.map snd

        let requiredToWin = marksInRowRequiredToWin board.dimensions

        hasNVertically requiredToWin playerCoordinates
        || hasNHorizontally requiredToWin playerCoordinates
        || hasNDiagonally requiredToWin playerCoordinates


module Play =
    open Game

    let init size sign = (Board.square size, sign)

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


    let playTurn (newMark: Mark) (game: State) : State =
        match game with
        | GameOn (board, hasTurn) ->
            // TODO Check that newMark and hasTurn are the same
            let newBoard = placeMark newMark board

            if Rules.hasPlayerWon hasTurn newBoard then
                GameOver(newBoard, (Winner hasTurn))
            elif Board.isFull newBoard then
                GameOver(newBoard, Draw)
            else
                GameOn(newBoard, changeSign hasTurn)

        | GameOver (_, _) -> game
