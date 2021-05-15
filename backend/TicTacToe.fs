namespace TicTacToe

open System.Text.Json

module Game =
    type Sign =
        | X
        | O

    type Result =
        | Winner of Sign
        | Draw

    type Coordinate = { x: int; y: int }

    type Mark = { sign: Sign; coordinate: Coordinate }

    type Dimensions = { height: int; width: int }

    type Board =
        { dimensions: Dimensions
          marks: Mark list }


module Message =

    type GameOn =
        { board: Game.Board
          nowHasTurn: Game.Sign
          yourSign: Game.Sign }

    type MarkPlaced =
        { newMark: Game.Mark
          board: Game.Board }

    type Result =
        | Won
        | Lost
        | Draw

    type GameEnded = { board: Game.Board; result: Result }


    type MessageContent =
        | SearchingOpponent
        | GameOn of GameOn
        | MarkPlaced of MarkPlaced
        | GameEnded of GameEnded

    type MessageType =
        | SearchingOpponent
        | GameOn
        | MarkPlaced
        | GameEnded

    type Message =
        { msgType: MessageType
          msg: MessageContent }

    module Decoder =
        type Test = int

module Json =
    let serialize obj = JsonSerializer.Serialize obj


    type Test =
        | JsonException
        | ArgumentNullException

    let deserialize<'a> (str: string) : Result<'a, exn> =
        try
            JsonSerializer.Deserialize<'a> str |> Ok
        with ex -> Error ex
