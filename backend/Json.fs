module Json

open System.Text.Json
open TicTacToe.Core.Game

let serialize obj = JsonSerializer.Serialize obj

type Test =
    | JsonException
    | ArgumentNullException

let deserialize<'a> (str: string) : Result<'a, exn> =
    try
        JsonSerializer.Deserialize<'a> str |> Ok
    with ex -> Error ex


type GameOn =
    { board: Board
      nowHasTurn: Sign
      yourSign: Sign }

type MarkPlaced = { newMark: Mark; board: Board }

type Result =
    | Won
    | Lost
    | Draw

type GameEnded = { board: Board; result: Result }

type Message =
    | SearchingOpponent
    | GameOn of GameOn
    | NewMark of MarkPlaced
    | GameEnded of GameEnded
