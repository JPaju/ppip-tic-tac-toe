module Json

open System.Text.Json
open TicTacToe

let serialize obj = JsonSerializer.Serialize obj

type Test =
    | JsonException
    | ArgumentNullException

let deserialize<'a> (str: string) : Result<'a, exn> =
    try
        JsonSerializer.Deserialize<'a> str |> Ok
    with ex -> Error ex


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

type Message =
    | SearchingOpponent
    | GameOn of GameOn
    | NewMark of MarkPlaced
    | GameEnded of GameEnded
