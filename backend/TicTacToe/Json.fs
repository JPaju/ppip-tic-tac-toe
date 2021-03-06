namespace TicTacToe

open Thoth.Json.Net
open TicTacToe.Core
open TicTacToe.MultiplayerGame
open TicTacToe.Messages

module Decode =

    let fromString = Decode.fromString

    let sign : Decoder<Game.Sign> =
        Decode.string
        |> Decode.andThen
            (fun signString ->
                match signString.ToUpper() with
                | "X" -> Decode.succeed Game.Sign.X
                | "O" -> Decode.succeed Game.Sign.O
                | _ -> Decode.fail ("Invalid sign: " + signString))

    let coordinate : Decoder<Game.Coordinate> =
        Decode.map2 (fun x y -> (x, y)) (Decode.field "x" Decode.int) (Decode.field "y" Decode.int)

    let mark : Decoder<Game.Mark> =
        Decode.map2 (fun s c -> (s, c)) (Decode.field "sign" sign) (Decode.field "coordinate" coordinate)

    let inMessage (player: PlayerData) (string: string) : PlayerMessage option =
        fromString mark string
        |> function
        | Ok s -> MarkReceived(player, s) |> GameMessage |> Some
        | _ -> None

module Encode =
    let toJson<'a> = Encode.toString 0

    let sign (s: Game.Sign) =
        match s with
        | Game.Sign.O -> "O"
        | Game.Sign.X -> "X"
        |> Encode.string

    let coordinate ((x, y): Game.Coordinate) =
        Encode.object [ "x", Encode.int x
                        "y", Encode.int y ]

    let mark ((s, c): Game.Mark) =
        Encode.object [ "coordinate", coordinate c
                        "sign", sign s ]

    let dimensions ({ height = height; width = width }: Game.Dimensions) =
        Encode.object [ "height", Encode.int height
                        "width", Encode.int width ]

    let board ({ marks = m; dimensions = d }: Game.Board) =
        Encode.object [ "dimensions", dimensions d
                        "marks", m |> List.map mark |> Encode.list ]

    let markPlaced (b: Game.Board) (newMark: Game.Mark) =
        Encode.object [ "newMark", mark newMark
                        "board", board b ]

    let gameOn
        ({ board = b
           startingSign = startingSign
           yourSign = yourSign }: StartInfo)
        =
        Encode.object [ "yourSign", sign yourSign
                        "nowHasTurn", sign startingSign
                        "board", board b ]

    let result (r: Result) =
        match r with
        | Won -> Encode.string "won"
        | Lost -> Encode.string "lost"
        | Draw -> Encode.string "draw"

    let gameEnded (b: Game.Board) (r: Result) =
        Encode.object [ "board", board b
                        "result", result r ]

    let searchingOpponent =
        Encode.object [ "waiting", Encode.string "waiting" ]

    let outMessage (msg: OutMessage) =
        match msg with
        | OpponentFound startData -> gameOn startData
        | NewMark ((b, _), m) -> markPlaced b m
        | GameOver { board = b; result = r } -> gameEnded b r
        | SearchingOpponent -> searchingOpponent
