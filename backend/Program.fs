open Suave
open Suave.Operators
open Suave.Filters
open Suave.RequestErrors
open Suave.Logging
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open System.Text
open FSharp.Control.Reactive

open TicTacToe.Core
open TicTacToe.MultiplayerGame

let receivedMessage = Subject<InMessage>.broadcast
let sendMessage = Subject<OutMessage>.broadcast

module Decode =

    let private parseInt string =
        try
            System.Int32.Parse string |> Some
        with _ -> None

    let private coordinate x y : Game.Coordinate option =
        Option.map2 (fun x y -> (x, y)) (parseInt x) (parseInt y)

    let private sign (sign: string) : Game.Sign option =
        match sign.ToUpper() with
        | "O" -> Game.Sign.O |> Some
        | "X" -> Game.Sign.X |> Some
        | _ -> None

    let inMessage (string: string) : InMessage option =
        match string.Split([| ',' |]) with
        | [| x; y; signString |] ->
            ((coordinate x y), (sign signString))
            ||> Option.map2 (fun coord sign -> (sign, coord))
            |> Option.map MarkReceived

        | _ -> None


module Encode =
    let sign (sign: Game.Sign) : string =
        match sign with
        | Game.Sign.X -> "X"
        | Game.Sign.O -> "O"

    let coordinate ((x, y): Game.Coordinate) : string = $"x: {x}, y: {y}"

    let mark ((s, c): Game.Mark) : string = $"{sign s} ({coordinate c})"

    let board ({ marks = marks }: Game.Board) : string = $"{marks}"

    let outMessage (msg: OutMessage) : string =
        match msg with
        | MarkPlaced (b, m) -> $"New mark: {mark m}, current board: {board b}"
        | _ -> "Some not very important message"


let updateGame (oldState: Game.State) (msg: InMessage) : Game.State =
    match msg with
    | MarkReceived mark -> Play.update mark oldState
    | _ -> oldState

let createOutmessage (currentState: Game.State) (msg: InMessage) : OutMessage =
    match (currentState, msg) with
    | (Game.OnGoing (board, _), MarkReceived mark) -> MarkPlaced(board, mark)
    | _ -> SearchingOpponent

let gameState =
    receivedMessage
    |> Observable.scanInit (Play.init Game.Sign.O) updateGame

Observable.zip gameState receivedMessage
|> Observable.map (fun (state, msg) -> createOutmessage state msg)
|> Observable.subscribe (fun msg -> sendMessage |> Subject.onNext msg |> ignore)
|> ignore

let addNewConnection (webSocket: WebSocket) (_: HttpContext) =

    socket {

        sendMessage
        |> Observable.map (
            Encode.outMessage
            >> System.Text.Encoding.UTF8.GetBytes
            >> ByteSegment
        )
        |> Observable.map (fun bytes -> webSocket.send Text bytes true)
        |> Observable.subscribe (Async.RunSynchronously >> ignore) // TODO Fix ?
        |> ignore

        let mutable loop = true

        while loop do
            let! msg = webSocket.read ()

            match msg with
            | (Text, data, true) ->

                data
                |> Encoding.UTF8.GetString
                |> Decode.inMessage
                |> Option.iter (fun msg -> (receivedMessage |> Subject.onNext msg |> ignore))

            | (Close, _, _) ->
                let emptyResponse = [||] |> ByteSegment
                do! webSocket.send Close emptyResponse true
                loop <- false

            | _ -> ()
    }


let app : WebPart =
    choose [ path "/play" >=> handShake addNewConnection
             NOT_FOUND "Found no handlers." ]

[<EntryPoint>]
let main _ =
    startWebServer
        { defaultConfig with
              logger = Targets.create Verbose [||] }
        app

    0
