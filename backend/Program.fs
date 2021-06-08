open Suave
open Suave.Operators
open Suave.Filters
open Suave.Files
open Suave.RequestErrors
open Suave.Logging
open System
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open System.Text
open TicTacToe.Core.Domain
open TicTacToe.Core
open FSharp.Control.Reactive


type PlayerId = string
type Command = Mark * PlayerId

let receivedCommands = Subject<Command>.broadcast

receivedCommands
|> Observable.subscribe (fun newCommand -> Console.WriteLine($"New command from websocket: {newCommand}"))
|> ignore


module Decode =

    let private parseInt string =
        try
            let i = System.Int32.Parse string
            Some(i)
        with _ -> None

    let coordinate x y : Coordinate option =
        Option.map2 (fun x y -> (x, y)) (parseInt x) (parseInt y)

    let sign (sign: string) : Sign option =
        match sign.ToUpper() with
        | "O" -> Some O
        | "X" -> Some X
        | _ -> None

    let command (string: string) : Command option =
        match string.Split([| ',' |]) with
        | [| x; y; signString; name |] ->
            ((coordinate x y), (sign signString))
            ||> Option.map2 (fun coord sign -> (sign, coord))
            |> Option.map (fun mark -> (mark, name))

        | _ -> None


let accumulator (oldState: GameState) (mark: Mark) : GameState =
    let newState = Game.update mark oldState

    System.Console.WriteLine($"Old state: {oldState}")
    System.Console.WriteLine($"New state: {newState}")

    newState

let gameState =
    receivedCommands
    |> Observable.iter (fun cmd -> System.Console.WriteLine($"Received new command: {cmd}"))
    |> Observable.map (fun (mark, _) -> mark)
    |> Observable.scanInit (Game.init O) accumulator


gameState
|> Observable.subscribe (fun _ -> printfn "\n\n")
|> ignore


let ws (webSocket: WebSocket) (_: HttpContext) =

    socket {
        let mutable loop = true

        while loop do
            let! msg = webSocket.read ()

            match msg with
            | (Text, data, true) ->

                let msgFromClient = Encoding.UTF8.GetString data //UTF8Encoding.UTF8.ToString data

                System.Console.WriteLine($"Received websocket message: {msgFromClient}")

                Decode.command msgFromClient
                |> Option.iter (fun cmd -> (receivedCommands |> Subject.onNext cmd |> ignore))

                let byteResponse =
                    "Received: " + msgFromClient
                    |> System.Text.Encoding.UTF8.GetBytes
                    |> ByteSegment

                do! webSocket.send Text byteResponse true

            | (Close, _, _) ->
                let emptyResponse = [||] |> ByteSegment
                do! webSocket.send Close emptyResponse true
                loop <- false

            | _ -> ()
    }

let wsWithErrorHandling (webSocket: WebSocket) (context: HttpContext) : Async<Choice<unit, Error>> =

    let exampleDisposableResource =
        { new IDisposable with
            member __.Dispose() =
                printfn "Resource needed by websocket connection disposed" }

    let websocketWorkflow = ws webSocket context

    async {
        let! successOrError = websocketWorkflow

        match successOrError with
        | Choice1Of2 () -> ()
        | Choice2Of2 (error) ->
            printfn "Error: [%A]" error
            exampleDisposableResource.Dispose()

        return successOrError
    }

let app : WebPart =
    choose [ path "/websocket" >=> handShake ws
             // path "/websocketWithSubprotocol" >=> handShakeWithSubprotocol (chooseSubprotocol "test") ws
             path "/websocketWithError"
             >=> handShake wsWithErrorHandling
             GET
             >=> choose [ path "/" >=> file "index.html"
                          browseHome ]
             NOT_FOUND "Found no handlers." ]

[<EntryPoint>]
let main _ =
    startWebServer
        { defaultConfig with
              logger = Targets.create Verbose [||] }
        app

    0
