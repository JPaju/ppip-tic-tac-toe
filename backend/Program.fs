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
open TicTacToe.Game
open TicTacToe
open Json

type WSGame =
    { mutable player1: WebSocket option
      mutable player2: WebSocket option
      mutable game: GameState }

let emptyGame =
    { player1 = None
      player2 = None
      game = NoPlayers }

let ws (game: WSGame) (webSocket: WebSocket) (context: HttpContext) =

    socket {
        let mutable loop = true

        while loop do
            //   type Opcode = Continuation | Text | Binary | Reserved | Close | Ping | Pong
            let! msg = webSocket.read ()

            match msg with
            | (Text, data, true) ->
                // the message can be converted to a string
                let msgFromClient = Encoding.UTF8.GetString data //UTF8Encoding.UTF8.ToString data

                // let deserializedFromClient =
                //     deserialize<Dimensions> (msgFromClient)


                // match deserializedFromClient with
                // | Ok req -> System.Console.WriteLine req
                // | _ -> ()



                (* let testi2Serialization test (msg: Result<Dimensions, exn>): Dimensions =
                    match msg with
                        | Ok req -> req
                        | _ -> "seg" *)


                //let response: string = TicTacToe.Json.serialize testi;


                // let dimensions : Dimensions = { height = 420; width = 69 }
                //let markplaced : MarkPlaced = { height = 420; width = 69 }
                // let marks : Mark list =
                //     [ { sign = X
                //         coordinate = { x = 3; y = 5 } }
                //       { sign = O
                //         coordinate = { x = 1; y = 2 } } ]


                // let board : Board =
                //     { dimensions = dimensions
                //       marks = marks }

                // let markplaced : MarkPlaced = { newMark = marks.Head; board = board }

                // let message : Message =
                //     { msgType = NewMark
                //       msg = MarkPlaced(markplaced) }

                // let response : string = TicTacToe.Json.serialize markplaced
                let response : string = "lol"

                System.Console.WriteLine response
                // the response needs to be converted to a ByteSegment
                let byteResponse =
                    response
                    |> System.Text.Encoding.UTF8.GetBytes
                    |> ByteSegment

                // the `send` function sends a message back to the client
                do! webSocket.send Text byteResponse true

            | (Close, _, _) ->
                let emptyResponse = [||] |> ByteSegment
                do! webSocket.send Close emptyResponse true

                // after sending a Close message, stop the loop
                loop <- false

            | _ -> ()
    }

/// An example of explictly fetching websocket errors and handling them in your codebase.
let wsWithErrorHandling (webSocket: WebSocket) (context: HttpContext) : Async<Choice<unit, Error>> =

    let exampleDisposableResource =
        { new IDisposable with
            member __.Dispose() =
                printfn "Resource needed by websocket connection disposed" }

    let websocketWorkflow = ws emptyGame webSocket context

    async {
        let! successOrError = websocketWorkflow

        match successOrError with
        // Success case
        | Choice1Of2 () -> ()
        // Error case
        | Choice2Of2 (error) ->
            // Example error handling logic here
            printfn "Error: [%A]" error
            exampleDisposableResource.Dispose()

        return successOrError
    }

let app : WebPart =
    choose [ path "/websocket" >=> handShake (ws emptyGame)
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
