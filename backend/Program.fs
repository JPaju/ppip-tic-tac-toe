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

open TicTacToe


let receivedMessage =
    Subject<MultiplayerGame.InMessage>.broadcast

let sendMessage =
    Subject<MultiplayerGame.OutMessage>.broadcast

let gameState =
    receivedMessage
    |> Observable.scanInit MultiplayerGame.NoPlayers MultiplayerGame.updateMultiplayerState

Observable.zip gameState receivedMessage
|> Observable.map (fun (state, msg) -> MultiplayerGame.createOutmessage state msg)
|> Observable.subscribe (fun msg -> sendMessage |> Subject.onNext msg |> ignore)
|> ignore

let addNewConnection (webSocket: WebSocket) (_: HttpContext) =

    receivedMessage
    |> Subject.onNext MultiplayerGame.PlayerConnected
    |> ignore

    socket {

        sendMessage
        |> Observable.map (
            Encode.outMessage
            >> Encode.toJson
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

                receivedMessage
                |> Subject.onNext MultiplayerGame.PlayerDisconnected
                |> ignore

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
