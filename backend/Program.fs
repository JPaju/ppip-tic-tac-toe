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
    Subject<MultiplayerGame.PlayerMessage>.broadcast

let events =
    Subject<MultiplayerGame.TargetedEvent>.broadcast

let gameState =
    receivedMessage
    |> Observable.scanInit
        (None, MultiplayerGame.NoPlayers)
        (fun (_, state) msg -> MultiplayerGame.processIncomingMessage state msg)
    |> Observable.filter (fst >> Option.isSome)
    |> Observable.map (fst >> Option.get)
    |> Observable.subscribe (fun msg -> events |> Subject.onNext msg |> ignore)


let addNewConnection (webSocket: WebSocket) (httpContext: HttpContext) =

    let portString = httpContext.connection.port.ToString()
    let name = httpContext.connection.ipAddr.ToString()
    let playerData : MultiplayerGame.PlayerData = { id = portString; name = name }

    events
    |> Observable.map (
        (Messages.fromTargetedEvent playerData)
        >> Encode.outMessage
        >> Encode.toJson
        >> System.Text.Encoding.UTF8.GetBytes
        >> ByteSegment
    )
    |> Observable.map (fun bytes -> webSocket.send Text bytes true)
    |> Observable.subscribe (Async.RunSynchronously >> ignore) // TODO Fix ?
    |> ignore

    receivedMessage
    |> Subject.onNext (
        playerData
        |> MultiplayerGame.PlayerConnected
        |> MultiplayerGame.ConnectionMessage
    )
    |> ignore

    socket {

        let mutable loop = true

        while loop do
            let! msg = webSocket.read ()

            match msg with
            | (Text, data, true) ->

                data
                |> Encoding.UTF8.GetString
                |> Decode.inMessage playerData
                |> Option.iter
                    (fun msg ->
                        System.Console.WriteLine($"Sending message to: {name}, content: {msg}") //TODO remove?
                        (receivedMessage |> Subject.onNext msg |> ignore))

            | (Close, _, _) ->
                let emptyResponse = [||] |> ByteSegment
                do! webSocket.send Close emptyResponse true

                // receivedMessage
                // |> Subject.onNext (playerData |> MultiplayerGame.PlayerDisconnected |> MultiplayerGame.ConnectionMessage)
                // |> ignore

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
