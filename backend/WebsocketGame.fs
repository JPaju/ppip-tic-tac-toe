namespace TicTacToe

open FSharp.Control.Reactive
open TicTacToe.Core.Domain
open TicTacToe.Core


module Game =

    type WebsocketGameState =
        | NoPlayers
        | OnePlayer of Sign
        | Playing of GameState

    type InMessage =
        | PlayerConnected
        | PlayerDisconnected
        | MarkReceived of Mark


    let updateGame (message: InMessage) (state: WebsocketGameState) : WebsocketGameState =
        match (message, state) with
        | (PlayerConnected, NoPlayers) -> OnePlayer X
        | (PlayerConnected, OnePlayer sign) ->
            let newGame = GameOn(Board.empty, sign)
            Playing newGame
        | (MarkReceived mark, Playing game) -> Playing(Game.update mark game)
        | (_, _) -> state


module Test =
    let test = Subject<int>.broadcast

    test
    |> Observable.subscribe (fun (value: int) -> printfn "%i" value)
    |> ignore

    test
    |> Subject.onNext 34
    |> Subject.onNext 37
    |> Subject.onNext 37
    |> Subject.onNext 37
    |> Subject.onNext 37
    |> Subject.onNext 69
    |> Subject.onNext 420
    |> ignore
