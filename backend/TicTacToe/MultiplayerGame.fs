namespace TicTacToe

open TicTacToe.Core

module MultiplayerGame =

    type State =
        | NoPlayers
        | OnePlayer of Game.Sign
        | TwoPlayers of Game.State

    type InMessage =
        | PlayerConnected
        | PlayerDisconnected
        | MarkReceived of Game.Mark

    type OutMessage =
        | SearchingOpponent
        | MarkPlaced of Game.Playing * Game.Mark
        | GameEnded of Game.Finished


    let updateMultiplayerState (state: State) (message: InMessage) : State =
        match (message, state) with
        | (PlayerConnected, NoPlayers) -> OnePlayer Game.Sign.X
        | (PlayerConnected, OnePlayer sign) ->
            let newGame = Play.init sign
            TwoPlayers newGame
        | (PlayerDisconnected, OnePlayer _) -> NoPlayers
        | (MarkReceived mark, TwoPlayers game) -> TwoPlayers(Play.update mark game)
        | (_, _) -> state

    let updateBoard (oldState: Game.State) (msg: InMessage) : Game.State =
        match msg with
        | MarkReceived mark -> Play.update mark oldState
        | _ -> oldState

    let createOutmessage (currentState: State) (msg: InMessage) : OutMessage =
        match currentState with
        | NoPlayers -> SearchingOpponent
        | OnePlayer _ -> SearchingOpponent
        | TwoPlayers boardState ->
            match (boardState, msg) with
            | (Game.GameOn game, MarkReceived mark) -> MarkPlaced(game, mark)
            | (Game.GameOver result, MarkReceived _) -> GameEnded result
            | _ -> SearchingOpponent
