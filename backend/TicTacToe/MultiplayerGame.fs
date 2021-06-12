namespace TicTacToe

open TicTacToe.Core

module MultiplayerGame =

    type State =
        | NoPlayers
        | OnePlayer of Game.Sign
        | Playing of Game.State

    type InMessage =
        | PlayerConnected
        | PlayerDisconnected
        | MarkReceived of Game.Mark

    type OutMessage =
        | SearchingOpponent
        | GameStarted of Game.Board * Game.Sign
        | MarkPlaced of Game.Board * Game.Mark
        | GameEnded of Game.Board * Game.Result


    let updateGame (message: InMessage) (state: State) : State =
        match (message, state) with
        | (PlayerConnected, NoPlayers) -> OnePlayer Game.Sign.X
        | (PlayerConnected, OnePlayer sign) ->
            let newGame = Game.OnGoing(Board.empty, sign)
            Playing newGame
        | (MarkReceived mark, Playing game) -> Playing(Play.update mark game)
        | (_, _) -> state
