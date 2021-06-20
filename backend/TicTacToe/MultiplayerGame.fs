namespace TicTacToe

open TicTacToe.Core

module MultiplayerGame =

    type PlayerData = { id: string; name: string }

    type Players = { X: PlayerData; O: PlayerData }

    type GameData =
        { players: Players
          gameState: Game.State }

    type MultiplayerState =
        | NoPlayers
        | OnePlayer of PlayerData
        | TwoPlayers of GameData

    // Consumed / Inputs
    type ConnectionMessage = PlayerConnected of PlayerData
    // | PlayerDisconnected of PlayerData

    type GameMessage = MarkReceived of PlayerData * Game.Mark

    type PlayerMessage =
        | ConnectionMessage of ConnectionMessage
        | GameMessage of GameMessage


    // Produced / Outputs
    type GameEvent =
        | GameStarted of Game.Playing
        | MarkPlaced of Game.Playing * Game.Mark
        | GameEnded of Game.Finished

    type MatchMakingEvent = | SearchingOpponent
    // | OpponentDisconnected

    type Event =
        | GameEvent of GameEvent
        | MatchMakingEvent of MatchMakingEvent

    type Target =
        | Single of PlayerData
        | Multi of Players

    type TargetedEvent = Event * Target

    let processIncomingMessage
        (oldState: MultiplayerState)
        (msg: PlayerMessage)
        : (TargetedEvent option * MultiplayerState) =
        match (msg, oldState) with
        | (ConnectionMessage conMsg, NoPlayers) ->
            match conMsg with
            | PlayerConnected player ->
                let newState = OnePlayer player
                let event = MatchMakingEvent <| SearchingOpponent
                let target = Single player
                let targetedEvent = (event, target)

                (Some(targetedEvent), newState)
        | (ConnectionMessage conMsg, OnePlayer player1) ->
            match conMsg with
            | PlayerConnected player2 ->
                let players = { X = player1; O = player2 }

                let playing = Play.init 5 Game.Sign.X
                let gameState = Game.GameOn <| playing

                let gameData =
                    { players = players
                      gameState = gameState }

                let newState = TwoPlayers gameData
                let event = playing |> GameStarted |> GameEvent
                let target = Multi players
                let targetedEvent = (event, target)

                (Some(targetedEvent), newState)
        | (GameMessage gameMsg,
           TwoPlayers { gameState = oldGameState
                        players = players }) ->
            match gameMsg with
            | MarkReceived (_, mark) ->
                let newGameState = Play.playTurn mark oldGameState

                let gameData =
                    { players = players
                      gameState = newGameState }

                let target = Multi players

                match newGameState with
                | Game.GameOn playingGame ->
                    let event =
                        (playingGame, mark) |> MarkPlaced |> GameEvent

                    let targetedEvent = (event, target)
                    (Some(targetedEvent), TwoPlayers gameData)
                | Game.GameOver gameFinished ->

                    let event = gameFinished |> GameEnded |> GameEvent
                    let targetedEvent = (event, target)
                    (Some(targetedEvent), TwoPlayers gameData)

        | _ -> (None, oldState)


module Messages =
    open MultiplayerGame

    type StartInfo =
        { yourSign: Game.Sign
          startingSign: Game.Sign
          board: Game.Board }

    type Result =
        | Won
        | Lost
        | Draw

    type EndInfo = { board: Game.Board; result: Result }

    type OutMessage =
        | SearchingOpponent
        | OpponentFound of StartInfo
        | NewMark of Game.Playing * Game.Mark
        | GameOver of EndInfo

    let participates ({ id = playerId }: PlayerData) (players: Players) : bool =
        players.X.id = playerId || players.O.id = playerId

    let getSign
        ({ X = { id = circleId }
           O = { id = crossId } }: Players)
        (id: string)
        : Game.Sign option =
        match id with
        | id when id = circleId -> Game.Sign.O |> Some
        | id when id = crossId -> Game.Sign.X |> Some
        | _ -> None

    let getResult (result: Game.Result) (context: Game.Sign) : Result =
        match result with
        | Game.Winner sign when sign = context -> Won
        | Game.Winner _ -> Lost
        | Game.Draw -> Draw

    let fromTargetedEvent (player: PlayerData) ((event, target): TargetedEvent) : OutMessage =
        match event with
        | GameEvent gameEvent ->
            match target with
            | Single _ -> SearchingOpponent
            | Multi players ->
                match gameEvent with
                | GameStarted (board, startingSign) ->
                    let startInfo =
                        getSign players player.id
                        |> Option.map
                            (fun sign ->
                                { startingSign = startingSign
                                  board = board
                                  yourSign = sign })
                        |> Option.get

                    OpponentFound startInfo

                | MarkPlaced (playing, mark) -> NewMark(playing, mark)

                | GameEnded (board, result) ->
                    getSign players player.id
                    |> Option.map (getResult result)
                    |> Option.map (fun res -> { board = board; result = res })
                    |> Option.map GameOver
                    |> Option.get

        | MatchMakingEvent _ -> SearchingOpponent
