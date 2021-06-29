# Tic Tac Toe

## Prerequisites
### Backend
[.NET 5.0](https://dotnet.microsoft.com/download) is required to run the backend.
To run the backend you must navigate to the `backend` folder and run the following command. This will start the backend on port `8080`. 
```
dotnet run
```
### Frontend
The frontend requires npm for the installation of the elm environment. You can install the elm environment with the following command.
```
npm i create-elm-app 
```
The frontend can then be started from the `frontend` folder with the following command after you have installed the prerequisites.
```sh
elm-app start
```
## Playing the game
You need to open two browser windows with the frontend opened. Then you need to choose `Play online` and the game starts when both players have joined. When the game has ended, the backend must be restarted if you want to play again. 
