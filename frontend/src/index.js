import './main.css';
import { Elm } from './Main.elm';

const app = Elm.Main.init({
  node: document.getElementById('root')
});


const endGameAfterMessages = 3
const opponentDelay = 1000
const connectDelay = 1500
const dimensions = { height: 5, width: 5 }


const searchingOpponent = { waiting: 'lulz' }

const gameOn = {
  yourSign: 'X', nowHasTurn: 'X',
  board: { dimensions, marks: [] }
}

const markPlaced = lastMark => ({
  newMark: {
    coordinate: { x: lastMark.coordinate.x + 1, y: lastMark.coordinate.y, },
    sign: lastMark.sign === 'X' ? 'O' : 'X'
  }
})

const gameEnded = marks => ({ board: { dimensions, marks }, result: 'Won' })


let messageCount = 0
let currentBoard = []
app.ports.sendMark.subscribe(mark => {
  messageCount++
  currentBoard.push(mark)

  const toSend = markPlaced(mark)
  currentBoard.push(toSend.newMark)

  setTimeout(
    () => {
      app.ports.receiveGameMessage.send(toSend)

      if (messageCount >= endGameAfterMessages) {
        app.ports.receiveGameMessage.send(gameEnded(currentBoard))
        messageCount = 0
        currentBoard = []
      }
    },
    opponentDelay
  )
})




app.ports.debugPort.subscribe(msg => {
  switch (msg.toLowerCase()) {
    case "connect":
      setTimeout(
        () => app.ports.receiveGameMessage.send(searchingOpponent),
        connectDelay
      )
      break;

    case "searchopponent":
      setTimeout(
        () => app.ports.receiveGameMessage.send(gameOn),
        connectDelay
      )
      break;

    default:
      break;
  }
})
