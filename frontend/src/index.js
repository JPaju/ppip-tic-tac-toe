import './main.css';
import { Elm } from './Main.elm';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.sendCoordinate.subscribe(coordinate => {
  console.log('Sending coordinate:', coordinate)

  const [prevX, prevY] = coordinate

  const mark = {
    coordinate: { x: prevX + 1, y: prevY, },
    sign: 'X'
  }

  setTimeout(
    () => app.ports.receiveCoordinate.send(mark),
    1000
  )
})
