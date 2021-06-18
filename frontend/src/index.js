import './main.css';
import { Elm } from './Main.elm';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

let connection;

app.ports.sendMark.subscribe(mark => {
  const decoded = JSON.stringify(mark);

  connection.send(decoded);
})

app.ports.connectionPort.subscribe(msg => {
  if (msg === "connect") {

    connection = new WebSocket("ws://localhost:8080/play")
    connection.onmessage = (event) => {
      const raw = event.data;
      const parsed = JSON.parse(raw)

      app.ports.receiveGameMessage.send(parsed);
    }
  } else if (msg === "close") {
    connection.close();
  }
})
