export const flags = ({ env }) => {
  return {
    apiUrl: env.API_URL,
  };
};

export const onReady = ({ app, env }) => {
  if (app.ports && app.ports.outgoing) {
    app.ports.outgoing.subscribe(({ tag, data }) => {
      switch (tag) {
        case "CREATE_WEBSOCKET":
          createWebSocket(app, env, data);
          return;
        case "SEND_WEBSOCKET_MESSAGE":
          sendWebSocketMessage(data);
          return;
        default:
          console.warn(`Unhandled outgoing port: "${tag}"`);
          return;
      }
    });
  }
};

let ws;

function createWebSocket(app, env, data) {
  const { WEBSOCKET_URL = "ws://127.0.0.1:8080/" } = env;

  if (!WEBSOCKET_URL) {
    console.error("Websocket url is required to create websocket");
    return;
  }

  const createWebSocket = () => {
    const { crosswordId } = data;

    if (!crosswordId) {
      console.error("Crossword id is required to create websocket");
      return;
    }

    const teamId = 1; //TODO
    const userId = 1; //TODO

    const url = `${WEBSOCKET_URL}move/${teamId}/${crosswordId}/${userId}`;
    ws = new WebSocket(url);

    ws.addEventListener("message", function (event) {
      app.ports.messageReceiver.send(event.data);
    });
    ws.onopen = function () {
      console.log(`Connected to websocket ${url}`);
    };
    ws.onclose = function () {
      console.log("Disconnected from websocket");
      createWebSocket();
    };
  };
  createWebSocket();
  console.log("Websocket created");
}

function sendWebSocketMessage(data) {
  if (!ws) {
    console.error("Websocket is not initialized");
    return;
  }

  ws.send(JSON.stringify(data));
}
