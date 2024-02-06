var app = Elm.Main.init({});
var ws = new WebSocket(location.href.replace("http", "ws") + "chat");

app.ports.send.subscribe(m => ws.send(JSON.stringify(m)));
app.ports.push.subscribe(h => chat.insertAdjacentHTML("beforeend", h));

ws.onmessage = m => app.ports.recv.send(JSON.parse(m.data));
chat_input.onchange = e => {
    app.ports.input.send(e.target.value);
    e.target.value = "";
};
