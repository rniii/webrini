var app = Elm.Main.init({});
var ws = new WebSocket(location.href.replace("http", "ws") + "chat");

var timestamp = () =>
    new Date().toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" });

var isoTime = () =>
    new Date().toISOString();

app.ports.send.subscribe(m => ws.send(JSON.stringify(m)));
app.ports.push.subscribe(h => {
    const scroll = chat.scrollHeight - chat.clientHeight - chat.scrollTop <= 1;

    chat.insertAdjacentHTML("beforeend", `<div><time datetime="${isoTime()}">${timestamp()}</time> ${h}`);
    if (scroll) chat.lastElementChild.scrollIntoView();
});

ws.onmessage = m => app.ports.recv.send(JSON.parse(m.data));
ws.onerror = e => app.ports.recv.send({ tag: "Error", contents: e });
chat_input.onchange = e => {
    app.ports.input.send(e.target.value);
    e.target.value = "";
};
