module Websocket = {
  type t;
  type event = {data: string};
  type error = {reason: string};

  [@mel.new] external connect_ws: string => t = "WebSocket";
  [@mel.set] external on_message: (t, event => unit) => unit = "onmessage";
  [@mel.set] external on_error: (t, error => unit) => unit = "onerror";
  [@mel.send] external send_message: (t, string) => unit = "send";
};

module Html = {
  type html = string;
  type attr = string;
  type dom_node;

  external chat_input: dom_node = "chat_input";
  [@mel.set] external on_input: (dom_node, 'a => unit) => unit = "onchange";

  let push: html => unit = [%mel.raw
    "h => chat.insertAdjacentHTML('beforeend', h)"
  ];

  let text: string => html = [%mel.raw
    "v => v.replace(/[\"&<>]/g, c => `&#${c.charCodeAt()};`)"
  ];

  let fragment = (children: list(html)): html =>
    String.concat("", children);

  let attr = (name: string, value: string): attr =>
    " " ++ name ++ "=\"" ++ value ++ "\"";

  let node = (tag: string, attrs: list(attr), children: list(html)): html =>
    "<"
    ++ String.concat(" ", [tag, ...attrs])
    ++ ">"
    ++ String.concat("", children)
    ++ "</"
    ++ tag
    ++ ">";

  let node_ = (tag: string, attrs: list(attr)): html =>
    "<" ++ String.concat(" ", [tag, ...attrs]) ++ ">";

  let div = node("div");
  let span = node("span");

  let br = node_("br");

  let style = attr("style");
};

module Utils = {
  type date;

  [@mel.new] external date: unit => date = "Date";

  let timestamp = (_date: date): string => [%mel.raw
    "_date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })"
  ];

  let iso_time = (_date: date): string => [%mel.raw "_date.toISOString()"];
};
