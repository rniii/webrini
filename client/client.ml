open Chat.Websocket
open Chat.Html
open Chat.Utils

type msg =
  | Chat of string * string
  | Info of string
  | Join of string
  | Error of string

let handle_msg msg =
  let now = date () in
  let body =
    match msg with
    | Chat (author, content) ->
        fragment
          [
            span [ style "color:#ebc" ] [ text author; text ": " ];
            span [] [ text content ];
            br [];
          ]
    | Info content -> span [] [ text content ]
    | Join user -> span [] [ text user; text " joined" ]
    | Error e -> span [] [ text e ]
  in
  let body =
    div []
      [
        node "time" [ attr "datetime" (iso_time now) ] [ text (timestamp now) ];
        body;
      ]
  in
  push body

type raw_msg = { tag : string; author : string; text : string }

let ws = connect_ws ([%mel.raw "location.href.replace('http', 'ws')"] ^ "/chat")

let _ =
  on_message ws (fun _e ->
      let msg = [%mel.raw "JSON.parse(_e.data)"] in
      handle_msg
        (match msg.tag with
        | "Chat" -> Chat (msg.author, msg.text)
        | "Info" -> Info msg.text
        | tag -> Error ("Unknown server message" ^ tag)))

let _ = on_error ws (fun e -> handle_msg (Error e.reason))

let _ =
  on_input chat_input (fun _e ->
      send_message ws
        [%mel.raw "JSON.stringify({ tag: 'Send', text: _e.target.value })"];
      [%mel.raw "_e.target.value = ''"])
