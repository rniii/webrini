(*
 * rini <https//rinici.de>
 *
 * Copyright (c) 2024 rini
 * SPDX-License-Identifier: Apache-2.0
 *)

open Chat.Websocket
open Chat.Utils
open Html

type msg =
  | Chat of string * string
  | Info of string
  | Join of string
  | Error of string

type client_msg = Send of string | Command of string * string list

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
    | Join user ->
        fragment
          [
            span [ style "color:#ebc" ] [ text user ];
            span [] [ text " joined" ];
          ]
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

let send_msg ws msg =
  let data =
    match msg with
    | Send text ->
        stringify [%mel.obj { tag = "Chat"; src = [%mel.raw "null"]; text }]
    | Command (name, args) -> stringify [%mel.obj { tag = "Comm"; name; args }]
  in

  send_data ws data

type raw_msg = {
  tag : string;
  text : string;
  src : string;
  args : string array;
}

let () =
  let ws =
    connect_ws ([%mel.raw "location.origin.replace('http', 'ws')"] ^ "/chat")
  in

  on_open ws (fun _ ->
      let nick = [%mel.raw "localStorage.chatNick"] in
      send_data ws (stringify [%mel.obj { nick }]));

  on_message ws (fun e ->
      let raw = parse e.data in

      handle_msg
        (match raw.tag with
        | "Chat" -> Chat (raw.src, raw.text)
        | "Comm" -> Join raw.args.(0)
        | tag -> Error ("Unknown server message: " ^ tag)));

  on_error ws (fun e -> handle_msg (Error ("connection closed: " ^ e.reason)));

  on_submit chat_form (fun () ->
      let text = [%mel.raw "chat_input.value"] in
      send_msg ws (Send text);
      [%mel.raw "chat_input.value = '', false"])
