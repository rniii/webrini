// rini <https://rinici.de>
//
// Copyright (c) 2024 rini
// SPDX-License-Identifier: Apache-2.0

module Websocket = {
  type t;
  type event = {data: string};
  type error = {reason: string};

  [@mel.new] external connect_ws: string => t = "WebSocket";
  [@mel.set] external on_message: (t, event => unit) => unit = "onmessage";
  [@mel.set] external on_error: (t, error => unit) => unit = "onerror";
  [@mel.set] external on_open: (t, unit => unit) => unit = "onopen";
  [@mel.send] external send_data: (t, string) => unit = "send";
};

module Utils = {
  type date;

  [@mel.new] external date: unit => date = "Date";

  let timestamp = (_date: date): string => [%mel.raw
    "_date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })"
  ];

  let iso_time = (_date: date): string => [%mel.raw "_date.toISOString()"];

  external stringify: 'a => string = "JSON.stringify";
  external parse: string => 'a = "JSON.parse";
};
