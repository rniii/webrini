port module Chat exposing (..)

import Json.Decode exposing (..)
import Json.Encode as E


port send : Value -> Cmd msg


port recv : (Value -> msg) -> Sub msg


port input : (String -> msg) -> Sub msg


port push : String -> Cmd msg


type ClientMsg
    = Send String


type ServerMsg
    = Text String String
    | Info String


encode : ClientMsg -> Value
encode msg =
    E.object <|
        case msg of
            Send text ->
                [ ( "tag", E.string "Send" ), ( "text", E.string text ) ]


decode : Decoder ServerMsg
decode =
    field "tag" string |> andThen decodeTag


decodeTag : String -> Decoder ServerMsg
decodeTag t =
    case t of
        "Chat" ->
            map2 Text (field "author" string) (field "text" string)

        "Info" ->
            map Info (field "text" string)

        _ ->
            "Unknown server message " ++ t ++ " received" |> fail
