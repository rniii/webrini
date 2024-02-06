module Main exposing (main)

import Chat
import Json.Decode exposing (decodeValue, errorToString)
import Json.Encode exposing (Value)


push : Html -> Cmd msg
push =
    render >> Chat.push


main : Program () Model Msg
main =
    Platform.worker
        { init = always ( (), Cmd.none )
        , subscriptions = always (Sub.batch [ Chat.recv Recv, Chat.input Input ])
        , update = \msg -> always ( (), update msg )
        }


type alias Model =
    ()


type Msg
    = Recv Value
    | Input String


update : Msg -> Cmd Msg
update msg =
    case msg of
        Recv m ->
            case decodeValue Chat.decode m of
                Ok v ->
                    renderMsg v |> push

                Err e ->
                    fragment
                        [ span [] [ errorToString e |> text ], br [] ]
                        |> push

        Input s ->
            Chat.send <| Chat.encode <| Chat.Send s


renderMsg : Chat.ServerMsg -> Html
renderMsg msg =
    case msg of
        Chat.Text author content ->
            fragment
                [ span [] [ text author ]
                , span [] [ text ": " ]
                , span [] [ text content ]
                , br []
                ]

        Chat.Info content ->
            fragment
                [ span [] [ text content ]
                , br []
                ]


type Html
    = Html String


text : String -> Html
text =
    escapeHtml >> Html


span : List ( String, String ) -> List Html -> Html
span =
    el "span"


br : List ( String, String ) -> Html
br =
    el_ "br"


escapeHtml : String -> String
escapeHtml =
    escape "\"&<>"


escape : String -> String -> String
escape chars =
    String.foldr
        (\c ->
            if String.fromChar c |> String.contains chars then
                String.cons '\\' << String.cons c

            else
                String.cons c
        )
        ""


fragment : List Html -> Html
fragment =
    Html << String.concat << List.map render


el : String -> List ( String, String ) -> List Html -> Html
el tag attrs children =
    "<" ++ tag ++ attrs_ attrs ++ ">" ++ String.concat (List.map render children) ++ "</" ++ tag ++ ">" |> Html


el_ : String -> List ( String, String ) -> Html
el_ tag attrs =
    "<" ++ tag ++ attrs_ attrs ++ ">" |> Html


attrs_ : List ( String, String ) -> String
attrs_ =
    String.concat << List.map (\( k, v ) -> " " ++ escapeHtml k ++ "=" ++ escape "\"\\" v)


render : Html -> String
render html =
    case html of
        Html h ->
            h
