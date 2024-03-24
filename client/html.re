type t = string;
type attr = string;
type dom_node;

external chat_form: dom_node = "chat_form";
external chat_input: dom_node = "chat_input";
[@mel.set] external on_submit: (dom_node, 'a => 'b) => unit = "onsubmit";

let push = (_html: t): unit => {
  let scrollBottom = [%mel.raw
    "chat.scrollHeight - chat.clientHeight - chat.scrollTop"
  ];

  let () = [%mel.raw "chat.insertAdjacentHTML('beforeend', _html)"];

  if (scrollBottom <= 1) {
    let () = [%mel.raw "chat.lastElementChild.scrollIntoView()"];
    ();
  };
};

let text = (_src: string): t => [%mel.raw
  "_src.replace(/[\"&<>]/g, c => `&#${c.charCodeAt()};`)"
];

let rec fragment: list(t) => t =
  fun
  | [] => ""
  | [head, ...tail] => head ++ fragment(tail);

let attr = (name: string, value: string): attr =>
  " " ++ name ++ "=\"" ++ value ++ "\"";

let node_ = (tag: string, attrs: list(attr)): t =>
  "<" ++ tag ++ fragment(attrs) ++ ">";

let node = (tag: string, attrs: list(attr), children: list(t)): t =>
  "<"
  ++ tag
  ++ fragment(attrs)
  ++ ">"
  ++ fragment(children)
  ++ "</"
  ++ tag
  ++ ">";

let div = node("div");
let span = node("span");

let br = node_("br");

let style = attr("style");
