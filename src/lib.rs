pub fn home() -> maud::Markup {
    maud::html! {
        (maud::DOCTYPE)

        html lang="en" {
            head {
                meta charset="utf-8";
                meta name="viewport" content="width=device-width";
                meta name="description" content=">w<";
                meta name="theme-color" content="#d895ee";

                title { "~rini/" }
            }

            body {
                div id="chat" {}
                input id="chat_input";

                script src="dist/index.js" {}
            }
        }
    }
}
