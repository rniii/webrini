// rini <https://rinici.de>
//
// Copyright (c) 2024 rini
// SPDX-License-Identifier: Apache-2.0

use std::path::Path;
use std::process::Command;
use std::{fs, io};

use maud::Markup;

fn index() -> Markup {
    page(maud::html! {
        main {
            h1 { "~rini" }

            p { "awa" }
        }

        section {
            h2 { "cat" }

            #chat {}

            input placeholder="Type.." aria-label="Send a message" #chat_input;
        }

        script src="index.js" {}
    })
}

fn not_found() -> Markup {
    page(maud::html! {})
}

fn page(body: Markup) -> Markup {
    maud::html! {
        (maud::DOCTYPE)

        html lang="en" {
            head {
                meta charset="utf-8";
                meta name="viewport" content="width=device-width";
                meta name="description" content=">w<";
                meta name="theme-color" content="#d895ee";
                link rel="stylesheet" href="index.css";

                title { "~rini/" }
            }

            body { (body) }
        }
    }
}

macro_rules! pages {
    ($($name:literal $page:ident)*) => {
        [$(($name, $page as fn() -> Markup)),*]
    };
}

fn main() -> io::Result<()> {
    let pages = pages! {
        "index" index
        "404"   not_found
    };

    let dir = Path::new("pages");
    fs::create_dir_all(dir)?;

    for (file, page) in pages {
        let path = dir.join(file).with_extension("html");

        fs::write(&path, page().into_string())?;

        Command::new("zopfli").arg(path).spawn()?.wait()?;
    }

    Ok(())
}
