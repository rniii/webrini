// rini <https://rinici.de>
//
// Copyright (c) 2024 rini
// SPDX-License-Identifier: Apache-2.0

use std::path::Path;
use std::process::Command;
use std::{fs, io};

fn index() -> maud::Markup {
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

                script src="index.js" {}
            }
        }
    }
}

fn not_found() -> maud::Markup {
    maud::html! {}
}

macro_rules! pages {
    ($($name:literal $page:ident)*) => {
        [$(($name, $page as fn() -> maud::Markup)),*]
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
