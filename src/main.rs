// rini <https://rinici.de>
//
// Copyright (c) 2024 rini
// SPDX-License-Identifier: Apache-2.0

use std::path::Path;
use std::{fs, io};

type Page = fn() -> maud::Markup;

const PAGES: &[(&str, Page)] = &[("index", rini::home)];

fn main() -> io::Result<()> {
    let dir = Path::new("pages");

    fs::create_dir_all(dir)?;

    for (file, page) in PAGES {
        fs::write(dir.join(file).with_extension("html"), page().into_string())?;
    }

    Ok(())
}
