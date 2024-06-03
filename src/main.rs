//! A wrapper around the `java_class_manipulation` library which loads a class
//! file then writes the loaded file.

use std::{env, fs::OpenOptions, io};

extern crate clap;

use clap::{crate_authors, crate_version, Arg, ArgAction, Command};

use java_class_manipulation::JavaClass;

fn cli() -> Command<'static> {
    let in_arg = Arg::new("input")
        .short('i')
        .long("input")
        .value_name("FILE")
        .help("Sets the class file to read")
        .takes_value(true)
        .action(ArgAction::Set)
        .required(true);
    let out_arg = Arg::new("output")
        .short('o')
        .long("output")
        .value_name("FILE")
        .help("Sets the class file to write")
        .takes_value(true)
        .action(ArgAction::Set)
        .required(true);
    #[cfg(feature = "map_file")]
    let map_arg = Arg::new("map")
        .short('m')
        .long("map")
        .value_name("FILE")
        .help("Sets the file to use for renaming the elements of the class")
        .takes_value(true)
        .action(ArgAction::Set);
    let app = Command::new("Java Class Manipulator")
        .version(crate_version!())
        .author(crate_authors!())
        .about("Reads a Java class file, parses it, then writes the parsed class.")
        .arg(in_arg)
        .arg(out_arg);
    #[cfg(feature = "map_file")]
    let app = app.arg(map_arg);
    app
}

struct Config {
    in_file: String,
    out_file: String,
    _map_file: Option<String>,
}

fn parse_args() -> Config {
    let matches = cli().get_matches();
    let in_file = matches
        .get_one::<String>("input")
        .expect("Missing input file")
        .clone();
    let out_file = matches
        .get_one::<String>("output")
        .expect("Missing output file")
        .clone();
    let _map_file = if cfg!(feature = "map_file") {
        matches.get_one::<String>("map").cloned()
    } else {
        None
    };
    Config {
        in_file,
        out_file,
        _map_file,
    }
}

fn main() -> io::Result<()> {
    let config = parse_args();

    let mut fin = OpenOptions::new().read(true).open(config.in_file)?;
    let class = JavaClass::read(&mut fin).map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

    let mut fout = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(config.out_file)?;
    #[cfg(debug_assertions)]
    dbg!(&class);
    class
        .write(&mut fout)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_cli() {
        cli().debug_assert();
    }
}
