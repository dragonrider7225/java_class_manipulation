//! A wrapper around the `java_class_manipulation` library which loads a class
//! file then writes the loaded file.

use std::{env, fs::OpenOptions, io};

#[macro_use]
extern crate clap;

use clap::{App, Arg};

use java_class_manipulation::JavaClass;

struct Config {
    in_file: String,
    out_file: String,
    _map_file: Option<String>,
}

fn parse_args() -> Config {
    let in_arg = Arg::with_name("input")
        .short("i")
        .long("input")
        .value_name("FILE")
        .help("Sets the class file to read")
        .takes_value(true)
        .required(true);
    let out_arg = Arg::with_name("output")
        .short("o")
        .long("output")
        .value_name("FILE")
        .help("Sets the class file to write")
        .takes_value(true)
        .required(true);
    #[cfg(feature = "map_file")]
    let map_arg = Arg::with_name("map")
        .short("m")
        .long("map")
        .value_name("FILE")
        .help("Sets the file to use for renaming the elements of the class")
        .takes_value(true);
    let args = App::new("Java Class Manipulator")
        .version("0.1.0")
        .author(crate_authors!())
        .about("Reads a Java class file, parses it, then writes the parsed class.")
        .arg(in_arg)
        .arg(out_arg);
    #[cfg(feature = "map_file")]
    let args = args.arg(map_arg);
    let matches = args.get_matches();
    let in_file = matches
        .value_of("input")
        .expect("Missing input file")
        .to_string();
    let out_file = matches
        .value_of("output")
        .expect("Missing output file")
        .to_string();
    let _map_file = matches.value_of("map").map(|s| s.to_string());
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
    class
        .write(&mut fout)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

    Ok(())
}
