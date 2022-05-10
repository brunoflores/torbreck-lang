use std::env;
use std::error::Error;
use std::fs;
use std::process;

fn usage() -> &'static str {
    "Usage: breckrun filename"
}

struct Config {
    filename: String,
}

impl Config {
    fn new(args: &[String]) -> Result<Config, &'static str> {
        if args.len() != 2 {
            return Err(usage());
        }

        let filename = args[1].clone();
        Ok(Config { filename })
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let config = Config::new(&args).unwrap_or_else(|usage| {
        println!("{}", usage);
        process::exit(1);
    });
    if let Err(e) = run(config) {
        println!("error: {}", e);
        process::exit(1);
    }
}

fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(config.filename)?;
    println!("with text:\n{}", contents);
    Ok(())
}
