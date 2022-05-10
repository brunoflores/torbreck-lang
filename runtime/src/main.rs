use std::env;
use std::process;

use runtime::Config;

fn usage() -> &'static str {
    "Usage: breckrun filename"
}

fn crash(msg: &str) -> ! {
    println!("{}", msg);
    process::exit(1);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let config = match Config::new(&args) {
        Err(runtime::ConfigError) => crash(usage()),
        Ok(config) => config,
    };
    if let Err(e) = runtime::run(config) {
        crash(&format!("error: {}", e));
    }
}
