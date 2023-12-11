use std::env;
use std::path::PathBuf;
use std::process::exit;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Standard {
    C11,
    Gnu11,
    Clang11,
}

pub struct Cli {
    /// Print output from intermediate stages
    pub verbose: bool,

    /// Output file name
    pub output: Option<PathBuf>,

    /// Define macro
    pub define: Vec<String>,

    /// Add directory to the end of the list of include search paths
    pub include: Vec<String>,

    /// Add directory to SYSTEM include search path
    pub isystem: Vec<String>,

    /// Add directory to QUOTE include search path
    pub iquote: Vec<String>,

    /// C dialect
    dialect: Option<Standard>,

    /// Show staticstics
    pub show_stats: bool,

    /// Input file name
    input: Option<PathBuf>,

    /// Produce assembly file
    pub produce_assembly: bool,
}

impl Cli {
    pub fn parse() -> Self {
        let mut args = env::args();
        let prog_name = args.next().unwrap();

        let mut result = Self::new();
        loop {
            let arg = if let Some(arg) = args.next() {
                arg
            } else {
                break;
            };
            match arg.as_str() {
                "-h" | "-help" | "--help" => {
                    print_help(&prog_name);
                    exit(0);
                }

                "-v" => result.verbose = true,

                "-o" => {
                    let path = if let Some(path) = args.next() {
                        path
                    } else {
                        die("Expected filename after '-o'");
                    };
                    result.set_output(&path);
                }
                s if s.starts_with("-o") => {
                    let path = &s[2..];
                    result.set_output(path);
                }

                "-D" => {
                    let val = if let Some(val) = args.next() {
                        val
                    } else {
                        die("Expected value after '-D'");
                    };
                    result.add_define(&val);
                }
                s if s.starts_with("-D") => {
                    let val = &s[2..];
                    result.add_define(val);
                }

                "-I" => {
                    let val = if let Some(val) = args.next() {
                        val
                    } else {
                        die("Expected value after '-I'");
                    };
                    result.add_include(&val);
                }
                s if s.starts_with("-I") => {
                    let val = &s[2..];
                    result.add_include(val);
                }

                "-isystem" => {
                    let val = if let Some(val) = args.next() {
                        val
                    } else {
                        die("Expected value after '-isystem'");
                    };
                    result.add_isystem(&val);
                }

                "-iquote" => {
                    let val = if let Some(val) = args.next() {
                        val
                    } else {
                        die("Expected value after '-iquote'");
                    };
                    result.add_iquote(&val);
                }

                "-std=c11" => result.set_dialect(Standard::C11),
                "-std=gnu11" => result.set_dialect(Standard::Gnu11),
                "-std=clang11" => result.set_dialect(Standard::Clang11),
                s if s.starts_with("-std=") => die("Unrecognized C dialect"),

                "-show-stats" => result.show_stats = true,

                "-S" => result.produce_assembly = true,

                "-c" => (),

                s if s.starts_with("-") => die(&format!("Unrecognized parameter: {}", s)),

                s => result.set_input(s),
            }
        }

        if result.input.is_none() {
            die("No input files");
        }

        result
    }

    pub fn get_verbose(&self) -> bool {
        self.verbose
    }

    pub fn get_dialect(&self) -> Standard {
        self.dialect.unwrap_or(Standard::Gnu11)
    }

    pub fn get_input(&self) -> &PathBuf {
        self.input.as_ref().unwrap()
    }

    fn new() -> Self {
        Self {
            verbose: false,
            output: None,
            define: Vec::new(),
            include: Vec::new(),
            isystem: Vec::new(),
            iquote: Vec::new(),
            dialect: None,
            show_stats: false,
            input: None,
            produce_assembly: false,
        }
    }

    fn set_output(&mut self, output: &str) {
        if self.output.is_some() {
            die("Output file must not be specified more than once");
        }
        self.output = Some(PathBuf::from(output));
    }

    fn set_input(&mut self, input: &str) {
        if self.input.is_some() {
            die("Only one input file is supported");
        }
        self.input = Some(PathBuf::from(input));
    }

    fn set_dialect(&mut self, d: Standard) {
        if self.dialect.is_some() {
            die("C dialect must not be specified more than once");
        }
        self.dialect = Some(d);
    }

    fn add_define(&mut self, val: &str) {
        self.define.push(val.to_string());
    }

    fn add_include(&mut self, val: &str) {
        self.include.push(val.to_string());
    }

    fn add_isystem(&mut self, val: &str) {
        self.isystem.push(val.to_string());
    }

    fn add_iquote(&mut self, val: &str) {
        self.iquote.push(val.to_string());
    }
}

fn die(msg: &str) -> ! {
    println!("{}", msg);
    exit(1);
}

fn print_help(prog_name: &str) {
    print!(
        "Usage: {} [OPTIONS] <INPUT>

Arguments:
  <INPUT>  Input file name

Options:
  -v                       Print output from intermediate stages
  -S                       Produce an assembly file
  -o <OUTPUT>              Output file name
  -D<DEFINE>               Define macro
  -I <dir>                 Add directory to the end of the list of include search paths
  -isystem <dir>           Add directory to SYSTEM include search path
  -iquote <dir>            Add directory to QUOTE include search path
  -std=<standard>          C dialect [possible values: c11, gnu11, clang11]. Default gnu11.
  -show-stats              Show staticstics
  -h, --help               Print help",
        prog_name
    );
}
