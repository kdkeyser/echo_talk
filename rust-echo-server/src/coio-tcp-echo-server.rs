extern crate clap;
#[macro_use]
extern crate log;
extern crate env_logger;

extern crate coio;

use clap::{Arg, App};

use coio::Scheduler;
use coio::net::tcp::TcpListener;

#[derive (Clone, Copy, Debug)]
enum ExitState { OFFTRACK, NL, E, X, I, T, CR, MATCH }

struct ExitMatcher {
    state : ExitState
}

impl ExitMatcher {
    fn new () -> ExitMatcher {
        info! ("Creating ExitMatcher");
        ExitMatcher { state : ExitState::NL }
    }

    #[inline]
    fn goto_state (&mut self, s : ExitState) -> () {
        self.state = s;
    }

    #[inline]
    fn trans (&mut self, c : u8) -> () {
        let s = self.state;
        match (s, c) {
            (ExitState::NL, 101) => { self.goto_state (ExitState::E) }
            (ExitState::E, 120) =>  { self.goto_state (ExitState::X) }
            (ExitState::X, 105) =>  { self.goto_state (ExitState::I) }
            (ExitState::I, 116) =>  { self.goto_state (ExitState::T) }
            (ExitState::T, 13) =>  { self.goto_state (ExitState::CR) }
            (ExitState::CR, 10) =>  { self.goto_state (ExitState::MATCH) }
            (ExitState::MATCH, _) => { () }
            (_, 10) => { self.goto_state (ExitState::NL) }
            (_, _) => { self.goto_state (ExitState::OFFTRACK) }
        };
    }

    #[inline]
    fn try_match (&mut self, buf: &[u8]) -> bool {
        for c in buf {
            self.trans (*c);
        }
        match self.state {
            ExitState::MATCH => { true }
            _ => { false }
        }
    }
}

fn main() {
    env_logger::init().unwrap();
    info! ("Starting up");

    let matches = App::new("coio-tcp-echo")
                      .version(env!("CARGO_PKG_VERSION"))
                      .author("Y. T. Chung <zonyitoo@gmail.com>")
                      .arg(Arg::with_name("BIND")
                               .short("b")
                               .long("bind")
                               .takes_value(true)
                               .required(true)
                               .help("Listening on this address"))
                      .arg(Arg::with_name("THREADS")
                               .short("t")
                               .long("threads")
                               .takes_value(true)
                               .help("Number of worker threads"))
                      .arg(Arg::with_name("BUFFERSIZE")
                               .short("s")
                               .long("buffer_size")
                               .takes_value(true)
                               .help("Size of buffer per connection"))
                      .get_matches();

    let bind_addr = matches.value_of("BIND").unwrap().to_owned();
    let num_workers = matches.value_of("THREADS").unwrap_or("1").parse().unwrap();
    let buffer_size = matches.value_of("BUFFERSIZE").unwrap_or("16384").parse().unwrap();

    info! ("Server started at address {:?}", bind_addr);
    info! ("Starting with {:?} workers", num_workers);
    info! ("Using buffer size of {:?} per connection", buffer_size);

    Scheduler::new()
        .with_workers(num_workers)
        .run(move || {
            let server = TcpListener::bind(&bind_addr[..]).unwrap();

            info!("Listening on {:?}", server.local_addr().unwrap());

            for stream in server.incoming() {
                use std::io::{Read, Write};

                let (mut stream, addr) = stream.unwrap();
                info!("Accept connection: {:?}", addr);

                Scheduler::spawn(move || {
                    let mut buf = vec![0; buffer_size];

                    let mut em = ExitMatcher::new ();

                    loop {
                        debug!("Trying to Read...");
                        match stream.read(&mut buf) {
                            Ok(0) => {
                                info!("EOF received, going to close: {:?}", addr);
                                break;
                            }
                            Ok(len) => {
                                stream.write_all(&buf[0..len]).unwrap();
                                if em.try_match (&buf[0..len]) {
                                    info!("'exit' matched, closing connection");
                                    break;
                                }
                            }
                            Err(err) => {
                                panic!("Error occurs: {:?}", err);
                            }
                        }
                    }

                    info!("{:?} closed", addr);
                });
            }
        })
        .unwrap();
}
