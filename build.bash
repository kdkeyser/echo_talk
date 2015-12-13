#!/bin/bash
set -ex

cd go_server
/usr/local/go/bin/go build
cd -

cd go_driver
/usr/local/go/bin/go build
cd -

cd echo-server-conduit
stack build -j4
cd -

cd boost_asio_echo
make all -j4
cd -

cd echo-server-haskell
stack build -j4
cd -

cd ocaml_server
~/.opam/4.02.3/bin/ocamlfind ocamlopt -w A -syntax camlp4o -linkpkg -package lwt,lwt.syntax,lwt.unix server.ml -o ocaml-server
cd -

cd rust-echo-server
cargo build --release
cd -



