#!/bin/bash
set -ex

cd rust-echo-server
cargo build --release
cd -

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



