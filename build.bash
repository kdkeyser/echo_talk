#!/bin/bash
set -ex

cd rust-echo-server
cargo build --release
cd -

cd go_server
go build
cd -

cd go_driver
go build
cd -

cd echo-server-conduit
stack build
cd -

cd boost_asio_echo
make all
cd -



