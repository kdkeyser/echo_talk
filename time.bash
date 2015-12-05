#!/bin/bash
set -ex

TEST_CONNECTION_NUMBER=10
TEST_MESSAGE_LENGTH=30
THREADS=2
TEST_DURATION=2

run_driver () {
        cd go_driver
        export GOMAXPROCS=2
        ./go_driver -a "127.0.0.1:6000" -c ${TEST_CONNECTION_NUMBER} -l ${TEST_MESSAGE_LENGTH} -t ${TEST_DURATION}
        cd -
}

test_go () {
        cd go_server
        export GOMAXPROCS=${THREADS}
        # ./go_server > /dev/null 2> /dev/null &
        ./go_server &
        GO_SERVER_PID=$!
        sleep 1
        cd -

        run_driver

        kill ${GO_SERVER_PID}
}

test_conduit () {
        cd echo-server-conduit
        # stack exec Echo -- +RST -N ${THREADS} -RTS > /dev/null 2> /dev/null &
        stack exec Echo -- +RST -N ${THREADS} -N3 -A16M -kc4k -RTS &
        ECHO_SERVER_CONDUIT_PID=$!
        sleep 1
        cd -

        run_driver

        kill ${ECHO_SERVER_CONDUIT_PID}
}

test_boost () {
        cd boost_asio_echo/dist/Release/GNU-Linux
        # ./boost_asio_echo 127.0.0.1 6000 ${THREADS} > /dev/null 2> /dev/null &
        ./boost_asio_echo 127.0.0.1 6000 ${THREADS} 0 &
        BOOST_ASIO_ECHO_PID=$!
        sleep 1
        cd -

        run_driver

        kill ${BOOST_ASIO_ECHO_PID}
}

test_coio () {
        cd rust-echo-server/target/release
        #./coio-tcp-echo-server -t ${THREADS} --bind 127.0.0.1:6000 > /dev/null 2> /dev/null &
        ./coio-tcp-echo-server -t ${THREADS} --bind 127.0.0.1:6000 &
        COIO_TCP_ECHO_SERVER_PID=$!
        sleep 1
        cd -

        run_driver

        kill ${COIO_TCP_ECHO_SERVER_PID}
}

pkill go_server || true
pkill stack || true
pkill boost_asio_echo || true
pkill coio-tcp-echo-server || true

echo GO ------------------------------------------------------
test_go
sleep 2

echo CONDUIT ------------------------------------------------------
test_conduit
sleep 2
 
echo BOOST ------------------------------------------------------
test_boost
sleep 2

# echo COIO ------------------------------------------------------
# test_coio
# sleep 2




