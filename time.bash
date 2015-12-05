#!/bin/bash
set -e

TEST_CONNECTION_NUMBER=1000
TEST_MESSAGE_LENGTH=1000
DRIVER_THREADS=2
SERVER_THREADS=3
TEST_DURATION=10

run_driver () {
        cd go_driver
        export GOMAXPROCS=${DRIVER_THREADS}
        ./go_driver -a "127.0.0.1:6000" -c ${TEST_CONNECTION_NUMBER} -l ${TEST_MESSAGE_LENGTH} -t ${TEST_DURATION}
        cd -
}

test_go () {
        cd go_server
        export GOMAXPROCS=${SERVER_THREADS}
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
        # stack exec Echo -- +RST -N ${SERVER_THREADS} -RTS > /dev/null 2> /dev/null &
        stack exec Echo -- +RST -N ${SERVER_THREADS} -A16M -kc4k -RTS &
        ECHO_SERVER_CONDUIT_PID=$!
        sleep 1
        cd -

        run_driver

        kill ${ECHO_SERVER_CONDUIT_PID}
}

test_boost () {
        cd boost_asio_echo/dist/Release/GNU-Linux
        # ./boost_asio_echo 127.0.0.1 6000 ${SERVER_THREADS} > /dev/null 2> /dev/null &
        ./boost_asio_echo 127.0.0.1 6000 ${SERVER_THREADS} 0 &
        BOOST_ASIO_ECHO_PID=$!
        sleep 1
        cd -

        run_driver

        kill ${BOOST_ASIO_ECHO_PID}
}

test_coio () {
        cd rust-echo-server/target/release
        #./coio-tcp-echo-server -t ${SERVER_THREADS} --bind 127.0.0.1:6000 > /dev/null 2> /dev/null &
        ./coio-tcp-echo-server -t ${SERVER_THREADS} --bind 127.0.0.1:6000 &
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

echo BOOST ------------------------------------------------------
test_boost
sleep 2

echo GO ------------------------------------------------------
test_go
sleep 2

echo CONDUIT ------------------------------------------------------
test_conduit
sleep 2
 
# echo COIO ------------------------------------------------------
# test_coio
# sleep 2




