package main

import (
    "flag"
    "fmt"
    "log"
    "net"
    "sync"
    "sync/atomic"
    "time"
    "runtime"
)

var (
    targetAddr  = flag.String("a", "127.0.0.1:12345", "target echo server address")
    testMsgLen  = flag.Int("l", 26, "test message length")
    testConnNum = flag.Int("c", 50, "test connection number")
    testSeconds = flag.Int("t", 10, "test duration in seconds")
)

func HandleError(err error) (b bool) {
    if err != nil {
        // notice that we're using 1, so it will actually log where
        // the error happened, 0 = this function, we don't want that.
        _, fn, line, _ := runtime.Caller(1)
        log.Printf("[error] %s:%d %v", fn, line, err)
        b = true
    }
    return
}

//this logs the function name as well.
func FancyHandleError(err error) (b bool) {
    if err != nil {
        // notice that we're using 1, so it will actually log the where
        // the error happened, 0 = this function, we don't want that.
        pc, fn, line, _ := runtime.Caller(1)

        log.Printf("[error] in %s[%s:%d] %v", runtime.FuncForPC(pc).Name(), fn, line, err)
        b = true
    }
    return
}


func main() {
    flag.Parse()

    var (
        outNum uint64
        inNum  uint64
        stop   uint64
    )

    msg := make([]byte, *testMsgLen)

    go func() {
        time.Sleep(time.Second * time.Duration(*testSeconds))
        atomic.StoreUint64(&stop, 1)
    }()

    wg := new(sync.WaitGroup)

    for i := 0; i < *testConnNum; i++ {
        wg.Add(1)

        go func() {
            if conn, err := net.DialTimeout("tcp", *targetAddr, time.Minute*99999); err == nil {
                l := len(msg)
                msg[l-1] = '\n'
                recv := make([]byte, l)

                for {
                    for i := 0; i < l ; {
                        written, err := conn.Write(msg[i:l]);
                        if err != nil {
                            HandleError(err)
                            break
                        }
                        i += written
                    }
                    

                    atomic.AddUint64(&outNum, 1)

                    if atomic.LoadUint64(&stop) == 1 {
                        break
                    }

                    for i := 0; i < l ; {
                        read, err := conn.Read(recv[i:l])
                        if err != nil {
                            HandleError(err)
                            break
                        }
                        i += read
                    }

                    atomic.AddUint64(&inNum, 1)

                    if atomic.LoadUint64(&stop) == 1 {
                        break
                    }
                }
            } else {
                HandleError(err)
            }

            wg.Done()
        }()
    }

    wg.Wait()

    fmt.Println("Benchmarking:", *targetAddr)
    fmt.Println(*testConnNum, "clients, running", *testMsgLen, "bytes,", *testSeconds, "sec.")
    fmt.Println()
    fmt.Println("Speed:", outNum/uint64(*testSeconds), "request/sec,", inNum/uint64(*testSeconds), "response/sec")
    fmt.Println("Requests:", outNum)
    fmt.Println("Responses:", inNum)
}
