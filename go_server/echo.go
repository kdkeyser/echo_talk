package main

import (
    "runtime"
	"fmt"
	"net"
    "bytes"
)

func echo(s net.Conn, i int) {
	defer s.Close()
    recv := make([]byte, 65536)
	// fmt.Printf("%d: %v <-> %v\n", i, s.LocalAddr(), s.RemoteAddr())
	for {
        read := 0
        for i := 0; ; { 
		    n, e := s.Read(recv[i:])
		    if e != nil {
		    	goto Error
		    }
            i += n
            if recv[i-1] == '\n' {
                read = i
                break
            }
        }
        if bytes.Equal(recv, []byte("exit\n")) {
            break
        }
        s.Write(recv[0:read])
	}
Error:
	// fmt.Printf("%d: closed\n", i)
}

func main() {
    fmt.Printf("%d\n", runtime.NumCPU())
	l, e := net.Listen("tcp", ":6000")
	for i := 0; e == nil; i++ {
		var s net.Conn
		s, e = l.Accept()
		go echo(s, i)
	}
}
