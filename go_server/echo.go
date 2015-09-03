package main

import (
	"fmt"
	"net"
	"bufio"
        "bytes"
)

func echo(s net.Conn, i int) {
	defer s.Close()
	fmt.Printf("%d: %v <-> %v\n", i, s.LocalAddr(), s.RemoteAddr())
	b := bufio.NewReader(s)
        exit_arr := []byte("exit\r\n")
	for {
		line, e := b.ReadBytes('\n')
		if e != nil {
			break
		}
                if bytes.Equal(line, exit_arr) {
                        break
                }

		s.Write(line)
	}
	fmt.Printf("%d: closed\n", i)
}

func main() {
	l, e := net.Listen("tcp", ":6000")
	for i := 0; e == nil; i++ {
		var s net.Conn
		s, e = l.Accept()
		go echo(s, i)
	}
}
