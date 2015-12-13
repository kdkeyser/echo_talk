package main

import (
        //"runtime"
        //"fmt"
        "net"
)

type ExitState int

const (
        OFFTRACK ExitState = iota
        NL
        E
        X
        I
        T
        CR
        MATCH
)

func (es ExitState) to_string () string {
        switch es {
                case OFFTRACK : { return "OFFTRACK" }
                case NL : { return "NL" }
                case E : { return "E" }
                case X : { return "X" }
                case I : { return "I" }
                case T : { return "T" }
                case CR : { return "CR" }
                case MATCH : { return "MATCH" }
        }

        return ""
}

type ExitMatcher struct {
        state ExitState
}

func newExitMatcher () *ExitMatcher {
        em := new (ExitMatcher)
        em.state = NL
        return em
}

func (em *ExitMatcher) trans (c byte) {
        //from_state := em.state

        switch em.state {
                case NL: { if c == 101 { em.state = E } else { em.state = OFFTRACK } }
                case E: { if c == 120 { em.state = X } else { em.state = OFFTRACK } }
                case X: { if c == 105 { em.state = I } else { em.state = OFFTRACK } }
                case I: { if c == 116 { em.state = T } else { em.state = OFFTRACK } }
                case T: { if c == 13 { em.state = CR } else { em.state = OFFTRACK } }
                case CR: { if c == 10 { em.state = MATCH } else { em.state = OFFTRACK } }
                case MATCH: { return }
                default: { if c == 10 { em.state = NL } else { em.state = OFFTRACK } }
        }

        //to_state := em.state

        //fmt.Printf("%d: %s -> %s\n", c, from_state.to_string (), to_state.to_string ());

        return
}

func (em *ExitMatcher) try_match (cs []byte) bool {
        for _, c := range cs {
                em.trans (c)
        }

        return (em.state == MATCH)
}

func echo(s net.Conn, i int) {
        defer s.Close()
        em := newExitMatcher ()
        recv := make([]byte, 65536)
        // fmt.Printf("%d: %v <-> %v\n", i, s.LocalAddr(), s.RemoteAddr())
        for {
                n, e := s.Read(recv[:])
                if e != nil {
                        goto Error
                }
                matched := em.try_match (recv[0:n])
                if matched {
                        break
                } 
                s.Write(recv[0:n])
        }
        Error:
        // fmt.Printf("%d: closed\n", i)
}

func main() {
        // fmt.Printf("%d\n", runtime.NumCPU())
        l, e := net.Listen("tcp", ":6000")
        for i := 0; e == nil; i++ {
                var s net.Conn
                s, e = l.Accept()
                go echo(s, i)
        }
}
