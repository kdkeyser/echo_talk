(* ocamlfind c -w A -linkpkg -package lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax myecho.ml -o myecho *)
(* This code refers to https://github.com/avsm/ocaml-cohttpserver/blob/master/server/http_tcp_server.ml *)
open Lwt

let server_port = 6000
let backlog = 1024

let try_close chan =
  catch (fun () -> Lwt_io.close chan)
  (function _ -> return ())

let init_socket sockaddr =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.bind socket sockaddr;
  Lwt_unix.listen socket backlog;
  socket

let process socket ~callback =
  let rec _process () =
    Lwt_unix.accept socket >>=
      (fun (socket_cli, _) ->
        let inchan = Lwt_io.of_fd ~mode:Lwt_io.input socket_cli in
        let outchan = Lwt_io.of_fd ~mode:Lwt_io.output socket_cli in
        let c = callback inchan outchan in
        let events = [c] in
        ignore (Lwt.pick events >> try_close outchan >> try_close inchan);
        _process ()
      )
  in
  _process ()

type exitState = OFFTRACK | NL | E | X | I | T | CR | MATCH

let next_state es c =
        match (es, c) with
        | (NL, 'e') -> E
        | (E, 'x') -> X
        | (X, 'i') -> I
        | (I, 't') -> T
        | (T, '\r') -> CR
        | (CR, '\n') -> MATCH
        | (MATCH, _) -> MATCH
        | (_, '\n') -> NL
        | (_, _) -> OFFTRACK

let fold_bytes es buf actual_len =
        let rec loop i es =
                if i < actual_len then
                        let c = Bytes.get buf i in
                        let new_es = next_state es c in
                        loop (i+1) new_es
                else
                        es
        in
        loop 0 es

let copy_until_exit ic oc =
        let buf_len = 8192 in
        let buf = Bytes.create buf_len in
        let rec loop es () =
                Lwt_io.read_into ic buf 0 buf_len
                >>=
                (fun actual_len ->
                        let new_es = fold_bytes es buf actual_len in
                        Lwt_io.write_from_exactly oc buf 0 actual_len
                        >>=
                        (fun () -> Lwt.return new_es)
                )
                >>=
                (fun new_es ->
                        if new_es = MATCH then Lwt.return ()
                        else loop new_es ()
                )
        in
        loop NL ()

let main =
  let () = Printf.printf "Hallo\n%!" in
  let () = Printf.printf "Listening to port %d\n%!" server_port in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_any, server_port) in
  let socket = init_socket sockaddr in
  lwt () = Lwt.return () in
  Lwt_main.run (
    process
      socket
      ~callback:copy_until_exit
  )
