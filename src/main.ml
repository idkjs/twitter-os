(* Generated by Mirari (Fri, 6 Dec 2013 01:57:15 GMT). *)

open Filesystem_static

let get = function Some x -> x | None -> failwith "Bad IP!"
let ip = `IPv4 (
  get (Ipaddr.V4.of_string "192.168.56.99"),
  get (Ipaddr.V4.of_string "255.255.255.0"),
  [get (Ipaddr.V4.of_string "192.168.56.1")]
)

let listen_port = 8080
let listen_address = None

let main () =
  let spec = Cohttp_lwt_mirage.Server.({
    callback    = Dispatch.t;
    conn_closed = (fun _ () -> ());
  }) in
  Net.Manager.create (fun mgr interface id ->
    Printf.eprintf "listening to HTTP on port %d\\n" listen_port;
    Net.Manager.configure interface ip >>
    Cohttp_lwt_mirage.listen mgr (listen_address, listen_port) spec
  )

let () = OS.Main.run (Lwt.join [main (); Backend.run ()])
