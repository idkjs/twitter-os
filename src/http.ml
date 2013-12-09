open Lwt
module C = Cohttp
module CL = Cohttp_lwt_mirage
exception Http_error of string

let url_encode s =
  Uri.pct_encode ~component:`Host s
let html_decode x = x

type header = { code : string; fields : (string, string) Hashtbl.t; }
type params = (string * string) list
type meth = GET | POST

let conv xs = List.map (fun (x,y) -> (x, [y])) xs

let params2string ps =
    String.concat "&" (List.map (fun (k,v) -> k^"="^url_encode v) ps)

let get = function Some x -> x | None -> failwith "Bad IP!"
let write flow s = Net.Flow.write flow (Cstruct.of_string s)
let post ~headers ~params ~uri =
  let s =
    params2string params
  in
  let f flow =
    write flow (Printf.sprintf "POST %s HTTP/1.1\n" (Uri.path uri))
    >> Lwt_list.iter_s (fun (k,v) -> write flow (Printf.sprintf "%s: %s\n" k v))
    headers
    >> write flow "Content-Type: application/x-www-form-urlencoded\n"
    >> write flow (Printf.sprintf "Content-length: %d\n" (String.length s))
    >> write flow "\n"
    >> write flow s
    >> Net.Flow.read flow
    >>= (fun _ -> Net.Flow.close flow)
  in
  Net.Manager.create begin fun mgr interface id ->
    Net.Flow.connect mgr
    (`TCPv4 (None, (get (Ipaddr.V4.of_string "199.59.148.20"), 80), f))
  end

(** http access via cohttp *)
let by_curl meth protocol host ?(port=443) path ~params ~headers =
  let uri =
    Uri.make ~scheme:"http" ~host ~path ()
  in
  let headers =
    ["Host", host; "Accept", "*/*"] @ headers
  in
  post ~headers ~params ~uri
  (*CL.Client.post_form ~headers ~params:(C.Header.of_list params) uri*)
  >>= (fun _ -> Lwt.return ())
