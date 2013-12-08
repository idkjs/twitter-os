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

(** http access via cohttp *)
let by_curl meth protocol host ?(port=443) path ~params ~headers =
  let uri =
    Uri.make ~scheme:"https" ~host ~path ()
  in
  let headers =
    C.Header.of_list headers
  in
  CL.Client.post_form ~headers ~params:(C.Header.of_list params) uri
  >>= (fun _ -> assert false; Lwt.return ())
