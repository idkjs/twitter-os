exception Http_error of string

val url_encode  : string -> string
val html_decode : string -> string

type header = { code : string; fields : (string, string) Hashtbl.t; }
type params = (string * string) list
type meth = GET | POST

(** http access via cURL *)
val by_curl :
  meth                 (** GET/POST *)
  -> [`HTTP | `HTTPS ] (** protocol *)
  -> string            (** hostname *)
  -> ?port: int        (** port *)
  -> string            (** path *)
  -> params: params    (** get/post parameters *)
  -> headers: params
  -> unit Lwt.t
