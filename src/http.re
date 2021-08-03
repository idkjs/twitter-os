open Lwt;
module C = Cohttp;
module CL = Cohttp_lwt_mirage;
exception Http_error(string);

let url_encode = s => Uri.pct_encode(~component=`Host, s);
let html_decode = x => x;

type header = {
  code: string,
  fields: Hashtbl.t(string, string),
};
type params = list((string, string));
type meth =
  | GET
  | POST;

let conv = xs => List.map(((x, y)) => (x, [y]), xs);

let params2string = ps =>
  String.concat("&", List.map(((k, v)) => k ++ "=" ++ url_encode(v), ps));

let get =
  fun
  | Some(x) => x
  | None => failwith("Bad IP!");
let write = (flow, s) => Net.Flow.write(flow, Cstruct.of_string(s));
let post = (~headers, ~params, ~uri) => {
  let s = params2string(params);

  let f = flow =>
    write(flow, Printf.sprintf("POST %s HTTP/1.1\n", Uri.path(uri)))
    >> Lwt_list.iter_s(
         ((k, v)) => write(flow, Printf.sprintf("%s: %s\n", k, v)),
         headers,
       )
    >> write(flow, "Content-Type: application/x-www-form-urlencoded\n")
    >> write(
         flow,
         Printf.sprintf("Content-length: %d\n", String.length(s)),
       )
    >> write(flow, "\n")
    >> write(flow, s)
    >> Net.Flow.read(flow)
    >>= (_ => Net.Flow.close(flow));

  Net.Manager.create((mgr, interface, id) =>
    Net.Flow.connect(
      mgr,
      `TCPv4((None, (get(Ipaddr.V4.of_string("199.59.148.20")), 80), f)),
    )
  );
};

/** http access via cohttp */

let by_curl = (meth, protocol, host, ~port=443, path, ~params, ~headers) => {
  let uri = Uri.make(~scheme="http", ~host, ~path, ());

  let headers = [("Host", host), ("Accept", "*/*")] @ headers;

  post(~headers, ~params, ~uri)
  /*CL.Client.post_form ~headers ~params:(C.Header.of_list params) uri*/
  >>= (_ => Lwt.return());
};
