exception Http_error(string);

let url_encode: string => string;
let html_decode: string => string;

type header = {
  code: string,
  fields: Hashtbl.t(string, string),
};
type params = list((string, string));
type meth =
  | GET
  | POST;

/** http access via cURL */

let by_curl:
  (
    /** GET/POST */ meth,
    /** protocol */ [ | `HTTP | `HTTPS],
    /** hostname */ string,
    ~port: /** port */ int=?,
    /** path */ string,
    ~params: /** get/post parameters */ params,
    ~headers: params
  ) =>
  Lwt.t(unit);
