open Util;
open Http;

external (&): ('a => 'b, 'a) => 'b = "%apply";

let opt_param = (name, param) =>
  switch (param) {
  | None => []
  | Some(p) => [(name, p)]
  };

let rfc3986_encode = s => Http.url_encode(s); /*Netencoding.Url.encode s*/

let string_of_http_method =
  fun
  | GET => "GET"
  | POST => "POST";

type signature_method = [
  | `Hmac_sha1
  | `Plaintext
  | `Rsa_sha1(Cryptokit.RSA.key)
];

let string_of_signature_method: signature_method => string = (
  fun
  | `Plaintext => "PLAINTEXT"
  | `Hmac_sha1 => "HMAC-SHA1"
  | `Rsa_sha1(_) => "RSA-SHA1":
    signature_method => string
);

let normalize_url = url =>
  /*  let url = Neturl.parse_url ~enable_fragment:true url in
      let url = Neturl.remove_from_url ~query:true ~fragment:true url in
      Neturl.string_of_url url*/
  url;

let string_of_timestamp = t => {
  let s = string_of_float(t);
  String.sub(s, 0, String.length(s) - 1);
};

let make_timestamp = OS.Clock.time;

let make_nonce = () => "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg";

let hmac_sha1_hash = (text, key) =>
  text
  |> Cryptokit.hash_string(Cryptokit.MAC.hmac_sha1(key))
  |> Base64.encode;

let sha1_digest_info = h =>
  "0!0\t\006\005+\014\003\002\026\005\000\004\020" ++ h;

let pkcs1_pad = (rsa_key, v) => {
  let tLen = String.length(v);
  let emLen = rsa_key.Cryptokit.RSA.size / 8;
  "\000\001" ++ String.make(emLen - tLen - 3, '\255') ++ "\000" ++ v;
};

/** Returns unsigned raw string */

let rsa_sha1_hash' = (text, rsa_key) =>
  text
  |> Cryptokit.hash_string(Cryptokit.Hash.sha1())
  |> sha1_digest_info
  |> pkcs1_pad(rsa_key);

let rsa_sha1_hash = (text, rsa_key) =>
  rsa_sha1_hash'(text, rsa_key)
  |> Cryptokit.RSA.sign(rsa_key)
  |> Base64.encode;

let check_rsa_sha1_hash = (text, rsa_key, signature) =>
  try(
    text
    |> Cryptokit.hash_string(Cryptokit.Hash.sha1())
    |> sha1_digest_info
    |> pkcs1_pad(rsa_key)
    == (signature |> Base64.decode |> Cryptokit.RSA.unwrap_signature(rsa_key))
  ) {
  | _ => false
  };

let with_oauth_headers =
    (
      ~oauth_signature_method,
      ~oauth_consumer_key,
      ~oauth_timestamp,
      ~oauth_nonce,
      ~oauth_version,
      ~oauth_token=?,
      ~oauth_signature=?,
      ~params=[],
      ~k,
      (),
    ) =>
  k
  & [
    ("oauth_consumer_key", oauth_consumer_key),
    ("oauth_nonce", oauth_nonce),
  ]
  @ opt_param("oauth_signature", oauth_signature)
  @ [
    (
      "oauth_signature_method",
      string_of_signature_method(oauth_signature_method),
    ),
    ("oauth_timestamp", string_of_timestamp(oauth_timestamp)),
  ]
  @ opt_param("oauth_token", oauth_token)
  @ [("oauth_version", oauth_version)]
  @ params;

let signature_base_string = (~http_method, ~url) =>
  with_oauth_headers(
    ~oauth_signature=?None,
    ~k=params => {
      let params =
        /* make sure "oauth_signature" is not in the params */
        List.filter(
          fun
          | ("oauth_signature", _) => false
          | _ => true,
          params,
        );

      List.map(
        Uri.pct_encode(~component=`Host),
        [
          string_of_http_method(http_method),
          normalize_url(url),
          params
          |> List.map(((k, v)) => (rfc3986_encode(k), rfc3986_encode(v)))
          |> List.sort(((k, v), (k', v')) =>
               switch (String.compare(k, k')) {
               | 0 => String.compare(v, v')
               | c => c
               }
             )
          |> List.map(((k, v)) => k ++ "=" ++ v)
          |> String.concat("&"),
        ],
      )
      |> String.concat("&");
    },
  );

let pre_sign =
    (
      ~http_method,
      ~url,
      ~oauth_signature_method,
      ~oauth_consumer_key,
      ~oauth_consumer_secret,
      ~oauth_token=?,
      ~oauth_token_secret=?,
      ~oauth_timestamp,
      ~oauth_nonce,
      ~oauth_version,
      ~params=?,
      ~k,
      (),
    ) => {
  let key =
    rfc3986_encode(oauth_consumer_secret)
    ++ "&"
    ++ (
      switch (oauth_token_secret) {
      | None => ""
      | Some(s) => rfc3986_encode(s)
      }
    );

  let signature_base_string =
    signature_base_string(
      ~http_method,
      ~url,
      ~oauth_signature_method,
      ~oauth_consumer_key,
      ~oauth_token?,
      ~oauth_timestamp,
      ~oauth_nonce,
      ~oauth_version,
      ~params?,
      /* ?oauth_token_secret ~oauth_consumer_secret */
      (),
    );

  k(oauth_signature_method, signature_base_string, key);
};

let sign =
  pre_sign(~k=(oauth_signature_method, signature_base_string, key) =>
    switch (oauth_signature_method) {
    | `Plaintext => rfc3986_encode(key)
    | `Hmac_sha1 => hmac_sha1_hash(signature_base_string, key)
    | `Rsa_sha1(rsa_key) => rsa_sha1_hash(signature_base_string, rsa_key)
    }
  );

let _check_signature = (~oauth_signature) =>
  pre_sign(~k=(oauth_signature_method, signature_base_string, key) =>
    switch (oauth_signature_method) {
    | `Plaintext => rfc3986_encode(key) == oauth_signature
    | `Hmac_sha1 =>
      hmac_sha1_hash(signature_base_string, key) == oauth_signature
    | `Rsa_sha1(rsa_key) =>
      check_rsa_sha1_hash(signature_base_string, rsa_key, oauth_signature)
    }
  );

/** Auth params => a header line */

let encode_authorization_params = params => (
  "Authorization",
  params
  |> List.map(((k, v)) =>
       k ++ "=\"" ++ String.escaped(rfc3986_encode(v)) ++ "\""
     )
  |> String.concat(", "),
);

let suffix = ([(x, y), ...xs]) => [("OAuth " ++ x, y), ...xs];
let authorization_header =
  with_oauth_headers(~k=params =>
    encode_authorization_params & suffix(params)
  );

let create_oauth_header =
    (
      ~http_method,
      ~url,
      ~oauth_version,
      ~oauth_signature_method,
      ~oauth_timestamp,
      ~oauth_nonce,
      ~oauth_consumer_key,
      ~oauth_consumer_secret,
      ~oauth_token=?,
      ~oauth_token_secret=?,
      params,
    ) => {
  let oauth_signature =
    sign(
      ~http_method,
      ~url,
      ~oauth_version,
      ~oauth_signature_method,
      ~oauth_timestamp,
      ~oauth_nonce,
      ~oauth_consumer_key,
      ~params,
      ~oauth_consumer_secret,
      ~oauth_token?,
      ~oauth_token_secret?,
      (),
    );

  authorization_header(
    ~oauth_version,
    ~oauth_signature_method,
    ~oauth_timestamp,
    ~oauth_nonce,
    ~oauth_consumer_key,
    ~params=List.rev(List.tl(List.rev(params))),
    ~oauth_token?,
    ~oauth_signature,
    (),
  );
};

let string_of_protocol =
  fun
  | `HTTP => "http"
  | `HTTPS => "https";

let gen_access =
    (
      ~protocol,
      ~http_method,
      ~host,
      ~port=?,
      ~path,
      ~oauth_version="1.0",
      ~oauth_signature_method=`Hmac_sha1,
      ~oauth_timestamp=make_timestamp(),
      ~oauth_nonce="5ebe23c792f7f3bc0475988dd4381de8",
      ~oauth_token=?,
      ~oauth_token_secret=?,
      ~oauth_other_params=[],
      ~non_oauth_params=[],
      ~oauth_consumer_key,
      ~oauth_consumer_secret,
      (),
    ) => {
  let url = string_of_protocol(protocol) ++ "://" ++ host ++ path;
  let header =
    create_oauth_header(
      ~http_method,
      ~url,
      ~oauth_version,
      ~oauth_signature_method,
      ~oauth_timestamp,
      ~oauth_nonce,
      ~oauth_consumer_key,
      ~oauth_consumer_secret,
      ~oauth_token?,
      ~oauth_token_secret?,
      oauth_other_params @ non_oauth_params,
    );

  Http.by_curl(
    http_method,
    protocol,
    host,
    ~port?,
    path,
    ~headers=[header],
    ~params=non_oauth_params,
  );
};

let fetch_request_token = (~http_method=POST) =>
  /* CRv2 jfuruse: HTTPS */
  gen_access(
    ~protocol=`HTTP,
    ~http_method,
    ~oauth_token=?None,
    ~oauth_token_secret=?None,
    ~oauth_other_params=[],
    ~non_oauth_params=[],
  );

let fetch_access_token =
    (~verif, ~oauth_token, ~oauth_token_secret, ~http_method=POST) =>
  /* CRv2 jfuruse: HTTPS */
  gen_access(
    ~protocol=`HTTP,
    ~http_method,
    ~oauth_token,
    ~oauth_token_secret,
    ~oauth_other_params=[("oauth_verifier", verif)],
    ~non_oauth_params=[],
  );

let access_resource = (~oauth_token, ~oauth_token_secret, ~http_method=GET) =>
  gen_access(~http_method, ~oauth_token, ~oauth_token_secret);

type t = {
  consumer_key: string,
  consumer_secret: string,
  access_token: string,
  access_token_secret: string,
};

let access = (proto, oauth, meth, host, path, params) =>
  access_resource(
    ~protocol=proto,
    ~http_method=meth,
    ~host,
    ~path,
    ~oauth_consumer_key=oauth.consumer_key,
    ~oauth_consumer_secret=oauth.consumer_secret,
    ~oauth_token=oauth.access_token,
    ~oauth_token_secret=oauth.access_token_secret,
    ~non_oauth_params=params,
    (),
  );
