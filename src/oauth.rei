type signature_method = [
  | `Hmac_sha1
  | `Plaintext
  | `Rsa_sha1(Cryptokit.RSA.key)
];

type t = {
  consumer_key: string,
  consumer_secret: string,
  access_token: string,
  access_token_secret: string,
};

let access:
  (
    [ | `HTTP | `HTTPS],
    t,
    Http.meth,
    /** host */ string,
    /** path */ string,
    /** params */ list((string, string))
  ) =>
  Lwt.t(unit);
