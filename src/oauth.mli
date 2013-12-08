type signature_method = [ `Hmac_sha1
                        | `Plaintext
                        | `Rsa_sha1 of Cryptokit.RSA.key
                        ]

type t = {
  consumer_key        : string;
  consumer_secret     : string;
  access_token        : string;
  access_token_secret : string;
}

val access :
  [ `HTTP | `HTTPS ]
  -> t
  -> Http.meth
  -> string (** host *)
  -> string (** path *)
  -> (string * string) list (** params *)
  -> unit Lwt.t
