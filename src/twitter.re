open Lwt;
module CL = Cohttp_lwt_mirage;

let t = {
  Oauth.consumer_key: "-",
  consumer_secret: "-",
  access_token: "-",
  access_token_secret: "-",
};

let tweet = text =>
  Oauth.access(
    `HTTP,
    t,
    Http.POST,
    "api.twitter.com",
    "/1.1/statuses/update.json",
    [("status", text)],
  )
  >>= (_ => Lwt.return());
