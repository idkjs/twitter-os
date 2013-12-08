open Lwt
module CL = Cohttp_lwt_mirage

let t = {
  Oauth.consumer_key = "bwxaKyK31ymrky68m6mHw";
  consumer_secret = "Sd2xCDeOXMyUHEZRIMbjUk8ntE4LoCRRGwpOjKo8";
  access_token = "34549501-xF5XAm2ze3yuePy964EEPU49d6UFgcGTHX2sNFOXP";
  access_token_secret = "yEKkeBUPZ3yUQ8rFLqBN31hcLvy3kx1rr8rd8NwIbRxow"
}

let tweet text =
  Oauth.access `HTTPS t Http.POST "api.twitter.com" "/1.1/statuses/update.json"
      ["status", text]
  >>= fun _ -> Lwt.return ()
