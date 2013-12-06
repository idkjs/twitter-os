open Lwt
module CL = Cohttp_lwt_mirage

let tweet _ =
  CL.Client.post Uri.(of_string "http://www.google.com/")
  >>= fun _ -> Lwt.return ()
