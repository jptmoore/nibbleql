open Lwt;
open Cohttp_lwt_unix;

let content_format = ref("json");

let post = (~uri, ~payload) => {
  let headers = Cohttp.Header.of_list([("Content-Type", content_format^), ("Connection", "keep-alive")]);
  let body = Cohttp_lwt.Body.of_string(payload);
  Client.post(~headers=headers, ~body=body, Uri.of_string(uri)) >>= 
    (((_, body)) => body |> Cohttp_lwt.Body.to_string);
};

