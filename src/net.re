open Lwt;
open Cohttp_lwt_unix;

let content_format = ref("json");

let post_worker = (~uri, ~payload) => {
  let headers = Cohttp.Header.of_list([("Accept", content_format^), ("Connection", "keep-alive")]);
  let body = Cohttp_lwt.Body.of_string(payload);
  Client.post(~headers=headers, ~body=body, Uri.of_string(uri)) >>= 
    (((_, body)) => body |> Cohttp_lwt.Body.to_string);
};

let get_worker = (~uri, ~payload) => {
  let headers = Cohttp.Header.of_list([("Content-Type", content_format^)]);
  Client.get(~headers=headers, Uri.of_string(uri)) >>=
    ((_, body)) => body |> Cohttp_lwt.Body.to_string;
}

let delete_worker = (~uri, ~payload) => {
  let headers = Cohttp.Header.of_list([]);
  Client.delete(~headers=headers, Uri.of_string(uri)) >>=
    ((_, body)) => body |> Cohttp_lwt.Body.to_string;
}

let call_with = (~fn, ~uri, ~payload="", ()) => {
  Lwt_main.run(
    Lwt.catch(
      () => fn(~uri, ~payload),
      fun
      | Failure(m) => Lwt.return(m)
      | exn => Lwt.fail(exn)
    )
  )
}

let post = (~uri, ~payload) => call_with(~fn=post_worker, ~uri, ~payload, ());

let get = (~uri) => call_with(~fn=get_worker, ~uri, ());

let delete = (~uri) => call_with(~fn=delete_worker, ~uri, ());
