open Lwt;
open Cohttp_lwt_unix;

let content_format = ref("json");

let post_worker = (~uri, ~payload) => {
  let headers = Cohttp.Header.of_list([("Accept", content_format^), ("Connection", "keep-alive")]);
  let body = Cohttp_lwt.Body.of_string(payload);
  Client.post(~headers=headers, ~body=body, Uri.of_string(uri)) >>= 
    (((_, body)) => body |> Cohttp_lwt.Body.to_string);
};


let post = (~uri, ~payload) => {
  Lwt_main.run(post_worker(~uri, ~payload));
}

let get_worker = (~uri) => {
  let headers = Cohttp.Header.of_list([("Content-Type", content_format^)]);
  Client.get(~headers=headers, Uri.of_string(uri)) >>=
    ((_, body)) => body |> Cohttp_lwt.Body.to_string;
}

let get = (~uri) => {
  Lwt_main.run(get_worker(~uri));
};

let delete_worker = (~uri) => {
  let headers = Cohttp.Header.of_list([]);
  Client.delete(~headers=headers, Uri.of_string(uri)) >>=
    ((_, body)) => body |> Cohttp_lwt.Body.to_string;
}

let delete = (~uri) => {
  Lwt_main.run(delete_worker(~uri));
};