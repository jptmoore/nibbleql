open Printf;

let host_uri = ref("http://localhost:8000");

let mil = 1000 * 1000;

let get_time = () => {
  let t_sec = Unix.gettimeofday();
  let t_mirco = t_sec *. float_of_int(mil);
  int_of_float(t_mirco);
};

let get_seconds = (n) => get_time() - (n * mil);
let get_minutes = (n) => get_time() - (n * mil * 60);
let get_hours = (n) => get_time() - (n * mil * 60 * 60);
let get_days = (n) => get_time() - (n * mil * 60 * 60 * 24);

type func_t = option(string);
type from_t = string;
type tag_t = option((string,string));
type since_t = int;
type max_age_t = option(int);
type range_t = (int,int);
type to_t = string;
type num_t = float;
type last_t = int;
type method_t = string;
type uri_t = string;

type value = [
  | `Set(uri_t)
  | `Post(num_t, to_t, tag_t)
  | `Get_since(func_t,from_t,tag_t,since_t)
  | `Get_range(func_t,from_t,tag_t,range_t)
  | `Get_last(func_t,from_t,tag_t,last_t)
  | `Delete_range(from_t,tag_t,range_t)
];

let handle_uri = (uri) => {
  host_uri := uri;
  uri;
}

let process_payload = (num, tag) => {
  switch(tag) {
  | Some((s1,s2)) => sprintf("{\"%s\": \"%s\", \"value\": \"%f\"}", s1, s2, num)
  | None => sprintf("{\"value\": \"%f\"}", num)
  }
};

let handle_post = (num, to_, tag) => {
  
  let uri = host_uri^ ++ "/ts/" ++ to_;
  let payload = process_payload(num, tag);
  sprintf("Post %s to %s", payload, uri)
};

let process_get_tag = (tag) => {
  switch(tag) {
  | Some((s1,s2)) => sprintf("/filter/%s/equals/%s", s1, s2)
  | None => ""
  }
};

let process_func = (func) => {
  switch(func) {
  | Some(func) => sprintf("/%s", func)
  | None => ""
  }
};

let handle_get_since = (func,from,tag,since) => {
  sprintf(" --path /ts/%s", from) ++ sprintf("/since/%d", since) ++ 
  process_get_tag(tag) ++ process_func(func) ++ " --mode get"
};

let handle_get_range = (func,from,tag,(t1,t2)) => {
  sprintf(" --path /ts/%s", from) ++ sprintf("/range/%d/%d", t1, t2) ++ 
  process_get_tag(tag) ++ process_func(func) ++ " --mode get"
};

let handle_get_last = (func,from,tag,last) => {
  sprintf(" --path /ts/%s", from) ++ sprintf("/last/%d", last) ++ 
  process_get_tag(tag) ++ process_func(func) ++ " --mode get"
};


let handle_delete_range = (from,tag,(t1,t2)) => {
  sprintf(" --path /ts/%s", from) ++ sprintf("/range/%d/%d", t1, t2) ++ 
  process_get_tag(tag) ++ " --mode delete"
};

let process = (statement) => {
  switch(statement) {
  | `Set (uri) => handle_uri(uri);
  | `Post(num, to_, tag) => handle_post(num, to_, tag)
  | `Get_since(func,from,tag,since) => handle_get_since(func,from,tag,since)
  | `Get_range(func,from,tag,range) => handle_get_range(func,from,tag,range)
  | `Get_last(func,from,tag,last) => handle_get_last(func,from,tag,last)
  | `Delete_range(from,tag,range) => handle_delete_range(from,tag,range)
  };
};