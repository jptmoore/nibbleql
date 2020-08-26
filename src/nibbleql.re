open Printf;

let host_uri = ref("http://localhost:5000");

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
type last_t = int;
type method_t = string;
type uri_t = string;
type tagset_t = list((string,string));
type timestamp_t = int;
type value_t = float;
type datapoint_t = list((option(timestamp_t), option(tagset_t), value_t));



type value = [
  | `Set(uri_t)
  | `Post(datapoint_t, to_t)
  | `Get_since(func_t,from_t,tag_t,since_t)
  | `Get_range(func_t,from_t,tag_t,range_t)
  | `Get_last(func_t,from_t,tag_t,last_t)
  | `Delete_range(from_t,tag_t,range_t)
];

let handle_uri = (uri) => {
  host_uri := uri;
  host_uri^
}

let get_values_worker = (acc, value) => {
  acc++"{\"value\":" ++ Float.to_string(value) ++ "},";
}

let gen_values = (values) => {
  open String;
  List.fold_left( (acc,x) => get_values_worker(acc, x), "[", values) |>
    x => sub(x, 0, length(x)-1)  ++ "]";
}

let process_payload = (num, tag) => {
  switch(tag) {
  | Some((s1,s2)) => "not implemented";
  | None => gen_values(num);
  }
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

/* let handle_post = (num, to_, tag) => {
  let uri = sprintf("%s/ts/%s", host_uri^, to_);
  let payload = process_payload(num, tag);
  Net.post(~uri, ~payload); 
}; */

let handle_post = (datapoint, to_) => {
  "woot";
}

let gen_uri = (func,from,tag,cmd) => {
  sprintf("%s/ts/%s", host_uri^, from) ++ cmd ++ 
  process_get_tag(tag) ++ process_func(func);
}

let handle_get_since = (func,from,tag,since) => {
  let uri = gen_uri(func,from,tag,sprintf("/since/%d", since));
  Net.get(~uri);  
};

let handle_get_range = (func,from,tag,(t1,t2)) => {
  let uri = gen_uri(func,from,tag,sprintf("/range/%d/%d", t1, t2)); 
  Net.get(~uri);
};

let handle_get_last = (func,from,tag,last) => {
  let uri = gen_uri(func,from,tag,sprintf("/last/%d", last)); 
  Net.get(~uri);  
};

let handle_delete_range = (from,tag,(t1,t2)) => {
  let uri = gen_uri(None,from,tag,sprintf("/range/%d/%d", t1, t2)); 
  Net.delete(~uri);
};

let process = (statement) => {
  switch(statement) {
  | `Set (uri) => handle_uri(uri);
  | `Post(datapoint, to_) => handle_post(datapoint, to_)
  | `Get_since(func,from,tag,since) => handle_get_since(func,from,tag,since)
  | `Get_range(func,from,tag,range) => handle_get_range(func,from,tag,range)
  | `Get_last(func,from,tag,last) => handle_get_last(func,from,tag,last)
  | `Delete_range(from,tag,range) => handle_delete_range(from,tag,range)
  };
};
