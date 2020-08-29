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
type data_t = list((option(timestamp_t), option(tagset_t), value_t));
type filter_t = option(list((string,string)));

type value = [
  | `Set(uri_t)
  | `Post(data_t, to_t)
  | `Get_since(func_t,from_t,filter_t,since_t)
  | `Get_range(func_t,from_t,tag_t,range_t)
  | `Get_last(func_t,from_t,tag_t,last_t)
  | `Delete_range(from_t,tag_t,range_t)
];

let set_host = (uri) => {
  host_uri := uri;
  "";
}


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

let get_tag_worker = (acc, tag) => {
  let (n,v) = tag;
  acc++sprintf("{\"%s\":\"%s\"},", n, v);
}

let gen_tag = (data) => {
  open String;
  List.fold_left( (acc,x) => get_tag_worker(acc, x), "[", data) |>
    x => sub(x, 0, length(x)-1)  ++ "]";
}

let get_values_worker = (acc, data) => {
  let (ts, t, v) = data;
  switch (ts, t, v) {
  | (Some(ts), Some(t), v) =>
    let timestamp = sprintf("\"timestamp\": %s", Int.to_string(ts));
    let tag = sprintf("\"tag\": %s", gen_tag(t));
    let value = sprintf("\"value\": %s", Float.to_string(v));
    acc++sprintf("{%s,%s,%s}", timestamp, tag, value)++",";
  | (Some(ts), None, v) =>
    let timestamp = sprintf("\"timestamp\": %s", Int.to_string(ts));
    let value = sprintf("\"value\": %s", Float.to_string(v));
    acc++sprintf("{%s,%s}", timestamp, value)++",";
  | (None, Some(t), v) =>
    let tag = sprintf("\"tag\": %s", gen_tag(t)); 
    let value = sprintf("\"value\": %s", Float.to_string(v));
    acc++sprintf("{%s,%s}", tag, value)++",";
  | (None, None, v) =>
    let value = sprintf("\"value\": %s", Float.to_string(v));
    acc++sprintf("{%s}", value)++",";
  }

}

let gen_values = (data) => {
  open String;
  List.fold_left( (acc,x) => get_values_worker(acc, x), "[", data) |>
    x => sub(x, 0, length(x)-1)  ++ "]";
}


let handle_post = (data, to_) => {
  let uri = sprintf("%s/ts/%s", host_uri^, to_);
  let payload = gen_values(data);
  Net.post(~uri, ~payload); 
}

let gen_uri = (func,from,tag,cmd) => {
  sprintf("%s/ts/%s", host_uri^, from) ++ cmd ++ 
  process_get_tag(tag) ++ process_func(func);
}

let gen_filter_worker = (acc, x) => {
  acc++Printf.sprintf("%s,", x);
}

let gen_name_filter = (data) => {
  open String;
  List.fold_left( (acc,(n,_)) => gen_filter_worker(acc, n), "", data) |>
    x => sub(x, 0, length(x)-1);
}

let gen_value_filter = (data) => {
  open String;
  List.fold_left( (acc,(_,v)) => gen_filter_worker(acc, v), "", data) |>
    x => sub(x, 0, length(x)-1);
}

let gen_filter = (data) => {
  open Printf;
  sprintf("filter/%s/equals/%s", gen_name_filter(data), gen_value_filter(data));
}

let handle_get_since = (func,from,filter,since) => {
  let tag = switch (filter) {
  | None => "";
  | Some(data) => gen_filter(data)
  }
  let uri = gen_uri(func,from,None,sprintf("/since/%d/%s", since,tag));
  uri;
  /* Net.get(~uri); */
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
  | `Set (uri) => set_host(uri);
  | `Post(data, to_target) => handle_post(data, to_target)
  | `Get_since(func,from,filter,since) => handle_get_since(func,from,filter,since)
  | `Get_range(func,from,tag,range) => handle_get_range(func,from,tag,range)
  | `Get_last(func,from,tag,last) => handle_get_last(func,from,tag,last)
  | `Delete_range(from,tag,range) => handle_delete_range(from,tag,range)
  };
};
