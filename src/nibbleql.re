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
  | `Get_range(func_t,from_t,filter_t,range_t)
  | `Get_last(func_t,from_t,filter_t,last_t)
  | `Delete_range(from_t,filter_t,range_t)
];

let set_host = (uri) => {
  host_uri := uri;
  "";
}

let gen_func_s = (func) => {
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

let gen_series_s = (series) => {
  sprintf("%s/ts/%s", host_uri^, series);
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
  sprintf("/filter/%s/equals/%s", gen_name_filter(data), gen_value_filter(data));
}

let gen_filter_s = (filter) => {
  switch (filter) {
    | None => "";
    | Some(data) => gen_filter(data)
  };
}

let handle_get_since = (func,series,filter,since) => {
  let func_s = gen_func_s(func);
  let filter_s = gen_filter_s(filter);
  let since_s = sprintf("/since/%d", since);
  let series_s = gen_series_s(series);
  series_s ++ since_s ++ filter_s ++ func_s;
  /* Net.get(~uri); */
};

let handle_get_range = (func,series,filter,(t1,t2)) => {
  let range_s = sprintf("/range/%d/%d", t1, t2); 
  let func_s = gen_func_s(func);
  let filter_s = gen_filter_s(filter);
  let series_s = gen_series_s(series);
  series_s ++ range_s ++ filter_s ++ func_s;  
  /* Net.get(~uri); */
};

let handle_get_last = (func,series,filter,last) => {
  let last_s = sprintf("/last/%d", last); 
  let func_s = gen_func_s(func);
  let filter_s = gen_filter_s(filter);
  let series_s = gen_series_s(series);
  series_s ++ last_s ++ filter_s ++ func_s;  
  /* Net.get(~uri);   */
};

let handle_delete_range = (series,filter,(t1,t2)) => {
  let delete_s = sprintf("/range/%d/%d", t1, t2);
  let filter_s = gen_filter_s(filter);
  let series_s = gen_series_s(series);
  series_s ++ delete_s ++ filter_s;  
  /* Net.delete(~uri); */
};

let process = (statement) => {
  switch(statement) {
  | `Set (uri) => set_host(uri);
  | `Post(data, to_target) => handle_post(data, to_target)
  | `Get_since(func,from,filter,since) => handle_get_since(func,from,filter,since)
  | `Get_range(func,from,filter,range) => handle_get_range(func,from,filter,range)
  | `Get_last(func,from,filter,last) => handle_get_last(func,from,filter,last)
  | `Delete_range(from,filter,range) => handle_delete_range(from,filter,range)
  };
};
