open Lexer;
open Lexing;

let error_info = lexbuf => {
  let pos = lexbuf.lex_curr_p;
  Printf.sprintf(
    "%s:%d:%d",
    pos.pos_fname,
    pos.pos_lnum,
    pos.pos_cnum - pos.pos_bol + 1,
  );
};

let parse_with_error = lexbuf =>
  try(Parser.prog(Lexer.read, lexbuf)) {
  | SyntaxError(msg) =>
    let info = error_info(lexbuf);
    Printf.printf("%s %s", info, msg);
    None;
  | Parser.Error =>
    let info = error_info(lexbuf);
    Printf.printf("%s syntax error", info);
    None;
  };

let parse = lexbuf => {
  switch (parse_with_error(lexbuf)) {
  | Some(value) => Nibbleql.process(value)
  | None => ""
  };
};

let parse_string = s => {
  let lexbuf = Lexing.from_string(s);
  parse(lexbuf);
};

let rec user_input = (prompt, cb) =>
  switch (LNoise.linenoise(prompt)) {
  | None => ()
  | Some(v) =>
    cb(v);
    user_input(prompt, cb);
  };

let format_json = s => {
  open Yojson.Basic;
  let json = from_string(s);
  pretty_to_string(json);
};

let handle_input = s => {
  let result = parse_string(s);
  try(format_json(result)) {
    | _ => result
  } |> print_endline
};

let repl = () => {
  LNoise.history_load(~filename=".history") |> ignore;
  LNoise.history_set(~max_length=100) |> ignore;
  LNoise.set_completion_callback((line_so_far, ln_completions) =>
    if (line_so_far != "" && line_so_far.[0] == 'q') {
      ["quit"] |> List.iter(LNoise.add_completion(ln_completions));
    } else if (line_so_far != "" && line_so_far.[0] == 'g') {
      ["get "] |> List.iter(LNoise.add_completion(ln_completions));
    } else if (line_so_far != "" && line_so_far.[0] == 'p') {
      ["post "] |> List.iter(LNoise.add_completion(ln_completions));
    } else if (line_so_far != "" && line_so_far.[0] == 'd') {
      ["delete "] |> List.iter(LNoise.add_completion(ln_completions));
    } else if (line_so_far != "" && line_so_far.[0] == 's') {
      ["set host "] |> List.iter(LNoise.add_completion(ln_completions));
    }
  );
  ["Welcome to nibbleql the query language for nibbledb\n"]
  |> List.iter(print_endline);
  (
    from_user => {
      if (from_user == "quit") {
        exit(0);
      };
      LNoise.history_add(from_user) |> ignore;
      LNoise.history_save(~filename=".history") |> ignore;
      handle_input(from_user);
    }
  )
  |> user_input("nibble> ");
};

try (repl()) {
  | Sys.Break => print_string("So long, and thanks for all the fish")
  | e => Printf.eprintf("there was an error: %s%s\n",Printexc.to_string(e),Printexc.get_backtrace());
};