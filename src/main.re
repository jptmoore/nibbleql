open Lexer;
open Lexing;


let error_info = (lexbuf) => {
  let pos = lexbuf.lex_curr_p;
  Printf.sprintf("%s:%d:%d", pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1);
};

let parse_with_error = (lexbuf) => {
  try (Parser.lang(Lexer.read, lexbuf)) {
  | SyntaxError(msg) => {
      let info = error_info(lexbuf);
      Printf.printf("%s %s", info, msg);
      None;
    };
  | Parser.Error => {
      let info = error_info(lexbuf);
      Printf.printf("%s syntax error", info);
      None;
    };
  };
};

let parse = (lexbuf) => {
  switch (parse_with_error(lexbuf)) {
  | Some(value) => Nibbleql.process(value);
  | None => "";
  };
};

let parse_string = (s) => {
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

let () = {
  /*LNoise.set_multiline(true);*/
  LNoise.set_hints_callback(
    (line) =>
      if (line != "set host ") {
        None;
      } else {
        Some(("<name> port <number>", LNoise.Yellow, true));
      }
  );
  LNoise.history_load(~filename=".history") |> ignore;
  LNoise.history_set(~max_length=100) |> ignore;
  LNoise.set_completion_callback(
    (line_so_far, ln_completions) =>
      if (line_so_far != "" && line_so_far.[0] == 'q') {
        ["quit"] |> List.iter(LNoise.add_completion(ln_completions));
      }
  );
  [
    "Welcome to nibbleql the query language for nibbledb\n"
  ]
  |> List.iter(print_endline);
  (
    (from_user) => {
      if (from_user == "quit") {
        exit(0);
      };
      LNoise.history_add(from_user) |> ignore;
      LNoise.history_save(~filename=".history") |> ignore;
      parse_string(from_user) |> print_endline;
    }
  )
  |> user_input("nibble> ");
};
