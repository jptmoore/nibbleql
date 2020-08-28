%token <int> INT
%token <string> STRING
%token <float> FLOAT
%token SET
%token POST
%token TO
%token WHERE
%token IS
%token HOST
%token GET
%token DELETE
%token FROM
%token SINCE
%token RANGE
%token EOF
%token MIN
%token MAX
%token SUM
%token COUNT
%token MEAN
%token SD
%token LAST
%token SECONDS
%token MINUTES
%token HOURS
%token DAYS
%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACK
%token RIGHT_BRACK
%token EQUALS
%token TIMESTAMP
%token TAG
%token VALUE
%token SEMI_COLON
%token OR
%token AND

%start <Nibbleql.value option> prog
%%

prog:
  | v = value; SEMI_COLON { Some v }
  | EOF       { None   } ;

value:
  | SET; host = host; { `Set (host) }
  | POST; data_items = data_items; to_target = to_target; { `Post (data_items, to_target) }
  | GET; func = func?; from = from; filter_items = filter_items?; since = since; { `Get_since (func,from,filter_items,since) }
  | GET; func = func?; from = from; filter_item = filter_item?; range = range; { `Get_range (func,from,filter_item,range) }
  | GET; func = func?; from = from; filter_item  = filter_item?; last = last; { `Get_last (func,from,filter_item,last) }
  | DELETE; from = from; filter_item = filter_item?; range = range; { `Delete_range (from,filter_item,range) }


host:
  HOST; s = STRING { s };

data_items:
  dpl = separated_list(COMMA, data_item)    { dpl };

ts:
  TIMESTAMP; EQUALS; n = INT; COMMA { n };

num:
  VALUE?; EQUALS?; v = FLOAT; { v };

data_item:
    LEFT_PAREN?; ts = ts?; tag_items = tag_items?; num = num; RIGHT_PAREN?  { (ts, tag_items, num) } ;

tag_items:
  TAG; EQUALS; LEFT_BRACK; tl = separated_list(COMMA, tag_item); RIGHT_BRACK; COMMA { tl };

tag_item:
  s1 = STRING; EQUALS; s2 = STRING;    { (s1,s2) };

func:
  MIN { "min" } | MAX; { "max"} | SUM; {"sum"} | COUNT; {"count"} | MEAN; {"mean"} | SD; {"sd"};

from:
  FROM; s = STRING { s };
  
to_target:
  TO; s = STRING { s };

range:
  | RANGE; n1 = INT; SECONDS; TO; n2 = INT; SECONDS { (Nibbleql.get_seconds(n1), Nibbleql.get_seconds(n2))}
  | RANGE; n1 = INT; MINUTES; TO; n2 = INT; MINUTES { (Nibbleql.get_minutes(n1), Nibbleql.get_minutes(n2))}
  | RANGE; n1 = INT; HOURS; TO; n2 = INT; HOURS { (Nibbleql.get_hours(n1), Nibbleql.get_hours(n2))}
  | RANGE; n1 = INT; DAYS; TO; n2 = INT; DAYS { (Nibbleql.get_days(n1), Nibbleql.get_days(n2))} ;

last:
  LAST; n = INT { n };

op: OR; | AND; {};

filter_items:
  | lis = separated_nonempty_list(op, filter_item); { lis };

filter_item:
  WHERE; s1 = STRING; IS; s2 = STRING { (s1,s2) } ;

since:
  | SINCE; n = INT; SECONDS { Nibbleql.get_seconds(n) }
  | SINCE; n = INT; MINUTES { Nibbleql.get_minutes(n) }
  | SINCE; n = INT; HOURS { Nibbleql.get_hours(n) }
  | SINCE; n = INT; DAYS { Nibbleql.get_days(n) } ;
