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
%token SEMI_COLON
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

%start <Nibbleql.value option> lang
%%

lang:
  | v = statement; SEMI_COLON { Some v }
  | EOF       { None   } ;

statement:
  | SET; host = host; { `Set (host) }
  | POST; dp_fields = dp_fields; to_ = to_; { `Post (dp_fields, to_) }
  | GET; func = func?; from = from; tag = tag?; since = since; { `Get_since (func,from,tag,since) }
  | GET; func = func?; from = from; tag = tag?; range = range; { `Get_range (func,from,tag,range) }
  | GET; func = func?; from = from; tag = tag?; last = last; { `Get_last (func,from,tag,last) }
  | DELETE; from = from; tag = tag?; range = range; { `Delete_range (from,tag,range) }


host:
  HOST; s = STRING { s };

dp_fields:
  dpl = separated_list(COMMA, dp_field)    { dpl };

ts:
  TIMESTAMP; EQUALS; n = INT; COMMA { n };

dp_field:
    LEFT_PAREN?; ts = ts?; tag_fields = tag_fields?; v = FLOAT; RIGHT_PAREN?  { (ts, tag_fields, v) } ;

tag_fields:
  TAG; EQUALS; LEFT_BRACK; tl = separated_list(COMMA, tag_field); RIGHT_BRACK; COMMA { tl };

tag_field:
  s1 = STRING; EQUALS; s2 = STRING;    { (s1,s2) };

func:
  MIN { "min" } | MAX; { "max"} | SUM; {"sum"} | COUNT; {"count"} | MEAN; {"mean"} | SD; {"sd"};

from:
  FROM; s = STRING { s };
  
to_:
  TO; s = STRING { s };

range:
  | RANGE; n1 = INT; SECONDS; TO; n2 = INT; SECONDS { (Nibbleql.get_seconds(n1), Nibbleql.get_seconds(n2))}
  | RANGE; n1 = INT; MINUTES; TO; n2 = INT; MINUTES { (Nibbleql.get_minutes(n1), Nibbleql.get_minutes(n2))}
  | RANGE; n1 = INT; HOURS; TO; n2 = INT; HOURS { (Nibbleql.get_hours(n1), Nibbleql.get_hours(n2))}
  | RANGE; n1 = INT; DAYS; TO; n2 = INT; DAYS { (Nibbleql.get_days(n1), Nibbleql.get_days(n2))} ;

last:
  LAST; n = INT { n };

tag:
  WHERE; s1 = STRING; IS; s2 = STRING { (s1,s2) } ;

since:
  | SINCE; n = INT; SECONDS { Nibbleql.get_seconds(n) }
  | SINCE; n = INT; MINUTES { Nibbleql.get_minutes(n) }
  | SINCE; n = INT; HOURS { Nibbleql.get_hours(n) }
  | SINCE; n = INT; DAYS { Nibbleql.get_days(n) } ;
