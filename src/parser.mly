%token <int> INT
%token <string> STRING
%token <float> FLOAT
%token SET
%token POST
%token TO
%token WHERE
%token IS
%token HOST
%token PORT
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

%start <Nibbleql.value option> lang
%%

lang:
  | v = statement; SEMI_COLON { Some v }
  | EOF       { None   } ;

statement:
  | SET; host = host;  port = port? { `Set (host, port) }
  | POST; num = FLOAT; to_ = to_; tag = tag?;  { `Post (num, to_, tag) }
  | GET; func = func?; from = from; tag = tag?; since = since; { `Get_since (func,from,tag,since) }
  | GET; func = func?; from = from; tag = tag?; range = range; { `Get_range (func,from,tag,range) }
  | GET; func = func?; from = from; tag = tag?; last = last; { `Get_last (func,from,tag,last) }
  | DELETE; from = from; tag = tag?; range = range; { `Delete_range (from,tag,range) }


host:
  HOST; s = STRING { s };

port:
  PORT; n = INT { n };

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
