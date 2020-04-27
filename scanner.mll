%{ open Parser }

let letter = ['a'-'z' 'A'-'Z' '_' '\'']
let digit = ['0'-'9']
let exp = ['e''E'] ['+' '-']? digit+

rule token = 
parse [' ' '\t' '\r' '\n']  { token lexbuf }
| "m@mba"                  { comment lexbuf }
| "mumbo" [^'\n']* '\n'    { token lexbuf }
| "if"      { IF }
| "then"    { THEN }
| "else"    { ELSE }
| "while"   { WHILE }
| "for"     { FOR }
| "do"      { DO }
| '.'       { DOT }
| "at"      { AT }
| '+'       { PLUS }
| '-'       { MINUS }
| '*'       { TIMES }
| '/'       { DIVIDE }
| "and"     { AND }
| "or"      { OR }
| "int"     { INT }
| "bool"    { BOOL }
| "char"    { CHAR }
| "string"  { STRING }
| "in"      { IN }
| ','       { COMMA }
| ':'       { COLON }
| '='       { EQUAL }
| "=="      { EQ }
| "!="      { NEQ }
| '<'       { LT }
| "<="      { LEQ }
| ">"       { GT }
| ">="      { GEQ }
| digit+ as lit                 { LITERAL(int_of_string lit) }
| (digit+ exp | (digit+ '.' digit* | '.' digit+) exp?) as reallit  
  { REALLIT(float_of_string reallit) }
| "true"    { BOOLLIT(true) }
| "false"   { BOOLLIT(false) }
| "'\\n'"   { CHARLIT('\n') }
| "'\\t'"   { CHARLIT('\t') }
| letter (letter | digit)* as lit { ID(lit) }
| '"' { 
  let (styp, s) = str "" Static lexbuf in
    match styp with
    | Static -> STRINGLIT(s)
    | BegInter -> BEGINTERSTRING(s)
    | _ -> raise (Failure "please sir" )
}
| eof { EOF }

and comment = parse
  "mamb@" { token lexbuf }
| _    { comment lexbuf }