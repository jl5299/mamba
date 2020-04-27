{ open Parser }

rule token =
  parse [' ' '\t' '\r' '\n'] { token lexbuf }
  | '+'	     	       	     { PLUS }
  | '-'			     { MINUS }
  | '*'			     { TIMES }
  | '/'			     { DIVIDE }
  | '='                      { ASSIGNMENT }
  | ';'                      { SEQUENCE } 
  | ['0'-'9']+ as lit	     { LITERAL (int_of_string lit) }
  | ['a' - 'z']+ as id       { VARIABLE (id) }
  | eof        	  	     { EOF }