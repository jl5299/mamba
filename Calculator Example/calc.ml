open Ast

module StringMaptoInt = Map.Make(String)

let rec eval expr m=
        match expr with
        |Lit (x) -> (x, m)
        |Binop (e1, op, e2) ->
                        let (v1, m1) = eval e1 m and (v2,m2) = eval e2 m in
                        (match op with
                        Add -> (v1 + v2, m)
                        | Sub -> (v1 - v2, m)
                        | Mul -> (v1 * v2, m)
                        | Div -> (v1 / v2, m))
        |Var(v) -> (StringMaptoInt.find v m, m)
        |Asn(v, e) ->
                        let (v1, m1) = eval e m in
                        let m2 = StringMaptoInt.add v v1 m1 in
                        (StringMaptoInt.find v m2, m2)
        |Seq(e1, e2) ->
                        let (v, m) = eval e1 m in
                        let (v, m) = eval e2 m in
                        (v, m)
       

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  let map = StringMaptoInt.empty in
  let result = eval expr map in
  let (value, map) = result in
  print_endline (string_of_int value)
