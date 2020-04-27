type op = 
  Add | Sub | Mul | Div | Equal | Neq 
| Less | Leq | Greater | Geq | And | Or

type uop = Neg

type prim_typ = Int | Bool | Real | Char

type typ =
  | String
  | Func of typ * typ (* typ1: args, typ2: output *)

type expr =
  | Id of string
  | Binop of expr * op * expr
  | Uniop of uop * expr
  | Lit of int
  | BoolLit of bool
  | CharLit of char
  | StringLit of string

and statement =
  | Decl of string * typ
  | Expr of expr
