open Printf
open Common.Type

type core =
  | Var of string
  | Const of const
  | Abs of string * ttype * core
  | App of core * core

let rec string_of_core =
  function
  | Var s -> s
  | Const k -> string_of_const k
  | Abs (x, a, n) -> sprintf "(λ %s:%s.%s)" x (string_of_ttype a) (string_of_core n)
  | App (l, m) -> sprintf "(%s %s)" (string_of_core l) (string_of_core m)