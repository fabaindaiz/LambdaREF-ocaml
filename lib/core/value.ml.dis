open Common.Env
open Common.Type
open Ast
open Printf

type value = 
  | Const of const
  | Lambda of string * ttype * core

let rec string_of_val =
  function
  | Const k -> sprintf "%s" (string_of_const k)
  | Lambda (x, a, n) -> sprintf "(λ %s:%s.%s)" x (string_of_ttype a) (string_of_core n)

type venv = value env