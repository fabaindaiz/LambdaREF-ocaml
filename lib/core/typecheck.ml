open Common.Env
open Common.Type
open Ast

let rec core_typing (m: core) (env: tenv) : ttype =
  match m with
  | Var x -> lookup_env x env
  | Const k -> const_typing k
  | Abs (x, a, n) ->
    let env' = (x, a) :: env in
    let b = core_typing n env' in
    TArrow (a, b)
  | App (l, m) ->
    (match core_typing l env with
    | TArrow (a, b) ->
      let a' = core_typing m env in
      check (=) a' a;
      b
    | _ -> raise (TypeError "Application type mismatch"))