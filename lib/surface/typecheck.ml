open Common.Env
open Common.Type
open Ast

let rec surface_typing (m : surf) (env : tenv) : ttype =
  match m with
  | Var x -> lookup_env x env
  | Const k -> const_typing k
  | Abs (x, a, n) ->
    let env' = (x, a) :: env in
    let b = surface_typing n env' in
    TArrow (a, b)
  | App (l, m) ->
    (match surface_typing l env with
    | TArrow (a, b) ->
      let a' = surface_typing m env in
      check (=) a' a;
      b
    | _ -> raise (TypeError "Application type mismatch"))

let rec surface_typed (m : surf) (env : tenv) : 'a tsurf =
  match m with
  | Var x -> Var (x, (surface_typing m env))
  | Const k -> Const (k, (surface_typing m env))
  | Abs (x, a, n) ->
    let env' = (x, a) :: env in
    let b = surface_typing n env' in
    Abs (x, a, surface_typed n env', TArrow (a, b))
  | App (l, m) ->
    (match surface_typing l env with
    | TArrow (_, b) ->
      App (surface_typed l env, surface_typed m env, b)
    | _ -> raise (TypeError "Application type mismatch"))