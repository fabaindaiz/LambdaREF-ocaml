open Common.Type
open Surface.Ast
open Surface.Typecheck
open Core.Ast

exception CompileError of string

let rec surf_to_core (m : 'a tsurf) : core =
  match m with
  | Var (x, _) -> Var x
  | Const (k, _) -> Const k
  | Abs (x, a, n, _) ->
    Abs (x, a, surf_to_core n)
  | App (l, m, _) ->
    (match tsurf_type l with
    | TArrow (_, _) ->
      App (surf_to_core l, surf_to_core m)
    | _ -> raise (CompileError "App: not an arrow"))


let compile_core (m : surf) (env : tenv) : core =
  let _ = surface_typing m env in
  let m' = surface_typed m env in
  surf_to_core m'