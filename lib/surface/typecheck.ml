open Common.Env
open Common.Type
open Ast

let rec surface_typing (m : surf) (env : tenv) : ttype =
  match m with
  | Var x -> lookup_env x env
  | Const k -> TBase (const_typing k)
  | Abs (x, a, n) ->
    let env' = (x, a) :: env in
    let b = surface_typing n env' in
    TArrow (a, b)
  | App (l, m) ->
    (match surface_typing l env with
    | TArrow (t1, t2) ->
      let t1' = surface_typing m env in
      check (=) t1' t1;
      t2
    | _ -> raise (TypeError "Application type mismatch"))
  | If (l, m, n) ->
    (match surface_typing l env with
    | TBase TBool ->
      let t2 = surface_typing m env in
      let t3 = surface_typing n env in
      check (=) t2 t3;
      t2
    | _ -> raise (TypeError "If condition must be boolean"))
  | Annot (e, t) ->
    let t' = surface_typing e env in
    check (=) t' t;
    t
  | Ref (e) ->
    let t = surface_typing e env in
    TRef t
  | Deref (e) ->
    (match surface_typing e env with
    | TRef t -> t
    | _ -> raise (TypeError "Deref can only be applied to references"))
  | Asgn (e, v) ->
    (match surface_typing e env with
    | TRef t1 -> 
      let t2 = surface_typing v env in
      check (=) t1 t2;
      TBase TUnit
    | _ -> raise (TypeError "Assignment can only be applied to references"))