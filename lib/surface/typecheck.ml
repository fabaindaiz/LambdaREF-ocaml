open Common.Env
open Common.Lattice
open Common.Type
open Ast

let rec surface_typing (m : surf) (env : tenv) : ttype =
  match m with
  | Var x -> lookup_env x env
  | Loc l -> TRef (lookup_env l env)
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

let rec gsurface_typing (m : gsurf) (env : genv) : gtype =
  match m with
  | Var x -> lookup_env x env
  | Loc l -> GRef (lookup_env l env)
  | Const k -> GBase (const_typing k)
  | Abs (x, a, n) ->
    let env' = (x, a) :: env in
    let b = gsurface_typing n env' in
    GArrow (a, b)
  | App (l, m) ->
    (match gsurface_typing l env with
    | GArrow (t1, t2) ->
      let t1' = gsurface_typing m env in
      check gtype_consistency t1' t1;
      t2
    | GUnknown -> GUnknown
    | _ -> raise (TypeError "Application type mismatch"))
  | If (l, m, n) ->
    let t1 = gsurface_typing l env in
    check gtype_consistency t1 (GBase TBool);
    let t2 = gsurface_typing m env in
    let t3 = gsurface_typing n env in
    (match gtype_meet t2 t3 with
      | Some t -> t
      | None -> raise (TypeError "Branches of if must have compatible types"))
  | Annot (e, t) ->
    let t' = gsurface_typing e env in
    check gtype_consistency t' t;
    t
  | Ref (e) ->
    let t = gsurface_typing e env in
    GRef t
  | Deref (e) ->
    (match gsurface_typing e env with
    | GRef t -> t
    | _ -> raise (TypeError "Deref can only be applied to references"))
  | Asgn (e, v) ->
    (match gsurface_typing e env with
    | GRef t1 ->
      let t2 = gsurface_typing v env in
      check gtype_consistency t1 t2;
      GBase TUnit
    | _ -> raise (TypeError "Assignment can only be applied to references"))