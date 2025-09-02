open Common.Env
open Common.Type
open Ast

let rec gcore_sval_typing (m: core_sval) (env : genv) : gtype =
  match m with
  | Var x -> lookup_env x env
  | Loc (l, t) -> GRef t
  | Const k -> GBase (const_typing k)
  | Abs (x, a, n) ->
    let env' = (x, a) :: env in
    let b = gcore_typing n env' in
    GArrow (a, b)

and gcore_val (m : core_val) (env : genv) : gtype =
  match m with
  | Simple s -> gcore_sval_typing s env
  | SAnnot (ev, s, g2) ->
    let g1 = gcore_sval_typing s env in
    (* TODO: use evidence *)
    check gtype_consistency g1 g2;
    g2

let rec gcore_typing (m : gcore) (env : genv) : gtype =
  match m with
  | Value v -> gcore_val v env
  | App (l, m) ->
    (match )
  | If (l, m, n) ->
    (match l with
    | Et (Ev (GBase TBool, GBase TBool), l') ->
      let g1 = gcore_typing l' env in
      check gtype_consistency g1 (GBase TBool);
      let t2 = gcore_typing m env in
      let t3 = gcore_typing n env in
      (match gtype_meet t2 t3 with
      | Some t -> t
      | None -> raise (TypeError "Branches of if must have compatible types"))
    | _ -> raise (TypeError "If condition must have Bool evidence"))


  | App (l, m) ->
    (match gsurface_typing l env with
    | GArrow (t1, t2) ->
      let t1' = gsurface_typing m env in
      check gtype_consistency t1' t1;
      t2
    | GUnknown -> GUnknown
    | _ -> raise (TypeError "Application type mismatch"))
  
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