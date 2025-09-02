open Printf
open Common.Type

type surf =
  | Var of string
  | Loc of string
  | Const of const
  | Abs of string * ttype * surf
  | App of surf * surf
  | If of surf * surf * surf
  | Annot of surf * ttype
  | Ref of surf
  | Deref of surf
  | Asgn of surf * surf

let rec string_of_surf =
  function
  | Var s -> s
  | Loc l -> sprintf "[%s]" l
  | Const k -> sprintf "(%s)" (string_of_const k)
  | Abs (x, a, n) -> sprintf "(λ %s:%s.%s)" x (string_of_ttype a) (string_of_surf n)
  | App (l, m) -> sprintf "(%s %s)" (string_of_surf l) (string_of_surf m)
  | If (c, t, e) -> sprintf "(if %s then %s else %s)" (string_of_surf c) (string_of_surf t) (string_of_surf e)
  | Annot (e, t) -> sprintf "(%s :: %s)" (string_of_surf e) (string_of_ttype t)
  | Ref r -> sprintf "(ref %s)" (string_of_surf r)
  | Deref d -> sprintf "(! %s)" (string_of_surf d)
  | Asgn (l, r) -> sprintf "(%s := %s)" (string_of_surf l) (string_of_surf r)

type gsurf =
  | Var of string
  | Loc of string
  | Const of const
  | Abs of string * gtype * gsurf
  | App of gsurf * gsurf
  | If of gsurf * gsurf * gsurf
  | Annot of gsurf * gtype
  | Ref of gsurf
  | Deref of gsurf
  | Asgn of gsurf * gsurf

let rec string_of_gsurf =
  function
  | Var s -> s
  | Loc l -> sprintf "[%s]" l
  | Const k -> sprintf "(%s)" (string_of_const k)
  | Abs (x, a, n) -> sprintf "(λ %s:%s.%s)" x (string_of_gtype a) (string_of_gsurf n)
  | App (l, m) -> sprintf "(%s %s)" (string_of_gsurf l) (string_of_gsurf m)
  | If (c, t, e) -> sprintf "(if %s then %s else %s)" (string_of_gsurf c) (string_of_gsurf t) (string_of_gsurf e)
  | Annot (e, t) -> sprintf "(%s :: %s)" (string_of_gsurf e) (string_of_gtype t)
  | Ref r -> sprintf "(ref %s)" (string_of_gsurf r)
  | Deref d -> sprintf "(! %s)" (string_of_gsurf d)
  | Asgn (l, r) -> sprintf "(%s := %s)" (string_of_gsurf l) (string_of_gsurf r)