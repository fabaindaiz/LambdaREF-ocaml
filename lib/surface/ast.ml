open Printf
open Common.Type

type surf =
  | Var of string
  | Const of const
  | Abs of string * ttype * surf
  | App of surf * surf

let rec string_of_surf =
  function
  | Var s -> s
  | Const k -> sprintf "(%s)" (string_of_const k)
  | Abs (x, a, n) -> sprintf "(λ %s:%s.%s)" x (string_of_ttype a) (string_of_surf n)
  | App (l, m) -> sprintf "(%s %s)" (string_of_surf l) (string_of_surf m)

type 'a tsurf =
  | Var of string * 'a
  | Const of const * 'a
  | Abs of string * ttype * 'a tsurf * 'a
  | App of 'a tsurf * 'a tsurf * 'a

let tsurf_type (m : 'a tsurf) : 'a =
  match m with
  | Var (_, t) -> t
  | Const (_, t) -> t
  | Abs (_, _, _, t) -> t
  | App (_, _, t) -> t