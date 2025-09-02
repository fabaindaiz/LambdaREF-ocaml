open Printf
open Common.Type
open Common.Evidence

type gcore_sval =
  | Var of string
  | Loc of string * gtype
  | Const of const
  | Abs of string * gtype * gcore

and gcore_val =
  | Simple of gcore_sval
  | SAnnot of ev * gcore_sval * gtype

and gcore_et =
  | Et of ev * gcore

and gcore =
  | Value of gcore_val
  | App of gtype * gcore_et * gcore_et
  | If of gcore_et * gcore_et * gcore_et
  | Annot of gcore_et * gtype
  | Ref of  gtype * gcore_et
  | Deref of gtype * gcore_et
  | Asgn of gtype * gcore_et * gcore_et