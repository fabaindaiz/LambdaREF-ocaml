open Common.Type
open Surface.Ast
open Printf
open CCSexp

exception ParseError of string

let rec parse_const (sexp : sexp) : const =
  match sexp with
  | `Atom "unit" -> Unit
  | `Atom "true" -> True
  | `Atom "false" -> False
  | _ -> raise (ParseError (sprintf "Not a valid constant %s" (to_string sexp)))

and parse_gtype (sexp : sexp) : gtype =
  match sexp with
  | `Atom "Unit" -> GBase TUnit
  | `Atom "Bool" -> GBase TBool
  | `Atom "?" -> GUnknown
  | `List [t1; `Atom "->"; t2] -> GArrow (parse_gtype t1, parse_gtype t2)
  | `List [`Atom "Ref"; t] -> GRef (parse_gtype t)
  | _ -> raise (ParseError (sprintf "Not a valid gtype %s" (to_string sexp)))

let rec parse_gsurface (sexp : sexp) : gsurf =
  match sexp with
  | `Atom s -> Var (s)
  | `List [k] -> Const (parse_const k)
  | `List [`Atom "lam"; `List [`Atom x; a]; n] -> Abs (x, parse_gtype a, parse_gsurface n)
  | `List [`Atom "if"; c; t; e] -> If (parse_gsurface c, parse_gsurface t, parse_gsurface e)
    | `List [e; `Atom "::"; t] -> Annot (parse_gsurface e, parse_gtype t)
  | `List [`Atom "ref"; e] -> Ref (parse_gsurface e)
  | `List [`Atom "!"; e] -> Deref (parse_gsurface e)
  | `List [`Atom "asgn"; e1; e2] -> Asgn (parse_gsurface e1, parse_gsurface e2)
  | `List [l; m] -> App (parse_gsurface l, parse_gsurface m)
  | _ -> raise (ParseError (sprintf "Not a valid term: %s" (to_string sexp)))

and parse_id (sexp : sexp) : string =
  match sexp with
  | `Atom name -> name
  | _ -> raise (ParseError (sprintf "Not a valid name: %s" (to_string sexp)))


let sexp_from_file : string -> CCSexp.sexp =
  fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> raise (ParseError (sprintf "Unable to parse file %s: %s" filename msg))

let sexp_from_string (src : string) : CCSexp.sexp =
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> raise (ParseError (sprintf "Unable to parse string %s: %s" src msg))