open Common.Type
open Surface.Ast
open Printf
open CCSexp

exception ParseError of string

let rec parse_const (sexp : sexp) : const =
  match sexp with
  | `Atom "unit" -> Unit
  | _ -> raise (ParseError (sprintf "Not a valid constant %s" (to_string sexp)))

and parse_ttype (sexp : sexp) : ttype =
  match sexp with
  | `Atom "Unit" -> TUnit
  | `List [t1; `Atom "->"; t2] -> TArrow (parse_ttype t1, parse_ttype t2)
  | _ -> raise (ParseError (sprintf "Not a valid ttype %s" (to_string sexp)))

let rec parse_surface (sexp : sexp) : surf =
  match sexp with
  | `Atom s -> Var (s)
  | `List [k] -> Const (parse_const k)
  | `List [`Atom "lam"; `List [`Atom x; a]; n] -> Abs (x, parse_ttype a, parse_surface n)
  | `List [l; m] -> App (parse_surface l, parse_surface m)
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