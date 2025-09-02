open Printf
open Env

exception TypeError of string

type base_type =
  | TUnit
  | TBool

let string_of_base_type =
  function
  | TUnit -> "Unit"
  | TBool -> "Bool"

type const =
  | Unit
  | True
  | False

let string_of_const =
  function
  | Unit -> "unit"
  | True -> "true"
  | False -> "false"

let const_typing =
  function
  | Unit -> TUnit
  | True | False -> TBool

type ttype = 
  | TBase of base_type
  | TArrow of ttype * ttype
  | TRef of ttype

let rec string_of_ttype =
  function
  | TBase b -> string_of_base_type b
  | TArrow (a, b) -> sprintf "(%s -> %s)" (string_of_ttype a) (string_of_ttype b)
  | TRef r -> sprintf "(ref %s)" (string_of_ttype r)

type tenv = ttype env

type gtype = 
  | GBase of base_type
  | GArrow of gtype * gtype
  | GRef of gtype
  | GUnknown

let rec string_of_gtype =
  function
  | GBase b -> string_of_base_type b
  | GArrow (a, b) -> sprintf "(%s -> %s)" (string_of_gtype a) (string_of_gtype b)
  | GRef r -> sprintf "(ref %s)" (string_of_gtype r)
  | GUnknown -> "?"

type genv = gtype env

let check (f : 'a -> 'a -> bool) (a : 'a) (b : 'a) : unit =
  if f a b then () else raise (TypeError "Type mismatch")