open Printf
open Env

exception TypeError of string

type base_type =
  | TUnit
  | TBool

type ttype = 
  | TBase of base_type
  | TArrow of ttype * ttype
  | TRef of ttype

let string_of_base_type =
  function
  | TUnit -> "Unit"
  | TBool -> "Bool"

let rec string_of_ttype =
  function
  | TBase b -> string_of_base_type b
  | TArrow (a, b) -> sprintf "(%s -> %s)" (string_of_ttype a) (string_of_ttype b)
  | TRef r -> sprintf "(ref %s)" (string_of_ttype r)

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

type tenv = ttype env

let check (f : 'a -> 'a -> bool) (a : 'a) (b : 'a) : unit =
  if f a b then () else raise (TypeError "Type mismatch")