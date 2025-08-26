open Printf
open Env

exception TypeError of string

type ttype = 
  | TUnit
  | TArrow of ttype *  ttype

let rec string_of_ttype =
  function
  | TUnit -> "Unit"
  | TArrow (a, b) -> sprintf "(%s -> %s)" (string_of_ttype a) (string_of_ttype b)

type const =
  | Unit

let string_of_const =
  function
  | Unit -> "unit"

let const_typing =
  function
  | Unit -> TUnit

type tenv = ttype env

let check (f : 'a -> 'a -> bool) (a : 'a) (b : 'a) : unit =
  if f a b then () else raise (TypeError "Type mismatch")