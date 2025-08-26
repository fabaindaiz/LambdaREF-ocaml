(** Interpreter **)
open Common.Env
open Surface.Ast
open Core.Ast
open Core.Typecheck
open Core.Value
open Compile
open Printf

exception InterpError of string

(* Type checks *)
let raise_type_error (expected_type_s : string) (ill_value : value) : 'a =
  raise (InterpError (sprintf "Type error: Expected %s but got %s" expected_type_s (string_of_val ill_value)))


(* interpreter *)
let rec interp (u : core) (env : venv) : value =
  match u with
  | Var x -> lookup_env x env
  | Const k -> Const k
  | Abs (x, a, n) -> Lambda (x, a, n)
  | App (l, m) ->
    (match interp l env with
    | Lambda (x, _, n) ->
      let v = interp m env in
      let env' = (x, v) :: env in
      interp n env'
    | _ -> raise_type_error "Lambda low" (interp l env))

let interp (m : surf) (env : venv) =
  let u = compile_core m [] in
  let _ = core_typing u [] in
  interp u env