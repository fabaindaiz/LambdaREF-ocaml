open Alcotest
open Parsing.Parse
open Driver.Compile
open Driver.Interp
open Surface.Ast
open Core.Ast
open Core.Value

(* Testing arithmetic expression using the print function defined in Interp 
   and the default equality for comparison *)
let core : core testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_core e)) (=)

let surf : surf testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_surf e)) (=)

let value : value testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_val e)) (=)

(* parser tests *)
let test_parse_unit () =
  check surf "same unit" (parse_surface (`List [`Atom "unit"])) (Const Unit)

let test_parse_lambda () =
  check surf "same lambda" (parse_surface (`List [`Atom "lam"; `List [`Atom "x"; `Atom "Unit"]; `Atom "x"])) (Abs("x", TUnit, Var "x"))

(* compile tests *)
let test_compile_unit () =
  check core "same unit" (compile_core (Const Unit) []) (Const Unit)

let test_compile_lambda () =
  check core "same lambda" (compile_core (Abs("x", TUnit, Var "x")) []) (Abs("x", TUnit, Var "x"))

(* interp tests *)
let test_interp_unit () =
  check value "same unit" (interp (Const Unit) []) (Const Unit)

let test_interp_lambda () =
  check value "same lambda" (interp (Abs("x", TUnit, Var "x")) []) (Lambda ("x", TUnit, Var "x"))

(* OCaml tests: extend with your own tests *)
let ocaml_tests = [
  "parser", [
    test_case "A unit" `Quick test_parse_unit;
    test_case "A lambda" `Quick test_parse_lambda;
  ];
  "compile", [
    test_case "A unit" `Quick test_compile_unit;
    test_case "A lambda" `Quick test_compile_lambda;
  ];
  "interp", [
    test_case "A unit" `Quick test_interp_unit;
    test_case "A lambda" `Quick test_interp_lambda;
  ];
  "errors", [
  ]
]  

let () =
  run "OCaml Tests" (ocaml_tests)