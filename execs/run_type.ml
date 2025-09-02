open Common.Type
open Surface.Typecheck
open Parsing.Parse
open Printf

let () =
  let args = Sys.argv in
  if Array.length args > 1 && Sys.file_exists args.(1)
  then
    let src = sexp_from_file args.(1) in
    printf "%s\n" (string_of_gtype (gsurface_typing (parse_gsurface src) []))
  else
    printf "usage: run_parse.exe <filename>\n"