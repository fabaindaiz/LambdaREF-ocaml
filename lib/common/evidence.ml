open Lattice
open Type

type ev =
  | Ev of gtype * gtype

let gtype_evidence (g1 : gtype) (g2 : gtype) : ev option =
  if gtype_consistency g1 g2 then
    (match gtype_meet g1 g2 with
    | Some g -> Some (Ev (g, g))
    | None -> None)
  else
    None