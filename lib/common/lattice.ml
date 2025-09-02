open Type

module Pttype =
  struct
    type g = gtype
    let create (g : gtype) : g = g
    let rec is_concrete (t : ttype) (g : g) : bool =
      match g with
      | GBase b  ->
        (match t with
        | TBase b' -> b = b'
        | _ -> false)
      | GArrow (g1, g2) ->
        (match t with
        | TArrow (t1, t2) -> is_concrete t1 g1 && is_concrete t2 g2
        | _ -> false)
      | GRef g' ->
        (match t with
        | TRef t' -> is_concrete t' g'
        | _ -> false)
      | GUnknown -> true
    let abstract (g : g) : g = g
  end

let rec gtype_precision (t1: gtype) (t2: gtype) : bool =
  match t1, t2 with
  | GBase b1, GBase b2 -> b1 = b2
  | GArrow (g1, g2), GArrow (g1', g2') -> gtype_precision g1 g1' && gtype_precision g2 g2'
  | GRef g1, GRef g2 -> gtype_precision g1 g2
  | _, GUnknown -> true
  | _ -> false

let rec gtype_consistency (t1: gtype) (t2: gtype) : bool =
  match t1, t2 with
  | _, GUnknown -> true
  | GUnknown, _ -> true
  | GArrow (g1, g2), GArrow (g1', g2') -> gtype_consistency g1 g1' && gtype_consistency g2 g2'
  | GRef g1, GRef g2 -> gtype_consistency g1 g2
  | g1, g2 -> g1 = g2

let rec gtype_meet (t1: gtype) (t2 : gtype) : gtype option =
  match t1, t2 with
  | _, GUnknown -> Some t1
  | GUnknown, _ -> Some t2
  | GBase b1, GBase b2 ->
    if b1 = b2 then
      Some t1
    else None
  | GArrow (g1, g2), GArrow (g1', g2') ->
    (match gtype_meet g1 g1', gtype_meet g2 g2' with
    | Some g1, Some g2 -> Some (GArrow (g1, g2))
    | _ -> None)
  | GRef g1, GRef g2 ->
    (match gtype_meet g1 g2 with
    | Some g -> Some (GRef g)
    | None -> None)
  | _ -> None