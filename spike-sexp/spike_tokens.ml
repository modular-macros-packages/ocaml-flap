type t = LP | RP | ATOM | QATOM

let compare (a : t) b = Stdlib.compare a b

let pp fmt t =
  Format.pp_print_string fmt
    (match t with LP -> "LP" | RP -> "RP" | ATOM -> "ATOM" | QATOM -> "QATOM")
