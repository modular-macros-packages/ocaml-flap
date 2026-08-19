
module type TERM = sig
  type t
  macro compare : t -> t -> int
  macro pp : Format.formatter -> t -> unit
end

module type ORDERED = sig
  type t
  macro compare : t -> t -> int
end
