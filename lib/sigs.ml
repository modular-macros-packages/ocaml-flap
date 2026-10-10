
module type TERM = sig
  type t
  $val compare : t -> t -> int
  $val pp : Format.formatter -> t -> unit
end

module type ORDERED = sig
  type t
  $val compare : t -> t -> int
end
