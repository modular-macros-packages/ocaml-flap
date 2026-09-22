
module type TERM = sig
  type t
  $gen compare : t -> t -> int
  $gen pp : Format.formatter -> t -> unit
end

module type ORDERED = sig
  type t
  $gen compare : t -> t -> int
end
