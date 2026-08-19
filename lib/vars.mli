(** Typed variables (plain library; see the note in vars.ml). *)

type (_, _) eql = Refl : ('a, 'a) eql

module Var :
sig
  type 'a t
  val var : unit -> _ t
  val str : _ t -> string
  val eql : 'a t -> 'b t -> ('a, 'b) eql option
end
