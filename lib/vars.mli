(** Typed variables (plain library; see the note in vars.ml). *)

module Var :
sig
  type 'a t
  val var : unit -> _ t
  val str : _ t -> string
  val eql : 'a t -> 'b t -> ('a, 'b) Type.eq option
end
