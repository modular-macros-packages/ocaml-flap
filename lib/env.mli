(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** Typed environments (variables live in the plain flap.vars
    sub-library; the operations here are [$let] bindings, running at
    generation time). *)

module Env $(V: sig type 'a t end) :
sig
  type entry = E : 'a Vars.Var.t * 'a V.t -> entry | Alias : 'a Vars.Var.t * 'a Vars.Var.t -> entry
  type t = private entry list

  $val empty : t
  $val bind : 'a. 'a Vars.Var.t -> 'a V.t -> t -> t
  $val alias : 'a. 'a Vars.Var.t -> 'a Vars.Var.t -> t -> t
  $val canonicalize : 'a. 'a Vars.Var.t -> t -> 'a Vars.Var.t
  $val extend : t -> t -> t

  $val (.%{}) : 'a. t -> 'a Vars.Var.t -> 'a V.t
  (** [env.%{x}] looks up variable [x] in environment [env] *)

  type pp_fn = { m : 'a. Format.formatter -> 'a V.t -> unit }
  $val pp : pp_fn -> Format.formatter -> t -> unit
end
