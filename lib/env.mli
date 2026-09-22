(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** Typed environments (variables live in the plain flap.vars
    sub-library; the operations here are macros, running at
    generation time). *)

module Env [V: sig type 'a t end] :
sig
  type entry = E : 'a Vars.Var.t * 'a V.t -> entry | Alias : 'a Vars.Var.t * 'a Vars.Var.t -> entry
  type t = private entry list

  $gen empty : unit -> t
  $gen bind : 'a. 'a Vars.Var.t -> 'a V.t -> t -> t
  $gen alias : 'a. 'a Vars.Var.t -> 'a Vars.Var.t -> t -> t
  $gen canonicalize : 'a. 'a Vars.Var.t -> t -> 'a Vars.Var.t
  $gen extend : t -> t -> t

  (** [find env x] looks up variable [x] in environment [env]
      (the original's [env.%{x}]) *)
  $gen find : 'a. t -> 'a Vars.Var.t -> 'a V.t
  type pp_fn = { m : 'a. Format.formatter -> 'a V.t -> unit }
  $gen pp : pp_fn -> Format.formatter -> t -> unit
end
