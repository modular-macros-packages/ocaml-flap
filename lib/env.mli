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

  macro empty : unit -> t
  macro bind : 'a. 'a Vars.Var.t -> 'a V.t -> t -> t
  macro alias : 'a. 'a Vars.Var.t -> 'a Vars.Var.t -> t -> t
  macro canonicalize : 'a. 'a Vars.Var.t -> t -> 'a Vars.Var.t
  macro extend : t -> t -> t
  macro find : 'a. t -> 'a Vars.Var.t -> 'a V.t
  (** [find env x] looks up variable [x] in environment [env]
      (the original's [env.%{x}]) *)
  type pp_fn = { m : 'a. Format.formatter -> 'a V.t -> unit }
  macro pp : pp_fn -> Format.formatter -> t -> unit
end
