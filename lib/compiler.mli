(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** Compile a separately-defined lexer and parser into a single fused parser.

    This corresponds to the fusion algorithm defined in Figure 6 of

      flap: A Deterministic Parser with Fused Lexing
      PLDI 2023

    (MacoCaml port: [Make] is a template over [TERM]; [compile] is a
    [$let] binding.)
 *)

module Make $(Term : Sigs.TERM) :
sig
  $val compile : 'a. (Reex.t * Fused.Make$(Term).lexer_rhs) list -> 'a Normalize.Make$(Term).t -> (string -> 'a) expr
  (** [compile l p] constructs the code for a fused parser from a lexer [l] and parser [p]. *)
end
