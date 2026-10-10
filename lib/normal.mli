(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(**
  A representation of parsers in Deterministic Greibach Normal Form.

  This corresponds to the normal form defined in Figure 4 of

      flap: A Deterministic Parser with Fused Lexing
      PLDI 2023

  (MacoCaml port: [Make] is a template over [TERM]; its operations are
  [$let] bindings, and the printers that upstream derived are
  hand-written.)
 *)
module Make $(Term : Sigs.TERM) :
sig
  type 'a ntseq = Empty : unit ntseq
                | Cons : 'a Vars.Var.t * 'b ntseq -> ('a * 'b) ntseq
  (** A possibly-empty sequence of typed variables *)

  type 'l prod = Prod : { nonterms : 'n ntseq;
                          semact : string Code.t -> 'n Code.t -> 'l Code.t; } -> 'l prod
  (** A [prod] is part of the right-hand side of a normalized production.
      If the production is

         [N -> T N1 ... Nn { e }]

      then [prod] represents the nonterminal sequence [N1 ... Nn] and
      a staged function that builds the semantic action [e]
      from variables representing the token corresponding to terminal [T]
                              and the parsed values corresponding to nonterminals [Ni] *)

  $val pp_prod : 'a 'b. 'b -> Format.formatter -> 'a prod -> unit
  $val show_prod : 'a 'b. 'b -> 'a prod -> string

  type 'a rhs = { prods : 'a prod Mcol.Mmap$(Term).t;
                  null : 'a Code.t option; }
  (** A [rhs] represents all the productions for a particular nonterminal N:

         [N -> T₁ N₁1 ... N₁n₁ { e₁ }
          N -> T₂ N₂1 ... N₂n₂ { e₂ }
               ...
          N -> Tⱼ Nⱼ1 ... Nⱼnⱼ { eⱼ }
          N -> eps { e } ]

      The two components are:
        a map [prods] mapping terminals [Tᵢ] to corresponding [prod] values [Nᵢ1 ... Nᵢnᵢ { eᵢ }], and
        an optional epsilon component representing [eps { e }].

      For details of the normal form, see sections 2 and 3 of the paper.
  *)

  $val pp_rhs : 'a 'b. 'b -> Format.formatter -> 'a rhs -> unit
  $val show_rhs : 'a 'b. 'b -> 'a rhs -> string

  module RhsV : sig type 'a t = 'a rhs end
  module Productions : module type of Env.Env$(RhsV)

  type 'a grammar = { start : 'a Vars.Var.t; items : Productions.t; }
  (** A [grammar] consists of a start symbol and a mapping from nonterminals to
      right-hand sides of type [rhs] *)

  $val pp_grammar : 'a 'b. 'b -> Format.formatter -> 'a grammar -> unit
  $val show_grammar : 'a 'b. 'b -> 'a grammar -> string

  $val trim : 'a. 'a grammar -> 'a grammar
  (** [trim g] returns a new grammar that is equivalent to [g] but does not
      contain productions that cannot be reached from the start symbol. *)
end
