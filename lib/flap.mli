(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** The signature of token-type parameters (MacoCaml port: the
    [compare]/[pp] operations upstream derived are [$val]s here; see
    sigs.ml for the wrapping pattern). *)
module Sigs = Sigs

(** [Parse [Term]] is a template over [TERM]: its combinators are
    [$let] bindings, and [compile] returns the generated parser as an
    expression for a splice. *)
module Parse $(Term : Sigs.TERM) :
sig
  type _ t
  (** The type of parsers *)

  $val eps : 'a. 'a Code.t -> 'a t
  (** [eps v] succeeds without consuming input and returns [v] *)

  $val ( >>> ) : 'a 'b. 'a t -> 'b t -> ('a * 'b) t
  (** [p >>> q] parses successive prefixes of the input using [p] and then [q]
      and returns a pair of the result of the two parses *)

  $val tok : 'a. Term.t -> (string Code.t -> 'a Code.t) -> 'a t
  (** [term t] parses a token whose tag is [t], or fails *)

  $val bot : 'a. 'a t
  (** [bot] fails without consuming input *)

  $val ( <|> ) : 'a. 'a t -> 'a t -> 'a t
  (** [p <|> q] parses using either [p] or [q], which must be disjoint
     --- that is, they must not both accept the empty string, and
     their first sets must not overlap *)

  $val any : 'a. 'a t list -> 'a t
  (** [any [p1; p2; ... pn]] is [p1 <|> p2 <|> ... <|> pn]  *)

  $val ( $ ) : 'a 'b. 'a t -> ('a Code.t -> 'b Code.t) -> 'b t
  (** If [p] parses the input and returns [v] then
      [p $ f] parses the input and returns [f v].
      Write it spaced: a glued [$x] is a splice. *)

  $val ( $. ) : 'a 'b. 'a t -> ('a Code.t -> 'b Code.t) -> 'b t
  [@@deprecated "( $. ) is the port's old name for ( $ )"]
  (** [( $. )] is the port's old name for [( $ )], kept as a
      deprecated alias. *)

  $val fix : 'b. ('b t -> 'b t) -> 'b t
  (** [fix (fun x -> e)] creates a recursive parser [e].  In the body [e]
      the parser can be accessed using the bound variable [x]. *)

  $val star : 'a. 'a t -> 'a list t
  (** [star p] repeatedly parses successive prefixes of the input using [p],
      and returns a list of the zero or more results *)

  type rhs = Skip | Error of string | Return of Term.t
  $val compile : 'a. (Reex.t * rhs) list -> 'a t -> ((string -> 'a) expr, string) result
  (** [compile l p] builds code [Ok c] for a lexer [l] and type-checked parser [p],
      or fails with an error message [Error e] if the parser does not satisfy the
      typing constraints (e.g. if the grammar is ambiguous or contains left-recursion) *)
end

module Cd : module type of Code
