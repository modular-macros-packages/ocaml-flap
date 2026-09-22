(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)


(** A less-naive code representation (MacoCaml port: operations are macros over ['a expr]).

    Naively using MetaOCaml's built-in code type and built-in
    quotation can lead to programs that generate undesirably-large
    code.  For example, consider the following artificial but
    illustrative program:

      let dup x = (x, x)
      let sum_pair x = fst x + snd x
      let double = fun x -> (sum_pair (dup x))

    Staging the program naively by simply adding quotations and splices
    produces the following:

      let dup x = .< (.~x, .~x) >.
      let sum_pair x = .< fst .~x + snd .~x >.
      let double = .< fun x -> .~(sum_pair (dup .<x>.)) >.

    The generated code for double behaves correctly, but contains
    unnecessary redexes and duplication:

      .<fun x -> (fst (x, x)) + (snd (x, x))>.

    The operations in this module can be used to provide an alternative
    staging:

      let dup x = pair x x 
      let sum_pair x = .< .~(dyn (fst x)) + .~(dyn (snd x)) >.
      let double = .< fun x -> .~(sum_pair (dup (injv .<x>.))) >.

    With this second implementation the generated code is significantly
    smaller:

      .<fun x -> x + x>.
 *)

type _ t
(** A type of code representations.*)

$gen inj : 'a. 'a expr -> 'a t
(** Injection from MetaOCaml's code representations *)

$gen injv : 'a. 'a expr -> 'a t
(** Value injection from MetaOCaml's code representations.  Using
   [injv] rather than [inj] indicates that it is safe to duplicate the
   argument in generated code. *)

$gen dyn : 'a. 'a t -> 'a expr
(** Conversion to MetaOCaml's code representations. *)

$gen let_ : 'a 'b. 'a t -> ('a t -> 'b t) -> 'b t
(** Construct a let binding. 

    [let_ e k] constructs the code
    .< let x = .~e in .~(k .<x>.) >.

    except that it performs some normalization:
    (1) if e is a value then the let binding is (beta-)reduced to [k e]
    (2) When multiple let bindings are combined they are reassociated
        into a linear form:
        [let x1 = e1 in let x2 = e2 in e3]
        rather than 
        [let x2 = (let x1 = e1 in e2) in e3]
 *)

$gen fst : 'a 'b. ('a * 'b) t -> 'a t
(** First projection.  [fst (pair (injv x) (injv y))] simplifies to [injv x].
 *)

$gen snd : 'a 'b. ('a * 'b) t -> 'b t
(** Second projection.  [snd (pair (injv x) (injv y))] simplifies to [injv y].
 *)

$gen pair : 'a 'b. 'a t -> 'b t -> ('a * 'b) t
(** Construct a representation of a pair from two representations.
 *)

(* A macro, thunked: a level-0 value would be invisible to macro callers. *)
$gen unit : unit -> unit t
(** The representation of the unit value ().
 *)
