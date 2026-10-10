(*
 * Copyright (c) 2023 Neel Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)


module Sigs = Sigs

module Parse $(Term : Sigs.TERM) = struct
  module V = Code
  module C =
  struct
    include Mcol.Mset$(Term)
    $let pp : Format.formatter -> t -> unit =
      fun fmt cs -> Format.fprintf fmt "{ ";
                    iter (fun t -> Format.fprintf fmt "%a, " Term.pp t) cs;
                    Format.fprintf fmt "}"
  end

  module Tp = struct
    type t = { first : C.t; follow : C.t; null : bool; guarded : bool }

    $let pp : Format.formatter -> t -> unit =
      fun fmt t ->
      Format.fprintf fmt "{ first = %a; follow = %a; null = %b; guarded = %b }"
        (fun fmt s -> C.pp fmt s) t.first (fun fmt s -> C.pp fmt s) t.follow t.null t.guarded

    $let equal : t -> t -> bool =
      fun t1 t2 ->
         (t1.null = t2.null)
      && (t1.guarded = t2.guarded)
      && C.equal t1.first t2.first
      && C.equal t1.follow t2.follow

    $let nonoverlapping : C.t -> C.t -> bool =
      fun cs cs' -> C.is_empty (C.inter cs cs')
    $let (++) : C.t -> C.t -> C.t = fun x y -> C.union x y
    $let (==>) : bool -> C.t -> C.t = fun b cs -> if b then cs else C.empty

    $let apart : t -> t -> bool =
      fun t1 t2 -> not t1.null && (nonoverlapping t1.follow t2.first)
    $let disjoint : t -> t -> bool =
      fun t1 t2 -> not (t1.null && t2.null) && nonoverlapping t1.first t2.first

    $let eps : t =
      { first = C.empty; follow = C.empty; null = true; guarded = true }
    $let tok : Term.t -> t = fun c ->
      { first = C.singleton c; follow = C.empty; null = false; guarded = true }
    $let seq : t -> t -> t = fun t1 t2 ->
      if apart t1 t2 then {
          first = t1.first;
          follow = (++) t2.follow ((==>) t2.null ((++) t2.first t1.follow));
          null = false;
          guarded = t1.guarded
        } else Format.kasprintf failwith "seq %a %a" (fun fmt t -> pp fmt t) t1 (fun fmt t -> pp fmt t) t2
    $let bot : t =
      { first = C.empty; follow = C.empty; null = false; guarded = true }
    $let alt : t -> t -> t = fun t1 t2 ->
      if disjoint t1 t2 then {
          first = (++) t1.first t2.first;
          follow = (++) t1.follow t2.follow;
          null = t1.null || t2.null;
          guarded = t1.guarded && t2.guarded;
        } else Format.kasprintf failwith "alt %a %a" (fun fmt t -> pp fmt t) t1 (fun fmt t -> pp fmt t) t2

    $let star : t -> t = fun t ->
      {(seq t t) with null = true; follow = (++) t.first t.follow}

    $let min : t =
      { first = C.empty; follow = C.empty; null = false; guarded = false }

    $let fix : (t -> t) -> t = fun f ->
      let rec loop tp =
        let tp' = f tp in
        if equal tp tp' then
          tp
        else
          loop tp'
      in
      loop min
  end

  module Var = struct
    type ('ctx, 'a) t =
      | Z : ('a * 'rest, 'a) t
      | S : ('rest, 'a) t -> ('b * 'rest, 'a) t
  end

  module Env $(T : sig type 'a t end) = struct
    type 'ctx t =
      | [] : unit t
      | (::) : 'a T.t * 'ctx t -> ('a * 'ctx) t

    $let rec lookup : 'ctx 'a. 'ctx t -> ('ctx, 'a) Var.t -> 'a T.t =
      fun (type ctx a) (env : ctx t) (n : (ctx, a) Var.t) : a T.t ->
      match n, env with
      | Z, x :: _   -> x
      | S n, _ :: xs -> lookup xs n

    type fn = {f : 'a. 'a T.t -> 'a T.t}
    $let rec map : 'ctx. fn -> 'ctx t -> 'ctx t =
      fun (type ctx) {f} (ctx : ctx t) : ctx t ->
      match ctx with
      | [] -> []
      | x :: xs -> f x :: map {f} xs
  end

  type 'a tag = Term.t * (string V.t -> 'a V.t)

  module Grammar = struct
    type ('ctx, 'a) var = ('ctx, 'a) Var.t =
      | Z : ('a * 'rest, 'a) var
      | S : ('rest, 'a) var -> ('b * 'rest, 'a) var

    type ('ctx, 'a, 'd) t' =
      Eps : 'a V.t -> ('ctx, 'a, 'd) t'
    | Seq : ('ctx, 'a, 'd) t * ('ctx, 'b, 'd) t -> ('ctx, 'a * 'b, 'd) t'
    | Tok : 'a tag -> ('ctx, 'a, 'd) t'
    | Bot : ('ctx, 'a, 'd) t'
    | Alt : ('ctx, 'a, 'd) t * ('ctx, 'a, 'd) t -> ('ctx, 'a, 'd) t'
    | Map : ('a V.t -> 'b V.t) * ('ctx, 'a, 'd) t -> ('ctx, 'b, 'd) t'
    | Fix : ('a * 'ctx, 'a, 'd) t -> ('ctx, 'a, 'd) t'
    | Var : ('ctx,'a) var -> ('ctx, 'a, 'd) t'
    | Star : ('ctx, 'a, 'd) t -> ('ctx, 'a list, 'd) t'
    and ('ctx, 'a, 'd) t = 'd * ('ctx, 'a, 'd) t'

    module TpV = struct type 'a t = Tp.t end
    module TpEnv = Env$(TpV)

    $let data : 'ctx 'a 'd. ('ctx, 'a, 'd) t -> 'd = fun (d, _) -> d

    $let rec typeof : 'ctx 'a 'd. 'ctx TpEnv.t -> ('ctx, 'a, 'd) t -> ('ctx, 'a, Tp.t) t  =
      fun (type ctx a d) (env : ctx TpEnv.t) ((_, g) : (ctx, a, d) t) : (ctx, a, Tp.t) t ->
      match g with
      | Eps v -> (Tp.eps, Eps v)
      | Seq (g1, g2) -> let guarded tp = {tp with Tp.guarded = true} in
                        let g1 = typeof env g1 in
                        let g2 = typeof (TpEnv.map {TpEnv.f = guarded} env) g2 in
                        (Tp.seq (data g1) (data g2), Seq(g1, g2))
      | Tok (tag,f) -> (Tp.tok tag, Tok (tag,f))
      | Bot  -> (Tp.bot, Bot)
      | Alt (g1, g2) -> let g1 = typeof env g1 in
                        let g2 = typeof env g2 in
                        (Tp.alt (data g1) (data g2), Alt(g1, g2))
      | Map(f, g') -> let g' = typeof env g' in
                      (data g', Map(f, g'))
      | Star g -> let g = typeof env g in
                  (Tp.star (data g), Star g)
      | Fix g'  -> let tp = Tp.fix (fun tp -> data (typeof (tp :: env) g')) in
                   if tp.guarded then let g' = typeof (tp :: env) g' in (data g', Fix g')
                   else Format.kasprintf failwith "guarded %a" (fun fmt t -> Tp.pp fmt t) tp
      | Var n -> (TpEnv.lookup env n, Var n)
  end

  module HOAS = struct
    (* Higher-order abstract syntax front-end to the parser
     combinators *)

    module UnitV = struct type 'a t = unit end
    module Ctx = Env$(UnitV)

    $let rec len : 'n. 'n Ctx.t -> int =
      fun (type n) (ctx : n Ctx.t) : int ->
      match ctx with
      | [] -> 0
      | () :: ctx -> 1 + len ctx

    $let rec tshift' : 'a 'i 'j. int -> 'j Ctx.t -> ('a * 'i) Ctx.t -> ('j, 'a) Var.t =
      fun (type a i j) (n : int) (c1 : j Ctx.t) (c2 : (a * i) Ctx.t) : (j, a) Var.t ->
      match n, c1, c2 with
        (* Both the 'assert false' and the 'Obj.magic' are safe here,
           since we know (although it's not captured in the types) that
           (a * i) is a prefix of j.

           More details: "Unembedding Domain Specific Languages" §4.4.
         *)
      |        _, [], _ -> assert false
      | 0, () :: _, () :: _ -> Obj.magic Var.Z
      | n, () :: c1, c2 -> Var.S (tshift' (n-1) c1 c2)

    $let tshift : 'a 'i 'j. 'j Ctx.t -> ('a * 'i) Ctx.t -> ('j, 'a) Var.t =
      fun c1 c2 -> tshift' (len c1 - len c2) c1 c2

    type 'a t = { untdb : 'ctx. 'ctx Ctx.t -> ('ctx, 'a, unit) Grammar.t }

    $let eps : 'a. 'a V.t -> 'a t = fun a -> { untdb = fun _ -> (), Eps a }
    $let (>>>) : 'a 'b. 'a t -> 'b t -> ('a * 'b) t =
      fun f g -> { untdb = fun i -> (), Seq (f.untdb i, g.untdb i) }
    $let tok : 'a. Term.t -> (string V.t -> 'a V.t) -> 'a t =
      fun t f -> { untdb = fun _ -> (), Tok (t, f) }
    $let bot : 'a. 'a t = { untdb = fun _ -> (), Bot }
    $let (<|>) : 'a. 'a t -> 'a t -> 'a t =
      fun f g -> { untdb = fun i -> (), Alt (f.untdb i, g.untdb i) }
    $let any : 'a. 'a t list -> 'a t = fun gs -> List.fold_left (fun x y -> (<|>) x y) bot gs
    $let ( $ ) : 'a 'b. 'a t -> ('a V.t -> 'b V.t) -> 'b t =
      fun g f -> { untdb = fun i -> (), Map (f, g.untdb i) }
    $let (( $. ) [@deprecated "( $. ) is the port's old name for ( $ )"]) = ( $ )
    $let fix : 'b. ('b t -> 'b t) -> 'b t =
      fun f ->
      { untdb = fun i ->
                (), Fix ((f {untdb = fun j -> (), Var (tshift j (() :: i))}).untdb
                       (() :: i)) }
    $let star : 'a. 'a t -> 'a list t = fun g -> { untdb = fun i -> (), Star (g.untdb i) }
  end
  include HOAS

  module Normalize =
  struct
    module G = Normalize.Make$(Term)

    module Ctx = Env$(G)

    $let rec norm : 'a 'ctx. 'ctx Ctx.t -> ('ctx, 'a, Tp.t) Grammar.t -> 'a G.t =
      fun (type a ctx) (ctx : ctx Ctx.t) (g : (ctx, a, Tp.t) Grammar.t) : a G.t ->
      match g with
      | _, Eps v -> G.eps v
      | _, Seq (g, h) -> G.(norm ctx g >>> norm ctx h)
      | _, Tok (t, f) -> G.(tok (t $$ f))
      | _, Bot -> G.bot ()
      | _, Alt (g, h) -> G.(norm ctx g <|> norm ctx h)
      | _, Map (f, g) -> G.(norm ctx g $ f)
      | t, Fix body -> G.fix t.Tp.first (fun h -> norm (h :: ctx) body)
      | _, Var x -> Ctx.lookup ctx x
      | _, Star _ -> failwith "star normalization not yet implemented"
  end

  module Compiler = Compiler.Make$(Term)
  type rhs = Fused.Make$(Term).lexer_rhs = Skip | Error of string | Return of Term.t
  $let type_check : 'a. 'a t -> (unit, 'a, Tp.t) Grammar.t =
    fun {untdb} -> Grammar.typeof [] (untdb [])
  $let compile : 'a. (Reex.t * rhs) list -> 'a t -> ((string -> 'a) expr, string) result =
    fun l p ->
    match type_check p with
    | p -> Ok (Compiler.compile l (Normalize.norm [] p))
    | exception (Failure e) -> Error e
end
module Cd = Code
