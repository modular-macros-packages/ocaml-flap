(*
 * Copyright (c) 2021 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)


$let pr : 'a. Format.formatter -> ('a, Format.formatter, unit) format -> 'a =
  fun fmt f -> Format.fprintf fmt f

module Unresolved $(Term : Sigs.TERM) =
struct
  (** Normal form grammars with unresolved references *)

  module TermMap = Mcol.Mmap$(Term)
  module Terms = Mcol.Mset$(Term)

  (** prod ::= {T} NT* ↦ S[e] *)
  type 'l prod = Prod : { nonterms: 'n ntseq; semact: string Code.t -> 'n Code.t -> 'l Code.t } -> 'l prod
               | Prod_of of Term.t * 'l Vars.Var.t
               | Map : ('m Code.t -> 'l Code.t) * 'm prod -> 'l prod
  and 'a ntseq =
    Empty : unit ntseq
  | One of 'a Vars.Var.t
  | Nonterms_of of Term.t * 'a Vars.Var.t
  | Append : 'a ntseq * 'b ntseq -> ('a * 'b) ntseq
  type 'a null =
    Some of 'a Code.t
  | None
  | Null_of of 'a Vars.Var.t
  | MapNull : ('a Code.t -> 'b Code.t) * 'a null -> 'b null
  | Either of 'a null * 'a null
  type 'a rhs = { prods: 'a prod TermMap.t; null: 'a null; (* Do we accept the empty string? *) }

  module RhsV = struct type 'a t = 'a rhs end
  module Productions = Env.Env$(RhsV)

  type 'a grammar = {
    start: 'a Vars.Var.t;
    items: Productions.t;
  }
  type 'a t = 'a grammar

  $let rec pp_prod : 'a. Format.formatter -> 'a prod -> unit =
    fun (type a) fmt (p : a prod) ->
    match p with
    | Prod p -> pp_ntseq fmt p.nonterms
    | Prod_of (t, x) -> pr fmt "(Prod_of %a %s)" Term.pp t (Vars.Var.str x)
    | Map (_, p) -> pr fmt "(Map <fun> %a)" (fun fmt p -> pp_prod fmt p) p
  and pp_ntseq : 'a. Format.formatter -> 'a ntseq -> unit =
    fun (type a) fmt (s : a ntseq) ->
    match s with
    | Empty -> ()
    | One x -> pr fmt "%s" (Vars.Var.str x)
    | Nonterms_of (t, x) -> pr fmt "(Nonterms_of %a %s)" Term.pp t (Vars.Var.str x)
    | Append (x, y) -> pr fmt "@[%a@ %a@]" (fun fmt s -> pp_ntseq fmt s) x (fun fmt s -> pp_ntseq fmt s) y

  $let rec pp_null : 'a. Format.formatter -> 'a null -> unit =
    fun (type a) fmt (n : a null) ->
    match n with
    | Some _ -> pr fmt "Some"
    | None   -> pr fmt "None"
    | Null_of x -> pr fmt "(Null_of %s)" (Vars.Var.str x)
    | MapNull (_, n) -> pp_null fmt n
    | Either (l, r) -> pr fmt "(Either %a %a)" (fun fmt n -> pp_null fmt n) l (fun fmt n -> pp_null fmt n) r

  $let pp_rhs : 'a. Format.formatter -> 'a rhs -> unit =
    fun fmt r ->
    begin
      TermMap.iter (fun t p -> pr fmt " |@ %a @[%a@]" Term.pp t (fun fmt p -> pp_prod fmt p) p) r.prods;
      pr fmt "|@ ";
      pp_null fmt r.null
    end

  $let pp_productions : Format.formatter -> Productions.t -> unit =
    fun fmt p ->
    Productions.pp { Productions.m = fun fmt r -> pp_rhs fmt r } fmt p

  $let pp_grammar : 'a. Format.formatter -> 'a grammar -> unit =
    fun fmt g -> pr fmt "<%s,@ @[%a@]>" (Vars.Var.str g.start) (fun fmt p -> pp_productions fmt p) g.items
  $let pp : 'a 'b. 'b -> Format.formatter -> 'a grammar -> unit =
    fun _ fmt g -> pp_grammar fmt g
  $let show : 'a 'b. 'b -> 'a grammar -> string =
    fun _ g -> Format.asprintf "%a" (fun fmt g -> pp_grammar fmt g) g

  type 'a tok = Term.t * (string Code.t -> 'a Code.t)
  $let ($$) : 'a. Term.t -> (string Code.t -> 'a Code.t) -> 'a tok = fun t f -> (t, f)

  open Productions
  $let fresh : 'a. ('a Vars.Var.t -> Productions.t) -> 'a grammar =
    fun k -> let start = Vars.Var.var () in { start; items = k start }

  $let eps : 'a. 'a Code.t -> 'a grammar =
    fun v -> fresh @@ fun s -> bind s {prods=TermMap.empty; null=Some v} empty

  $let seqprod : 'a 'b. 'a Vars.Var.t -> Term.t -> 'b Vars.Var.t -> ('a * 'b) prod =
    fun a t s -> Prod { nonterms = Append (Nonterms_of (t, a), One s);
                        semact = (fun _ n -> n) }

  $let ( >>> ) : 'a 'b. 'a grammar -> 'b grammar -> ('a * 'b) grammar =
    fun g g' -> fresh @@ fun start ->
    let gstarts = g.items.%{g.start} in
    bind start {prods=TermMap.mapi (fun t _ -> seqprod g.start t g'.start) gstarts.prods; null=None} (extend g.items g'.items)

  $let tok : 'a. 'a tok -> 'a grammar =
    fun (t,f) -> fresh @@ fun start ->
              bind start {prods=TermMap.singleton t (Prod {nonterms = Empty; semact = (fun c _ -> f c)});
                          null=None} empty

    $let bot : 'a. unit -> 'a grammar =
    fun () -> fresh @@ fun s -> bind s {prods=TermMap.empty; null=None} empty

  $let ( <|> ) : 'a. 'a grammar -> 'a grammar -> 'a grammar =
    fun g g' ->
    fresh @@ fun start -> (* guaranteed first sets distinct *)
    bind start {prods=TermMap.union (fun _ _ _ -> assert false) g.items.%{g.start}.prods g'.items.%{g'.start}.prods ;
                null = Either (g.items.%{g.start}.null, g'.items.%{g'.start}.null) } (extend g.items g'.items)

  $let ( $ ) : 'a 'b. 'a grammar -> ('a Code.t -> 'b Code.t) -> 'b grammar =
    fun { start ; items } f -> fresh @@ fun start' ->
    let startprods = items.%{start} in
    let prods = TermMap.map (fun p -> Map (f, p)) startprods.prods in
    let null = MapNull (f, startprods.null) in
    bind start' {prods; null} items

  $let fix : 'a. Terms.t -> ('a grammar -> 'a grammar) -> 'a grammar =
    fun ts f ->
    (* We've arranged things above so that nothing actually accesses the entries for a particular
       grammar; instead, it inserts a reference (e.g. 'Prod_of') to resolve later.
       (There may be some subtleties here: does the semact end up as a recursive function?) *)
    let start = Vars.Var.var () in
    let prods = Terms.fold (fun t m -> TermMap.add t (Prod_of (t, start)) m) ts TermMap.empty in
    let dummy = { start; items = bind start {prods; null = Null_of start} empty } in
    let g = f dummy in
    {g with items = alias start g.start g.items}
end

module Make $(Term : Sigs.TERM) =
struct
  module N = Normal.Make$(Term)
  module U = Unresolved$(Term)

  (** Translation to eliminate unresolved references, producing fully normalized grammars *)

  $let rec prod : 'l. U.Productions.t -> 'l U.prod -> 'l N.prod =
    fun (type l) prods (p : l U.prod) : l N.prod ->
    match p with
    | U.Prod {nonterms=Empty; semact} ->
       N.Prod {nonterms=Empty; semact}
    | U.Prod {nonterms=One x; semact} ->
       N.Prod {nonterms=Cons (U.Productions.canonicalize x prods, Empty); semact = fun t n -> semact t (Code.fst n)}
    | U.Prod {nonterms=Nonterms_of (t, x); semact} ->
       let N.Prod {nonterms; semact=sa2} = prod prods (U.TermMap.find t U.Productions.(prods.%{x}.prods)) in
       N.Prod {nonterms; semact=fun t n -> semact t (sa2 t n)}
    | U.Prod {nonterms=Append (l, r); semact=sa} ->
       let N.Prod {nonterms=ntl; semact=sal} = prod prods (U.Prod {nonterms=l;semact=(fun _ n -> n)}) in
       let N.Prod {nonterms=ntr; semact=sar} = prod prods (U.Prod {nonterms=r;semact=(fun _ n -> n)}) in
       append ntl sal ntr sar sa
    | U.Prod_of (t, x) -> prod prods U.Productions.(U.TermMap.find t prods.%{x}.prods)
    | U.Map (f, p) -> let N.Prod {nonterms; semact} = prod prods p in
                      N.Prod {nonterms; semact = fun t n -> f (semact t n)}
  and append : 'a 'b 'c 'd 'e. 'a N.ntseq -> (string Code.t -> 'a Code.t -> 'b Code.t) ->
                               'c N.ntseq -> (string Code.t -> 'c Code.t -> 'd Code.t) ->
                               (string Code.t -> ('b * 'd) Code.t -> 'e Code.t) ->
                               'e N.prod =
    fun (type a b c d e) (ntl : a N.ntseq) (sal : string Code.t -> a Code.t -> b Code.t)
        (ntr : c N.ntseq) (sar : string Code.t -> c Code.t -> d Code.t)
        (sa : string Code.t -> (b * d) Code.t -> e Code.t) : e N.prod ->
    match ntl with
    | Empty -> N.Prod { nonterms = ntr;
                        semact = fun t n -> let l = sal t Code.unit in
                                            let r = sar t n in
                                            sa t (Code.pair l r) }
    | Cons (x, xs) ->
       let N.Prod { nonterms = xs'; semact = semact' } = append xs (fun _ n -> n) ntr sar (fun _ ns -> ns) in
       N.Prod { nonterms = Cons (x, xs');
                semact = fun t n ->
                         Code.let_ n @@ fun n ->
                         Code.let_ (semact' t (Code.snd n)) @@ fun s ->
                         sa t (Code.pair (sal t (Code.pair (Code.fst n) (Code.fst s))) (Code.snd s)) }
  and rhs : 'a. U.Productions.t -> 'a U.rhs -> 'a N.rhs =
    fun prods r -> { N.prods = U.TermMap.map (prod prods) r.U.prods; null = null prods r.U.null}
  and null : 'a. U.Productions.t -> 'a U.null -> 'a Code.t option =
    fun (type a) prods (n : a U.null) : a Code.t option ->
    match n with
    | U.Some v -> Some v
    | U.None -> None
    | U.Null_of x -> null prods U.Productions.(prods.%{x}.null)
    | U.MapNull (f, v) -> Option.map f (null prods v)
    | U.Either (x, y) -> match null prods x, null prods y with (* guaranteed not both null *)
                         | None, None -> None
                         | Some x, _ -> Some x
                         | _, Some y -> Some y

  $let grammar : 'a. 'a U.grammar -> 'a N.grammar =
    fun { U.start; items } ->
    { N.start;
      items = List.fold_right (fun p e -> match p with (U.Productions.E (x, r)) -> N.Productions.bind x (rhs items r) e
                                                     | (U.Productions.Alias (x, y)) -> N.Productions.alias x y e)
                (items :> U.Productions.entry list)
                N.Productions.empty }
  include U
  $let resolve : 'a. 'a U.grammar -> 'a N.grammar = fun g -> grammar g
end
