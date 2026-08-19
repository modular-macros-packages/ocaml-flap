(*
 * Copyright (c) 2023 Neel Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)


module Sigs = Sigs

module Parse [Term : Sigs.TERM] = struct
  module V = Code
  module TermMap = Mcol.Mmap[Term]
  module TermSet = Mcol.Mset[Term]
  module ReexOrd = struct
    type t = Reex.t
    macro compare : t -> t -> int = fun x y -> Reex.compare x y
  end
  module ReexMap = Mcol.Mmap[ReexOrd]

  macro pr : 'a. Format.formatter -> ('a, Format.formatter, unit) format -> 'a =
    fun fmt f -> Format.fprintf fmt f

    module Gnf = struct
    type 'a ntseq = Empty : unit ntseq
                  | Cons : 'a Vars.Var.t * 'b ntseq -> ('a * 'b) ntseq
    macro rec pp_ntseq : 'a. Format.formatter -> 'a ntseq -> unit =
      fun (type a) fmt (s : a ntseq) ->
      match s with
      | Empty -> ()
      | Cons (x, xs) -> pr fmt "@[%s@ %a@]" (Vars.Var.str x) (fun fmt s -> pp_ntseq fmt s) xs

    type 'l prod = Prod : { nonterms: 'n ntseq; semact: string Code.t -> 'n Code.t -> 'l Code.t } -> 'l prod
    macro pp_prod : 'a 'b. 'b -> Format.formatter -> 'a prod -> unit =
      fun _ fmt (Prod p) -> pp_ntseq fmt p.nonterms

    type 'a rhs = { prods: 'a prod TermMap.t; null: 'a Code.t option }
    macro pp_rhs : 'a 'b. 'b -> Format.formatter -> 'a rhs -> unit =
      fun _ fmt r ->
      begin
        TermMap.iter (fun t p -> pr fmt " |@ %a @[%a@]" Term.pp t (fun fmt p -> pp_prod () fmt p) p) r.prods;
        pr fmt "|@ ";
        if Option.is_some r.null then pr fmt "| ε";
      end

    module RhsV = struct type 'a t = 'a rhs end
    module Productions = Env.Env[RhsV]
    macro pp_productions : Format.formatter -> Productions.t -> unit =
      fun fmt p ->
      Productions.pp { Productions.m = fun fmt r -> pp_rhs () fmt r } fmt p

    type 'a grammar = { start: 'a Vars.Var.t; items: Productions.t; }
    macro pp_grammar : 'a 'b. 'b -> Format.formatter -> 'a grammar -> unit =
      fun _ fmt g -> pr fmt "<%s,@ @[%a@]>" (Vars.Var.str g.start) (fun fmt p -> pp_productions fmt p) g.items
    macro show_grammar : 'a 'b. 'b -> 'a grammar -> string =
      fun _ g -> Format.asprintf "%a" (fun fmt g -> pp_grammar () fmt g) g

    type var = Var : _ Vars.Var.t -> var

    macro var_eql : var -> var -> bool =
      fun (Var x) (Var y) ->
      match Vars.Var.eql x y with
      | Some _ -> true
      | None -> false

    macro var_mem : var -> var list -> bool =
      fun x l -> List.exists (var_eql x) l

    macro trim : 'a. 'a grammar -> 'a grammar =
      fun { start; items } ->
      let rec add_nonterms : type a. a ntseq -> seen:var list -> var list -> var list  =
        fun nts ~seen todo ->
        match nts with
        | Empty -> todo
        | Cons (x, xs) when var_mem (Var x) seen -> add_nonterms xs ~seen todo
        | Cons (x, xs) when var_mem (Var x) todo -> add_nonterms xs ~seen todo
        | Cons (x, xs) -> add_nonterms xs ~seen (Var x :: todo)
      in
      let do_one (Var sym) ~seen todo =
        TermMap.fold
          (fun _ (Prod {nonterms;_}) todo -> add_nonterms nonterms ~seen todo)
          (Productions.find items sym).prods
          todo
      in
      let rec loop seen (todo : var list) =
        match todo with
        | [] -> seen
        | x :: xs -> let seen = x :: seen in
                     let todo = do_one x ~seen:(x :: seen) xs in
                     loop seen todo
      in
      let seen = loop [] [Var start] in
      let prods =
        List.fold_left
          (fun t -> function
            | Productions.E (x,rhs) -> if var_mem (Var x) seen
                                       then Productions.bind x rhs t
                                       else t
            | Alias _ -> t)
          (Productions.empty ())
          (items :> Productions.entry list)
      in
      { start; items = prods }
  end

    module Unresolved = struct
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
    type 'a rhs = { prods: 'a prod TermMap.t; null: 'a null;  }

    module RhsV = struct type 'a t = 'a rhs end
    module Productions = Env.Env[RhsV]

    type 'a grammar = {
      start: 'a Vars.Var.t;
      items: Productions.t;
    }
    type 'a t = 'a grammar

    macro rec pp_prod : 'a. Format.formatter -> 'a prod -> unit =
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

    macro rec pp_null : 'a. Format.formatter -> 'a null -> unit =
      fun (type a) fmt (n : a null) ->
      match n with
      | Some _ -> pr fmt "Some"
      | None   -> pr fmt "None"
      | Null_of x -> pr fmt "(Null_of %s)" (Vars.Var.str x)
      | MapNull (_, n) -> pp_null fmt n
      | Either (l, r) -> pr fmt "(Either %a %a)" (fun fmt n -> pp_null fmt n) l (fun fmt n -> pp_null fmt n) r

    macro pp_rhs : 'a. Format.formatter -> 'a rhs -> unit =
      fun fmt r ->
      begin
        TermMap.iter (fun t p -> pr fmt " |@ %a @[%a@]" Term.pp t (fun fmt p -> pp_prod fmt p) p) r.prods;
        pr fmt "|@ ";
        pp_null fmt r.null
      end

    macro pp_productions : Format.formatter -> Productions.t -> unit =
      fun fmt p ->
      Productions.pp { Productions.m = fun fmt r -> pp_rhs fmt r } fmt p

    macro pp_grammar : Format.formatter -> 'a grammar -> unit =
      fun fmt g -> pr fmt "<%s,@ @[%a@]>" (Vars.Var.str g.start) (fun fmt p -> pp_productions fmt p) g.items

    type 'a tok = Term.t * (string Code.t -> 'a Code.t)
    macro ($$) : 'a. Term.t -> (string Code.t -> 'a Code.t) -> 'a tok = fun t f -> (t, f)

    macro fresh : 'a. ('a Vars.Var.t -> Productions.t) -> 'a grammar =
      fun k -> let start = Vars.Var.var () in { start; items = k start }

    macro eps : 'a. 'a Code.t -> 'a grammar =
      fun v -> fresh @@ fun s -> Productions.bind s {prods=TermMap.empty (); null=Some v} (Productions.empty ())

    macro seqprod : 'a 'b. 'a Vars.Var.t -> Term.t -> 'b Vars.Var.t -> ('a * 'b) prod =
      fun a t s -> Prod { nonterms = Append (Nonterms_of (t, a), One s);
                          semact = (fun _ n -> n) }

    macro ( >>> ) : 'a 'b. 'a grammar -> 'b grammar -> ('a * 'b) grammar =
      fun g g' -> fresh @@ fun start ->
      let gstarts = Productions.find g.items g.start in
      Productions.bind start {prods=TermMap.mapi (fun t _ -> seqprod g.start t g'.start) gstarts.prods; null=None}
        (Productions.extend g.items g'.items)

    macro tok : 'a. 'a tok -> 'a grammar =
      fun (t,f) -> fresh @@ fun start ->
                Productions.bind start {prods=TermMap.singleton t (Prod {nonterms = Empty; semact = (fun c _ -> f c)});
                                        null=None} (Productions.empty ())

    macro bot : 'a. unit -> 'a grammar =
      fun () -> fresh @@ fun s -> Productions.bind s {prods=TermMap.empty (); null=None} (Productions.empty ())

    macro ( <|> ) : 'a. 'a grammar -> 'a grammar -> 'a grammar =
      fun g g' ->
      fresh @@ fun start -> 
      Productions.bind start
        {prods=TermMap.union (fun _ _ _ -> assert false)
                 (Productions.find g.items g.start).prods
                 (Productions.find g'.items g'.start).prods ;
         null = Either ((Productions.find g.items g.start).null,
                        (Productions.find g'.items g'.start).null) }
        (Productions.extend g.items g'.items)

    macro ( $. ) : 'a 'b. 'a grammar -> ('a Code.t -> 'b Code.t) -> 'b grammar =
      fun { start ; items } f -> fresh @@ fun start' ->
      let startprods = Productions.find items start in
      let prods = TermMap.map (fun p -> Map (f, p)) startprods.prods in
      let null = MapNull (f, startprods.null) in
      Productions.bind start' {prods; null} items

    macro fix : 'a. TermSet.t -> ('a grammar -> 'a grammar) -> 'a grammar =
      fun ts f ->
            let start = Vars.Var.var () in
      let prods = TermSet.fold (fun t m -> TermMap.add t (Prod_of (t, start)) m) ts (TermMap.empty ()) in
      let dummy = { start; items = Productions.bind start {prods; null = Null_of start} (Productions.empty ()) } in
      let g = f dummy in
      {g with items = Productions.alias start g.start g.items}
  end

    module Resolve = struct
    module U = Unresolved

    macro rec prod : 'l. U.Productions.t -> 'l U.prod -> 'l Gnf.prod =
      fun (type l) prods (p : l U.prod) : l Gnf.prod ->
      match p with
      | U.Prod {nonterms=Empty; semact} ->
         Gnf.Prod {nonterms=Empty; semact}
      | U.Prod {nonterms=One x; semact} ->
         Gnf.Prod {nonterms=Cons (U.Productions.canonicalize x prods, Empty); semact = fun t n -> semact t (Code.fst n)}
      | U.Prod {nonterms=Nonterms_of (t, x); semact} ->
         let Gnf.Prod {nonterms; semact=sa2} = prod prods (TermMap.find t (U.Productions.find prods x).U.prods) in
         Gnf.Prod {nonterms; semact=fun t n -> semact t (sa2 t n)}
      | U.Prod {nonterms=Append (l, r); semact=sa} ->
         let Gnf.Prod {nonterms=ntl; semact=sal} = prod prods (U.Prod {nonterms=l;semact=(fun _ n -> n)}) in
         let Gnf.Prod {nonterms=ntr; semact=sar} = prod prods (U.Prod {nonterms=r;semact=(fun _ n -> n)}) in
         append ntl sal ntr sar sa
      | U.Prod_of (t, x) -> prod prods (TermMap.find t (U.Productions.find prods x).U.prods)
      | U.Map (f, p) -> let Gnf.Prod {nonterms; semact} = prod prods p in
                        Gnf.Prod {nonterms; semact = fun t n -> f (semact t n)}
    and append : 'a 'b 'c 'd 'e. 'a Gnf.ntseq -> (string Code.t -> 'a Code.t -> 'b Code.t) ->
                                 'c Gnf.ntseq -> (string Code.t -> 'c Code.t -> 'd Code.t) ->
                                 (string Code.t -> ('b * 'd) Code.t -> 'e Code.t) ->
                                 'e Gnf.prod =
      fun (type a b c d e) (ntl : a Gnf.ntseq) (sal : string Code.t -> a Code.t -> b Code.t)
          (ntr : c Gnf.ntseq) (sar : string Code.t -> c Code.t -> d Code.t)
          (sa : string Code.t -> (b * d) Code.t -> e Code.t) : e Gnf.prod ->
      match ntl with
      | Empty -> Gnf.Prod { nonterms = ntr;
                            semact = fun t n -> let l = sal t (Code.unit ()) in
                                                let r = sar t n in
                                                sa t (Code.pair l r) }
      | Cons (x, xs) ->
         let Gnf.Prod { nonterms = xs'; semact = semact' } = append xs (fun _ n -> n) ntr sar (fun _ ns -> ns) in
         Gnf.Prod { nonterms = Cons (x, xs');
                    semact = fun t n ->
                             Code.let_ n @@ fun n ->
                             Code.let_ (semact' t (Code.snd n)) @@ fun s ->
                             sa t (Code.pair (sal t (Code.pair (Code.fst n) (Code.fst s))) (Code.snd s)) }
    and rhs : 'a. U.Productions.t -> 'a U.rhs -> 'a Gnf.rhs =
      fun prods r -> { Gnf.prods = TermMap.map (prod prods) r.U.prods; null = null prods r.U.null}
    and null : 'a. U.Productions.t -> 'a U.null -> 'a Code.t option =
      fun (type a) prods (n : a U.null) : a Code.t option ->
      match n with
      | U.Some v -> Some v
      | U.None -> None
      | U.Null_of x -> null prods (U.Productions.find prods x).U.null
      | U.MapNull (f, v) -> Option.map f (null prods v)
      | U.Either (x, y) -> match null prods x, null prods y with 
                           | None, None -> None
                           | Some x, _ -> Some x
                           | _, Some y -> Some y

    macro grammar : 'a. 'a U.grammar -> 'a Gnf.grammar =
      fun { U.start; items } ->
      { Gnf.start;
        items = List.fold_right (fun p e -> match p with (U.Productions.E (x, r)) -> Gnf.Productions.bind x (rhs items r) e
                                                       | (U.Productions.Alias (x, y)) -> Gnf.Productions.alias x y e)
                  (items :> U.Productions.entry list)
                  (Gnf.Productions.empty ()) }

    macro resolve : 'a. 'a U.grammar -> 'a Gnf.grammar = fun g -> grammar g
  end

    module Fused = struct
    type 'a cont = Success of 'a Gnf.prod
                 | Error of string
    module FRhsV = struct type 'a t = (Reex.t * 'a cont) list * 'a V.t option end
    module Productions = Env.Env[FRhsV]
    type productions = Productions.t
    type 'a grammar = { start: 'a Vars.Var.t; items: Productions.t; }

    macro lookup : 'a. Productions.t -> 'a Vars.Var.t -> (Reex.t * 'a cont) list * 'a V.t option =
      fun e x -> Productions.find e x

    macro pp_cont : 'a. Format.formatter -> 'a cont -> unit =
      fun fmt c ->
      match c with
      | Success p -> pr fmt "✓ %a" (fun fmt p -> Gnf.pp_prod (fun _ _ -> ()) fmt p) p
      | Error e -> pr fmt "X %S" e

    macro pp_frhs : 'a. Format.formatter -> (Reex.t * 'a cont) list * 'a V.t option -> unit =
      fun fmt (l, o) ->
      List.iter (fun (r, k) -> pr fmt "@ @[| \"%a\" @[%a@]@]" Reex.pp r (fun fmt k -> pp_cont fmt k) k) l;
      Option.iter (fun _ -> pr fmt "@ @[| EPS@]") o

    macro pp_productions : Format.formatter -> Productions.t -> unit =
      fun fmt p ->
      Productions.pp { Productions.m = fun fmt r -> pp_frhs fmt r } fmt p

    macro pp_grammar : 'a. Format.formatter -> 'a grammar -> unit =
      fun fmt g -> pr fmt "<@[%s,@ @[%a@]@]>" (Vars.Var.str g.start) (fun fmt p -> pp_productions fmt p) g.items

    type lexer_rhs = Skip | Error of string | Return of Term.t
    type inverted_lexer = { skip: Reex.t option;
                            errors: string ReexMap.t;
                            matches: Reex.t TermMap.t }

    macro invert_lexer : (Reex.t * lexer_rhs) list -> inverted_lexer =
      fun rules ->
      let add_rule ilexer (r, rhs) =
        match ilexer, rhs with
        | { skip = None; _ }   , Skip -> { ilexer with skip = Some r }
        | { skip = Some s; _ } , Skip -> { ilexer with skip = Some (Reex.(<|>) r s) }
        | { errors; _ }        , Error s -> { ilexer with errors = ReexMap.add r s errors }
        | { matches; _ }       , Return t ->
           let matches = match TermMap.find t matches with
           | exception Not_found -> TermMap.add t              r    matches
           | s                   -> TermMap.add t (Reex.(<|>) r s) matches
           in { ilexer with matches }
      in
      List.fold_left add_rule
        { skip = None; errors = ReexMap.empty (); matches = TermMap.empty () }
        rules

    macro frhs : 'a. inverted_lexer -> 'a Vars.Var.t -> 'a Gnf.rhs -> (Reex.t * 'a cont) list * 'a V.t option =
      fun { skip; errors; matches } x { Gnf.prods; null } ->
            let prods =
        TermMap.fold
          (fun t r rmap ->
            match TermMap.find_opt t prods with
            | Some p -> (r, Success p) :: rmap
            | None -> rmap)
          matches
          []
      in
      let prods =
        match skip with
        | None -> prods
        | Some s -> (s, Success (Gnf.Prod { nonterms = Cons (x, Empty);
                                            semact = fun _ n -> V.fst n })) :: prods
      in
      let prods =
        ReexMap.fold (fun r e l -> (r, (Error e : _ cont)) :: l) errors prods
      in
      prods, null

    macro fusei : 'a. inverted_lexer -> 'a Gnf.grammar -> 'a grammar =
      fun ilexer { Gnf.start; items } -> 
      { start; items = List.fold_right
                         (fun p e ->
                           match p with Gnf.Productions.E (x, r) -> Productions.bind x (frhs ilexer x r) e
                                      | Gnf.Productions.Alias (x, y) -> Productions.alias x y e)
                         (items :> Gnf.Productions.entry list)
                         (Productions.empty ()) }

    macro fuse : 'a. (Reex.t * lexer_rhs) list -> 'a Gnf.grammar -> 'a grammar =
      fun l g -> fusei (invert_lexer l) g
  end

    module Compiler = struct
    type ('a, 'b) fn = ('a expr -> index:int expr -> len:int expr -> string expr -> 'b expr)

    module Idx =
    struct
      type 'a payload = { nt: 'a Vars.Var.t;
                          prods: Fused.productions;
                          nexts: ((Reex.t * (int ref expr -> index:int expr -> len:int expr -> string expr -> 'a expr)) list);
                          fn: (int ref, 'a) fn; }
      type 'a t = T : 'a payload -> (int ref -> index:int -> prev:int -> len:int -> string -> 'a) t

      macro rec eql_prods : 'a 'b. (Reex.t * 'a) list -> (Reex.t * 'b) list -> bool =
        fun l r ->
        match l, r with
        | [], [] -> true
        | [], _ | _, [] -> false
        | (x,_) :: xs, (y,_) :: ys -> Reex.equal x y && eql_prods xs ys

      macro eql : 'a 'b. 'a t -> 'b t -> ('a, 'b) Letrec.eql option =
        fun (type a b) (T {nt=l; nexts=lrhs;_} : a t) (T {nt=r; nexts=rrhs;_} : b t) : (a, b) Letrec.eql option ->
        match Vars.Var.eql l r with
        | Some Vars.Refl when eql_prods lrhs rrhs  -> Some Letrec.Refl
        | _ -> None
    end

    type resolver = { resolve : 'a. 'a Idx.t -> 'a expr }

    macro eps_fallback : 'a 'b. 'b Code.t option -> ('a, 'b) fn =
      fun c ->
      match c with
      | None -> (fun _ ~index:_  ~len:_ _ -> << failwith "unexpected eof" >>)
      | Some c -> (fun _ ~index:_  ~len:_ _ -> Code.dyn c)

        macro make_cases_exclusive : 'a. (Reex.t * 'a) list -> (Reex.t * 'a) list =
      fun cases ->
      (List.rev @@ Stdlib.fst @@ List.fold_left
        (fun (cases, seen) (lhs, rhs) ->
          (let lhs' = Reex.(<&>) lhs (Reex.not seen) in ((lhs', rhs) :: cases, Reex.(<|>) seen lhs')))
        ([], Reex.empty)
        cases)

    macro rec ntseq : 'a. resolver -> Fused.productions -> int ref expr -> len:int expr -> string expr -> 'a Gnf.ntseq -> 'a Code.t =
      fun (type a) resolve ps start ~len s (seq : a Gnf.ntseq) : a Code.t ->
      match seq with
      | Empty -> Code.unit ()
      | Cons (x, xs) -> let cases, eps = Fused.lookup ps x in
                        let cases' = List.map (fun (r, k) -> (r, unCont resolve ps k)) cases in
                        Code.let_ (Code.inj << $(resolve.resolve (Idx.T {nt=x; prods=ps; nexts=cases';
                                                                         fn = eps_fallback eps}))
                                                 $start ~prev:0 ~len:$len ~index:(! $start) $s >>) @@ fun x' ->
                        Code.let_ (ntseq resolve ps start ~len s xs) @@ fun xs' ->
                        Code.pair x' xs'

    and prod : 'a. resolver -> Fused.productions -> 'a Gnf.prod -> int ref expr -> len:int expr -> string expr -> string expr -> 'a Code.t
      = fun r ps (Gnf.Prod {nonterms; semact}) start ~len s tok ->
      Code.let_ (ntseq r ps start ~len s nonterms) @@ fun x ->
      semact (Code.injv tok) x

    and unCont : 'a. resolver -> Fused.productions -> 'a Fused.cont -> int ref expr -> index:int expr -> len:int expr -> string expr -> 'a expr =
      fun resolve ps k start ~index:i ~len s ->
      match k with
      | Fused.Success p -> << let _start = ! $start in let _ = $start := $i in
                      $(Code.dyn (prod resolve ps p start ~len s << String.sub $s _start ($i - _start) >>)) >>
      | Fused.Error e -> << Printf.ksprintf failwith "Error (position %d): %s" $i $(Expr.string e) >>

    macro step : 'a. resolver -> 'a Idx.t -> 'a expr =
      fun (type a) resolve (Idx.T {nt; prods; nexts=rhs0; fn} : a Idx.t) : a expr ->
      Reex_match.matchk_ ~options:{Reex_match.match_type=`ranges; null_optimization=true}
        (fun (nexts, fn) -> resolve.resolve (Idx.T {nt; prods; nexts; fn})) (rhs0, fn)

    macro compile : 'a. (Reex.t * Fused.lexer_rhs) list -> 'a Gnf.grammar -> (string -> 'a) expr =
      fun cases g ->
      let {Fused.start; items} = Fused.fuse (make_cases_exclusive cases) g in
      Letrec.letrec (module Idx) ~eql:(fun l r -> Idx.eql l r)
        (fun resolve idx -> step {resolve} idx)
        (fun resolve ->
          << fun s -> let start' = ref 0 and len = String.length s in
                      $(let rs, eps = Fused.lookup items start in
                        {resolve}.resolve (Idx.T {nt=start; prods=items; fn = eps_fallback eps;
                                                  nexts=List.map (fun (r, k) -> (r, unCont {resolve} items k)) rs }))
                           start' ~index:0 ~prev:0 ~len s >>)
  end

    module Recognizer = struct
    macro deriv_rhss : 'a. char -> (Reex.t * 'a) list -> (Reex.t * 'a) list =
      fun c l -> List.map (fun (r, k) -> Reex_deriv.deriv c r, k) l

    macro without_unmatchables : 'a. (Reex.t * 'a) list -> (Reex.t * 'a) list =
      fun rhss -> 
      List.filter (fun (r,_) -> not (Reex.equal r Reex.empty)) rhss

    macro first_nullable : 'a. (Reex.t * 'a) list -> (Reex.t * 'a) option =
      fun rs -> List.find_opt (fun (r,_) -> Reex_deriv.nullable r) rs

    macro explode : string -> char list =
      fun s -> List.init (String.length s) (String.get s)

    macro map_null : 'a 'b. 'a -> 'b option -> 'a option =
      fun s n -> Option.map (fun _ -> s) n

    module Lexer =
    struct
      type result = Term of Term.t | Skip
      type last = (result * char list) option

      macro last_of_rhs : char list -> Reex.t * Fused.lexer_rhs -> last =
        fun s k ->
        match k with
        | (_,Fused.Skip)   -> Some (Skip,s)
        | (_,Error _)  -> None
        | (_,Return t) -> Some (Term t,s)

      macro lex : (Reex.t * Fused.lexer_rhs) list -> string -> Term.t list option =
        fun cases s ->
        let rec l : (Reex.t * Fused.lexer_rhs) list -> last -> char list -> Term.t list =
          fun cases' k -> function
          | [] -> m k
          | c :: cs -> let cases'_c = without_unmatchables (deriv_rhss c cases') in
                                              if cases'_c = [] then m k
                       else 
                         match first_nullable cases'_c with
                            | None -> l cases'_c k cs
                            | Some k' -> l cases'_c (last_of_rhs cs k') cs

        and m = function
          | None                           -> failwith "lex"
          | Some (Skip,[])                 -> []
          | Some (Skip,(_ :: _ as rest))   -> l cases None rest
          | Some (Term t,[])               -> [t]
          | Some (Term t,(_ :: _ as rest)) -> t :: l cases None rest
        in
        match l cases None (explode s) with
        | exception Failure _ -> None
        | v -> Some v
    end

    module Normalized =
    struct
      macro rec parse : 'g 'p. 'g Gnf.grammar -> 'p Gnf.rhs -> Term.t list -> Term.t list option =
        fun (type g p) (g : g Gnf.grammar) (rhs : p Gnf.rhs) (ts : Term.t list) : Term.t list option ->
        match ts with
        | [] -> map_null [] rhs.null
        | t :: ts -> (match TermMap.find_opt t rhs.prods with
                      | Some (Prod {nonterms;_}) -> parse_seq g ts nonterms
                      | None -> map_null (t :: ts) rhs.null)

      and parse_seq : 'g 'n. 'g Gnf.grammar -> Term.t list -> 'n Gnf.ntseq -> Term.t list option =
        fun (type g n) (g : g Gnf.grammar) (cs : Term.t list) (nts : n Gnf.ntseq) : Term.t list option ->
        match nts with
        | Empty -> Some cs
        | Cons (x, xs) -> match parse g (Gnf.Productions.find g.items x) cs with
                          | Some cs -> parse_seq g cs xs
                          | None -> None

      macro recognize : 'a. 'a Gnf.grammar -> Term.t list -> bool =
        fun g s ->
        parse g (Gnf.Productions.find g.items g.start) s = Some []
    end

    module FusedR =
    struct
      type 'a rhss = (Reex.t * 'a Fused.cont) list

      type last = Match : char list * _ Gnf.ntseq -> last | Fail

      macro rec parse : 'g 'p. 'g Fused.grammar -> 'p rhss -> char list * last -> char list option =
        fun (type g p) (g : g Fused.grammar) (rhss : p rhss) (cl : char list * last) : char list option ->
        match cl with
        | [], Match (rest, nts) -> parse_seq g nts rest
        | [], Fail -> None
        | c :: cs, last -> let rhss'_c = without_unmatchables (deriv_rhss c rhss) in
                                                      let last' = match first_nullable rhss'_c with
                             | None                                    -> last
                             | Some (_, Fused.Success (Gnf.Prod {nonterms;_})) -> Match (cs, nonterms)
                             | Some (_, Fused.Error _)                 -> Fail
                           in
                           if rhss'_c = [] then
                             match last with
                                                          | Match (rest, nts) -> parse_seq g nts rest
                             | Fail              -> None
                           else 
                             parse g rhss'_c (cs, last')

      and parse_seq : 'g 'n. 'g Fused.grammar -> 'n Gnf.ntseq -> char list -> char list option =
                fun (type g n) (g : g Fused.grammar) (nts : n Gnf.ntseq) (cs : char list) : char list option ->
        match nts with
        | Empty -> Some cs
        | Cons (x, xs) -> let prods, eps = Fused.lookup g.items x in
                          match parse g prods (cs, if Option.is_some eps then Match (cs, Empty) else Fail) with
                          | Some cs -> parse_seq g xs cs
                          | None -> None

      macro recognize : 'a. 'a Fused.grammar -> string -> bool =
        fun g s ->
        let l = explode s in
        let prods, eps = Fused.lookup g.items g.start in
        parse g prods (l, if Option.is_some eps then Match (l, Empty) else Fail) = Some []
    end
  end

  
  module C = struct
    macro pp : Format.formatter -> TermSet.t -> unit =
      fun fmt cs -> Format.fprintf fmt "{ ";
                    TermSet.iter (fun t -> Format.fprintf fmt "%a, " Term.pp t) cs;
                    Format.fprintf fmt "}"
  end

  module Tp = struct
    type t = { first : TermSet.t; follow : TermSet.t; null : bool; guarded : bool }

    macro pp : Format.formatter -> t -> unit =
      fun fmt t ->
      Format.fprintf fmt "{ first = %a; follow = %a; null = %b; guarded = %b }"
        (fun fmt s -> C.pp fmt s) t.first (fun fmt s -> C.pp fmt s) t.follow t.null t.guarded

    macro equal : t -> t -> bool =
      fun t1 t2 ->
         (t1.null = t2.null)
      && (t1.guarded = t2.guarded)
      && TermSet.equal t1.first t2.first
      && TermSet.equal t1.follow t2.follow

    macro nonoverlapping : TermSet.t -> TermSet.t -> bool =
      fun cs cs' -> TermSet.is_empty (TermSet.inter cs cs')
    macro (++) : TermSet.t -> TermSet.t -> TermSet.t = fun x y -> TermSet.union x y
    macro (==>) : bool -> TermSet.t -> TermSet.t = fun b cs -> if b then cs else TermSet.empty ()

    macro apart : t -> t -> bool =
      fun t1 t2 -> not t1.null && (nonoverlapping t1.follow t2.first)
    macro disjoint : t -> t -> bool =
      fun t1 t2 -> not (t1.null && t2.null) && nonoverlapping t1.first t2.first

    macro eps : unit -> t = fun () ->
      { first = TermSet.empty (); follow = TermSet.empty (); null = true; guarded = true }
    macro tok : Term.t -> t = fun c ->
      { first = TermSet.singleton c; follow = TermSet.empty (); null = false; guarded = true }
    macro seq : t -> t -> t = fun t1 t2 ->
      if apart t1 t2 then {
          first = t1.first;
          follow = (++) t2.follow ((==>) t2.null ((++) t2.first t1.follow));
          null = false;
          guarded = t1.guarded
        } else Format.kasprintf failwith "seq %a %a" (fun fmt t -> pp fmt t) t1 (fun fmt t -> pp fmt t) t2
    macro bot : unit -> t = fun () ->
      { first = TermSet.empty (); follow = TermSet.empty (); null = false; guarded = true }
    macro alt : t -> t -> t = fun t1 t2 ->
      if disjoint t1 t2 then {
          first = (++) t1.first t2.first;
          follow = (++) t1.follow t2.follow;
          null = t1.null || t2.null;
          guarded = t1.guarded && t2.guarded;
        } else Format.kasprintf failwith "alt %a %a" (fun fmt t -> pp fmt t) t1 (fun fmt t -> pp fmt t) t2

    macro star : t -> t = fun t ->
      {(seq t t) with null = true; follow = (++) t.first t.follow}

    macro min : unit -> t = fun () ->
      { first = TermSet.empty (); follow = TermSet.empty (); null = false; guarded = false }

    macro fix : (t -> t) -> t = fun f ->
      let rec loop tp =
        let tp' = f tp in
        if equal tp tp' then
          tp
        else
          loop tp'
      in
      loop (min ())
  end

  module Var = struct
    type ('ctx, 'a) t =
      | Z : ('a * 'rest, 'a) t
      | S : ('rest, 'a) t -> ('b * 'rest, 'a) t
  end

  module LEnv [T : sig type 'a t end] = struct
    type 'ctx t =
      | [] : unit t
      | (::) : 'a T.t * 'ctx t -> ('a * 'ctx) t

    macro rec lookup : 'ctx 'a. 'ctx t -> ('ctx, 'a) Var.t -> 'a T.t =
      fun (type ctx a) (env : ctx t) (n : (ctx, a) Var.t) : a T.t ->
      match n, env with
      | Z, x :: _   -> x
      | S n, _ :: xs -> lookup xs n

    type fn = {f : 'a. 'a T.t -> 'a T.t}
    macro rec map : 'ctx. fn -> 'ctx t -> 'ctx t =
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
    module TpEnv = LEnv[TpV]

    macro data : 'ctx 'a 'd. ('ctx, 'a, 'd) t -> 'd = fun (d, _) -> d

    macro rec typeof : 'ctx 'a 'd. 'ctx TpEnv.t -> ('ctx, 'a, 'd) t -> ('ctx, 'a, Tp.t) t  =
      fun (type ctx a d) (env : ctx TpEnv.t) ((_, g) : (ctx, a, d) t) : (ctx, a, Tp.t) t ->
      match g with
      | Eps v -> (Tp.eps (), Eps v)
      | Seq (g1, g2) -> let guarded tp = {tp with Tp.guarded = true} in
                        let g1 = typeof env g1 in
                        let g2 = typeof (TpEnv.map {TpEnv.f = guarded} env) g2 in
                        (Tp.seq (data g1) (data g2), Seq(g1, g2))
      | Tok (tag,f) -> (Tp.tok tag, Tok (tag,f))
      | Bot  -> (Tp.bot (), Bot)
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
    module Ctx = LEnv[UnitV]

    macro rec len : 'n. 'n Ctx.t -> int =
      fun (type n) (ctx : n Ctx.t) : int ->
      match ctx with
      | [] -> 0
      | () :: ctx -> 1 + len ctx

    macro rec tshift' : 'a 'i 'j. int -> 'j Ctx.t -> ('a * 'i) Ctx.t -> ('j, 'a) Var.t =
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

    macro tshift : 'a 'i 'j. 'j Ctx.t -> ('a * 'i) Ctx.t -> ('j, 'a) Var.t =
      fun c1 c2 -> tshift' (len c1 - len c2) c1 c2

    type 'a t = { untdb : 'ctx. 'ctx Ctx.t -> ('ctx, 'a, unit) Grammar.t }

    macro eps : 'a. 'a V.t -> 'a t = fun a -> { untdb = fun _ -> (), Eps a }
    macro (>>>) : 'a 'b. 'a t -> 'b t -> ('a * 'b) t =
      fun f g -> { untdb = fun i -> (), Seq (f.untdb i, g.untdb i) }
    macro tok : 'a. Term.t -> (string V.t -> 'a V.t) -> 'a t =
      fun t f -> { untdb = fun _ -> (), Tok (t, f) }
    macro bot : 'a. unit -> 'a t = fun () -> { untdb = fun _ -> (), Bot }
    macro (<|>) : 'a. 'a t -> 'a t -> 'a t =
      fun f g -> { untdb = fun i -> (), Alt (f.untdb i, g.untdb i) }
    macro any : 'a. 'a t list -> 'a t = fun gs -> List.fold_left (fun x y -> (<|>) x y) (bot ()) gs
    macro ($.) : 'a 'b. 'a t -> ('a V.t -> 'b V.t) -> 'b t =
      fun g f -> { untdb = fun i -> (), Map (f, g.untdb i) }
    macro fix : 'b. ('b t -> 'b t) -> 'b t =
      fun f ->
      { untdb = fun i ->
                (), Fix ((f {untdb = fun j -> (), Var (tshift j (() :: i))}).untdb
                       (() :: i)) }
    macro star : 'a. 'a t -> 'a list t = fun g -> { untdb = fun i -> (), Star (g.untdb i) }
  end
  include HOAS

  module Norm =
  struct
    module GV = struct type 'a t = 'a Unresolved.grammar end
    module Ctx = LEnv[GV]

    macro rec norm : 'a 'ctx. 'ctx Ctx.t -> ('ctx, 'a, Tp.t) Grammar.t -> 'a Unresolved.t =
      fun (type a ctx) (ctx : ctx Ctx.t) (g : (ctx, a, Tp.t) Grammar.t) : a Unresolved.t ->
      match g with
      | _, Eps v -> Unresolved.eps v
      | _, Seq (g, h) -> Unresolved.(>>>) (norm ctx g) (norm ctx h)
      | _, Tok (t, f) -> Unresolved.tok (Unresolved.($$) t f)
      | _, Bot -> Unresolved.bot ()
      | _, Alt (g, h) -> Unresolved.(<|>) (norm ctx g) (norm ctx h)
      | _, Map (f, g) -> Unresolved.($.) (norm ctx g) f
      | t, Fix body -> Unresolved.fix t.Tp.first (fun h -> norm (h :: ctx) body)
      | _, Var x -> Ctx.lookup ctx x
      | _, Star _ -> failwith "star normalization not yet implemented"
  end

  type rhs = Fused.lexer_rhs = Skip | Error of string | Return of Term.t
  macro type_check : 'a. 'a t -> (unit, 'a, Tp.t) Grammar.t =
    fun {untdb} -> Grammar.typeof [] (untdb [])
  macro compile : 'a. (Reex.t * rhs) list -> 'a t -> ((string -> 'a) expr, string) result =
    fun l p ->
    match type_check p with
    | p -> Ok (Compiler.compile l (Resolve.resolve (Norm.norm [] p)))
    | exception (Failure e) -> Error e
end
module Cd = Code
