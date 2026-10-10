(*
 * Copyright (c) 2021 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Compilation of fused deterministic GNF *)

module Make $(Term : Sigs.TERM) =
struct
  module Gnf = Normal.Make$(Term)
  module Fused = Fused.Make$(Term)
  module Normalize = Normalize.Make$(Term)
  type ('a, 'b) fn = ('a expr -> index:int expr -> len:int expr -> string expr -> 'b expr)

  module Idx =
  struct
    type 'a payload = { nt: 'a Vars.Var.t;
                        prods: Fused.productions;
                        nexts: ((Reex.t * (int ref expr -> index:int expr -> len:int expr -> string expr -> 'a expr)) list);
                        fn: (int ref, 'a) fn; }
    type 'a t = T : 'a payload -> (int ref -> index:int -> prev:int -> len:int -> string -> 'a) t

    $let rec eql_prods : 'a 'b. (Reex.t * 'a) list -> (Reex.t * 'b) list -> bool =
      fun l r ->
      match l, r with
      | [], [] -> true
      | [], _ | _, [] -> false
      | (x,_) :: xs, (y,_) :: ys -> Reex.equal x y && eql_prods xs ys

    $let eql : 'a 'b. 'a t -> 'b t -> ('a, 'b) Type.eq option =
      fun (type a b) (T {nt=l; nexts=lrhs;_} : a t) (T {nt=r; nexts=rrhs;_} : b t) : (a, b) Type.eq option ->
      match Vars.Var.eql l r with
      | Some Type.Equal when eql_prods lrhs rrhs (* && lf == rf *) -> Some Type.Equal
      | _ -> None
  end

  type resolver = { resolve : 'a. 'a Idx.t -> 'a expr }

  $let eps_fallback : 'a 'b. 'b Code.t option -> ('a, 'b) fn =
    fun c ->
    match c with
    | None -> (fun _ ~index:_  ~len:_ _ -> << failwith "unexpected eof" >>)
    | Some c -> (fun _ ~index:_  ~len:_ _ -> Code.dyn c)

  (* The general algorithm is as follows:

       we maintain a mapping M: regex ↦ continuation
       we generate a function f_NT_M for each (NT, M) pair that behaves as follows:
          X := [(c, δc(M)) | c ∈ Σ]
          rnull = r ∈ M such that ε ∈ r
          match on s(i):
           c ↦  if all δc(M) are ⊥ then if rnull continue with M[rnull]
                                         else fail
                 else continue with f_NT_δc(M)
   *)
  $let make_cases_exclusive : 'a. (Reex.t * 'a) list -> (Reex.t * 'a) list =
    fun cases ->
    (List.rev @@ Stdlib.fst @@ List.fold_left
      (fun (cases, seen) (lhs, rhs) ->
        Reex.(let lhs' = lhs <&> not seen in ((lhs', rhs) :: cases, seen <|> lhs')))
      ([], Reex.empty)
      cases)

  $let rec ntseq : 'a. resolver -> Fused.productions -> int ref expr -> len:int expr -> string expr -> 'a Gnf.ntseq -> 'a Code.t =
    fun (type a) resolve ps start ~len s (seq : a Gnf.ntseq) : a Code.t ->
    match seq with
    | Empty -> Code.unit
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

  $let step : 'a. resolver -> 'a Idx.t -> 'a expr =
    fun (type a) resolve (Idx.T {nt; prods; nexts=rhs0; fn} : a Idx.t) : a expr ->
    Reex_match.matchk_ ~options:{Reex_match.match_type=`ranges; null_optimization=true}
      (fun (nexts, fn) -> resolve.resolve (Idx.T {nt; prods; nexts; fn})) (rhs0, fn)

  $let compile : 'a. (Reex.t * Fused.lexer_rhs) list -> 'a Normalize.t -> (string -> 'a) expr =
    fun cases g ->
    let {Fused.start; items} = Fused.fuse (make_cases_exclusive cases) (Normalize.resolve g) in
    Letrec.letrec (module Idx)
      (fun resolve idx -> step {resolve} idx)
      (fun resolve ->
        << fun s -> let start' = ref 0 and len = String.length s in
                    $(let rs, eps = Fused.lookup items start in
                      {resolve}.resolve (Idx.T {nt=start; prods=items; fn = eps_fallback eps;
                                                nexts=List.map (fun (r, k) -> (r, unCont {resolve} items k)) rs }))
                         start' ~index:0 ~prev:0 ~len s >>)
end
