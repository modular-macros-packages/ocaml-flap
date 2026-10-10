

$let pr : 'a. Format.formatter -> ('a, Format.formatter, unit) format -> 'a =
  fun fmt f -> Format.fprintf fmt f

(** Greibach Normal Form grammars. *)


module Make $(Term : Sigs.TERM) =
struct
  module TermMap = Mcol.Mmap$(Term)

  type 'a ntseq = Empty : unit ntseq
                | Cons : 'a Vars.Var.t * 'b ntseq -> ('a * 'b) ntseq
  $let rec ntseq : 'a. Format.formatter -> 'a ntseq -> unit =
    fun (type a) fmt (s : a ntseq) ->
    match s with
    | Empty -> ()
    | Cons (x, xs) -> pr fmt "@[%s@ %a@]" (Vars.Var.str x) (fun fmt s -> ntseq fmt s) xs

  type 'l prod = Prod : { nonterms: 'n ntseq; semact: string Code.t -> 'n Code.t -> 'l Code.t } -> 'l prod
  $let pp_prod : 'a 'b. 'b -> Format.formatter -> 'a prod -> unit =
    fun _ fmt (Prod p) -> ntseq fmt p.nonterms
  $let show_prod : 'a 'b. 'b -> 'a prod -> string =
    fun _ p -> Format.asprintf "%a" (fun fmt p -> pp_prod () fmt p) p

  type 'a rhs = { prods: 'a prod TermMap.t; null: 'a Code.t option }
  $let pp_rhs : 'a 'b. 'b -> Format.formatter -> 'a rhs -> unit =
    fun _ fmt r ->
    begin
      TermMap.iter (fun t p -> pr fmt " |@ %a @[%a@]" Term.pp t (fun fmt p -> pp_prod () fmt p) p) r.prods;
      pr fmt "|@ ";
      if Option.is_some r.null then pr fmt "| ε";
    end
  $let show_rhs : 'a 'b. 'b -> 'a rhs -> string =
    fun _ r -> Format.asprintf "%a" (fun fmt r -> pp_rhs () fmt r) r

  module RhsV = struct type 'a t = 'a rhs end
  module Productions = Env.Env$(RhsV)
  $let productions : Format.formatter -> Productions.t -> unit =
    fun fmt p ->
    Productions.pp { Productions.m = fun fmt r -> pp_rhs () fmt r } fmt p

  type 'a grammar = { start: 'a Vars.Var.t; items: Productions.t; }
  $let pp_grammar : 'a 'b. 'b -> Format.formatter -> 'a grammar -> unit =
    fun _ fmt g -> pr fmt "<%s,@ @[%a@]>" (Vars.Var.str g.start) (fun fmt p -> productions fmt p) g.items
  $let show_grammar : 'a 'b. 'b -> 'a grammar -> string =
    fun _ g -> Format.asprintf "%a" (fun fmt g -> pp_grammar () fmt g) g

  type var = Var : _ Vars.Var.t -> var

  $let var_eql : var -> var -> bool =
    fun (Var x) (Var y) ->
    match Vars.Var.eql x y with
    | Some _ -> true
    | None -> false

  $let var_mem : var -> var list -> bool =
    fun x l -> List.exists (var_eql x) l

  $let trim : 'a. 'a grammar -> 'a grammar =
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
        Productions.(items.%{sym}).prods
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
        Productions.empty
        (items :> Productions.entry list)
    in
    { start; items = prods }
end
