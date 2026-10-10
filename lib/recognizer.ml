(* A recognizer for fused grammars *)


module Make $(Term : Sigs.TERM) =
struct
  module N = Normal.Make$(Term)

  $let deriv_rhss : 'a. char -> (Reex.t * 'a) list -> (Reex.t * 'a) list =
    fun c l -> List.map (fun (r, k) -> Reex_deriv.deriv c r, k) l

  $let without_unmatchables : 'a. (Reex.t * 'a) list -> (Reex.t * 'a) list =
    fun rhss -> (* Remove rhss with ⊥ regexes *)
    List.filter (fun (r,_) -> not Reex.(equal r empty)) rhss

  $let first_nullable : 'a. (Reex.t * 'a) list -> (Reex.t * 'a) option =
    fun rs -> List.find_opt (fun (r,_) -> Reex_deriv.nullable r) rs

  $let explode : string -> char list =
    fun s -> List.init (String.length s) (String.get s)

  $let map_null : 'a 'b. 'a -> 'b option -> 'a option =
    fun s n -> Option.map (fun _ -> s) n

  module Lexer =
  struct
    module F = Fused.Make$(Term)
    type result = Term of Term.t | Skip
    type last = (result * char list) option

    $let last_of_rhs : char list -> Reex.t * F.lexer_rhs -> last =
      fun s k ->
      match k with
      | (_,F.Skip)   -> Some (Skip,s)
      | (_,Error _)  -> None
      | (_,Return t) -> Some (Term t,s)

    $let lex : (Reex.t * F.lexer_rhs) list -> string -> Term.t list option =
      fun cases s ->
      let rec l : (Reex.t * F.lexer_rhs) list -> last -> char list -> Term.t list =
        fun cases' k -> function
        | [] -> m k
        | c :: cs -> let cases'_c = without_unmatchables (deriv_rhss c cases') in
                     (* If all the derivatives are ⊥, we're done *)
                     if cases'_c = [] then m k
                     else (* At least one non-⊥ derivative.  We're going to consume c.
                             Update 'k' if we have an epsilon match, and continue lexing *)
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
    module P = Mcol.Mmap$(Term)

    $let rec parse : 'g 'p. 'g N.grammar -> 'p N.rhs -> Term.t list -> Term.t list option =
      fun (type g p) (g : g N.grammar) (rhs : p N.rhs) (ts : Term.t list) : Term.t list option ->
      match ts with
      | [] -> map_null [] rhs.N.null
      | t :: ts -> (match P.find_opt t rhs.N.prods with
                    | Some (N.Prod {nonterms;_}) -> parse_seq g ts nonterms
                    | None -> map_null (t :: ts) rhs.N.null)

    and parse_seq : 'g 'n. 'g N.grammar -> Term.t list -> 'n N.ntseq -> Term.t list option =
      fun (type g n) (g : g N.grammar) (cs : Term.t list) (nts : n N.ntseq) : Term.t list option ->
      match nts with
      | Empty -> Some cs
      | Cons (x, xs) -> match parse g N.Productions.(g.items.%{x}) cs with
                        | Some cs -> parse_seq g cs xs
                        | None -> None

    $let recognize : 'a. 'a N.grammar -> Term.t list -> bool =
      fun g s ->
      parse g N.Productions.(g.items.%{g.start}) s = Some []
  end

  module Fused =
  struct
    module F = Fused.Make$(Term)
    type 'a rhss = (Reex.t * 'a F.cont) list

    type last = Match : char list * _ N.ntseq -> last | Fail

    $let rec parse : 'g 'p. 'g F.grammar -> 'p rhss -> char list * last -> char list option =
      fun (type g p) (g : g F.grammar) (rhss : p rhss) (cl : char list * last) : char list option ->
      match cl with
      | [], Match (rest, nts) -> parse_seq g nts rest
      | [], Fail -> None
      | c :: cs, last -> let rhss'_c = without_unmatchables (deriv_rhss c rhss) in
                         (* Update 'last' if we have an epsilon match *)
                         let last' = match first_nullable rhss'_c with
                           | None                                    -> last
                           | Some (_, F.Success (N.Prod {nonterms;_})) -> Match (cs, nonterms)
                           | Some (_, F.Error _)                     -> Fail
                         in
                         if rhss'_c = [] then
                           match last with
                           (* If all the derivatives are ⊥, we're done *)
                           | Match (rest, nts) -> parse_seq g nts rest
                           | Fail              -> None
                         else (* At least one non-⊥ derivative.  Consume c and continue lexing. *)
                           parse g rhss'_c (cs, last')

    and parse_seq : 'g 'n. 'g F.grammar -> 'n N.ntseq -> char list -> char list option =
      (* check whether the nonterminal sequence N1...Nn matches cs *)
      fun (type g n) (g : g F.grammar) (nts : n N.ntseq) (cs : char list) : char list option ->
      match nts with
      | Empty -> Some cs
      | Cons (x, xs) -> let prods, eps = F.lookup g.F.items x in
                        match parse g prods (cs, if Option.is_some eps then Match (cs, Empty) else Fail) with
                        | Some cs -> parse_seq g xs cs
                        | None -> None

    $let recognize : 'a. 'a F.grammar -> string -> bool =
      fun g s ->
      let l = explode s in
      let prods, eps = F.lookup g.F.items g.F.start in
      parse g prods (l, if Option.is_some eps then Match (l, Empty) else Fail) = Some []
  end
end
