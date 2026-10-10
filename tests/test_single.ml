
module T = struct
  type t = Sexp_tokens.t = LPAREN | RPAREN | SYMBOL
  $let compare : t -> t -> int = fun x y -> Sexp_tokens.compare x y
  $let pp : Format.formatter -> t -> unit = fun fmt x -> Sexp_tokens.pp fmt x
end
module P = Flap.Parse$(T)
module C = Flap.Cd

$let cases : (Reex.t * P.rhs) list =
   P.(Reex.[chr '(',                                Return LPAREN;
            range 'a' 'z' >>> star (range 'a' 'z'), Return SYMBOL;
            chr ')',                                Return RPAREN;
            chr ' ',                                Skip])

$let (inj, injv, dyn) = C.(inj, injv, dyn)
open P

$let tok : T.t -> string P.t = fun c -> P.tok c @@ fun c -> c

$let star : 'a. 'a P.t -> 'a list P.t = fun x ->
  fix @@ fun starx ->
  (eps (injv << [] >>))
   <|>
  (x >>> starx $ fun p -> C.let_ p @@ fun p -> injv << $(C.(dyn (fst p))) :: $(C.(dyn (snd p))) >>)

$let sexp : ([`Atom | `Sexp of 'sexp list] as 'sexp) t =
  fix @@ fun sexp ->
    (tok T.SYMBOL $ fun _ -> injv << `Atom >>)
<|> (tok T.LPAREN >>> star sexp >>> tok T.RPAREN $ fun p -> C.let_ p @@ fun p -> injv << `Sexp $(C.(dyn (snd (fst p)))) >>)

$let compiled : unit -> (string -> ([`Atom | `Sexp of 'sexp list] as 'sexp)) expr = fun () ->
  match P.compile cases sexp with
  | Ok c -> c
  | Error e -> failwith e

let parse = $( compiled () )

let () =
  assert (parse "abc-def" = `Atom);
  assert (parse "()" = `Sexp []);
  assert (parse "( aaa )" = `Sexp [`Atom]);
  assert (parse "  (a(aa())  a)" = `Sexp [`Atom; `Sexp [`Atom; `Sexp []]; `Atom]);
  print_endline "flap sexp acceptance: all ok"
