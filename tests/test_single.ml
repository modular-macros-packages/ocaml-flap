
module T = struct
  type t = Sexp_tokens.t = LPAREN | RPAREN | SYMBOL
  $gen compare : t -> t -> int = fun x y -> Sexp_tokens.compare x y
  $gen pp : Format.formatter -> t -> unit = fun fmt x -> Sexp_tokens.pp fmt x
end
module P = Flap.Parse[T]
module C = Flap.Cd

$gen cases : unit -> (Reex.t * P.rhs) list = fun () ->
  [ Reex.chr '(', P.Return T.LPAREN;
    Reex.(>>>) (Reex.range 'a' 'z') (Reex.star (Reex.range 'a' 'z')), P.Return T.SYMBOL;
    Reex.chr ')', P.Return T.RPAREN;
    Reex.chr ' ', P.Skip ]

$gen tok' : T.t -> string P.t = fun c -> P.tok c (fun c -> c)

$gen star' : 'a. 'a P.t -> 'a list P.t = fun x ->
  P.fix (fun starx ->
    P.(<|>) (P.eps (C.injv << [] >>))
      (P.($.) (P.(>>>) x starx)
        (fun p -> C.let_ p (fun p -> C.injv << $(C.dyn (C.fst p)) :: $(C.dyn (C.snd p)) >>))))

$gen sexp : unit -> ([`Atom | `Sexp of 'sexp list] as 'sexp) P.t = fun () ->
  P.fix (fun sexp ->
    P.(<|>)
      (P.($.) (tok' T.SYMBOL) (fun _ -> C.injv << `Atom >>))
      (P.($.) (P.(>>>) (P.(>>>) (tok' T.LPAREN) (star' sexp)) (tok' T.RPAREN))
        (fun p -> C.let_ p (fun p -> C.injv << `Sexp $(C.dyn (C.snd (C.fst p))) >>))))

$gen compiled : unit -> (string -> ([`Atom | `Sexp of 'sexp list] as 'sexp)) expr = fun () ->
  match P.compile (cases ()) (sexp ()) with
  | Ok c -> c
  | Error e -> failwith e

let parse = $( compiled () )

let () =
  assert (parse "abc-def" = `Atom);
  assert (parse "()" = `Sexp []);
  assert (parse "( aaa )" = `Sexp [`Atom]);
  assert (parse "  (a(aa())  a)" = `Sexp [`Atom; `Sexp [`Atom; `Sexp []]; `Atom]);
  print_endline "flap sexp acceptance: all ok"
