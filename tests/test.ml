

open OUnit2

module Single_rec =
struct
  module C = Flap.Cd
  module T = struct
    type t = Sexp_tokens.t = LPAREN | RPAREN | SYMBOL
    $let compare : t -> t -> int = fun x y -> Sexp_tokens.compare x y
    $let pp : Format.formatter -> t -> unit = fun fmt x -> Sexp_tokens.pp fmt x
  end
  module P = Flap.Parse$(T)

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

  $let compiled' : unit -> (string -> ([`Atom | `Sexp of 'sexp list] as 'sexp)) expr = fun () ->
    match P.compile cases sexp with Ok c -> c | Error e -> failwith e

  let parser = $( compiled' () )
end


module Backtracking =
struct
  module C = Flap.Cd
  module T = struct
    type t = Bt_tokens.t = AA | AAAB | EOF
    $let compare : t -> t -> int = fun x y -> Bt_tokens.compare x y
    $let pp : Format.formatter -> t -> unit = fun fmt x -> Bt_tokens.pp fmt x
  end
  module P = Flap.Parse$(T)

  $let cases : (Reex.t * P.rhs) list =
    P.(Reex.[str "aa",   Return AA;
             str "aaab", Return AAAB;
             str "",     Return EOF])

  $let (inj, injv, dyn) = C.(inj, injv, dyn)
  open P

  $let tok : T.t -> string P.t = fun c -> P.tok c @@ fun c -> c

  $let star : 'a. 'a P.t -> 'a list P.t = fun x ->
    fix @@ fun starx ->
    (eps (injv << [] >>))
     <|>
    (x >>> starx $ fun p -> C.let_ p @@ fun p -> injv << $(C.(dyn (fst p))) :: $(C.(dyn (snd p))) >>)

  $let aa : [`AA | `AAAB] P.t = tok T.AA $ fun _ -> injv << `AA >>
  $let aaab : [`AA | `AAAB] P.t = tok T.AAAB $ fun _ -> injv << `AAAB >>

  $let grammar : [ `AA | `AAAB ] list t = star (aa <|> aaab)

  $let compiled' : unit -> (string -> [`AA | `AAAB] list) expr = fun () ->
    match P.compile cases grammar with Ok c -> c | Error e -> failwith e

  let parser = $( compiled' () )
end


module Sexp_recognizer =
struct
  module C = Flap.Cd
  module T = Single_rec.T
  module S = Flap__.Mcol.Mset$(T)
  module R = Flap__.Recognizer.Make$(T)
  module F = Flap__.Fused.Make$(T)
  module N = Flap__.Normalize.Make$(T)
  open N

  $let cases : (Reex.t * F.lexer_rhs) list =
    Reex.[chr '(',                                F.Return LPAREN;
          range 'a' 'z' >>> star (range 'a' 'z'), F.Return SYMBOL;
          chr ')',                                F.Return RPAREN;
          chr ' ',                                F.Skip]

  $let (inj, injv, dyn) = C.(inj, injv, dyn)

  $let tok : T.t -> string N.t = fun c -> N.tok (c $$ fun c -> c)

  $let startset : S.t = S.of_list T.[LPAREN; SYMBOL]

  $let star : 'a. 'a N.t -> 'a list N.t = fun x ->
    fix startset @@ fun starx ->
    (eps (injv << [] >>))
     <|>
    (x >>> starx $ fun p -> C.let_ p @@ fun p -> injv << $(C.(dyn (fst p))) :: $(C.(dyn (snd p))) >>)

  $let sexp : unit -> ([`Atom | `Sexp of 'sexp list] as 'sexp) t = fun () ->
    fix startset @@ fun sexp ->
      (tok T.SYMBOL $ fun _ -> injv << `Atom >>)
  <|> (tok T.LPAREN >>> star sexp >>> tok T.RPAREN $ fun p -> C.let_ p @@ fun p -> injv << `Sexp $(C.(dyn (snd (fst p)))) >>)

  $let fused : unit -> ([`Atom | `Sexp of 'sexp list] as 'sexp) F.grammar = fun () ->
    F.fuse cases (N.resolve (sexp ()))

  $let unfused : string -> bool = fun s ->
    match R.Lexer.lex cases s with
    | None -> false
    | Some ts -> R.Normalized.recognize (N.resolve (sexp ())) ts
end


module Tok_recognizer =
struct
  module C = Flap.Cd
  module T = struct
    type t = Tok_tokens.t = AA | AAAB
    $let compare : t -> t -> int = fun x y -> Tok_tokens.compare x y
    $let pp : Format.formatter -> t -> unit = fun fmt x -> Tok_tokens.pp fmt x
  end
  module S = Flap__.Mcol.Mset$(T)
  module R = Flap__.Recognizer.Make$(T)
  module F = Flap__.Fused.Make$(T)
  module N = Flap__.Normalize.Make$(T)
  open N

  $let cases : (Reex.t * F.lexer_rhs) list =
    Reex.[str "aa", F.Return AA;
          str "aaab", F.Return AAAB]

  $let (inj, injv, dyn) = C.(inj, injv, dyn)

  $let tok : T.t -> string N.t = fun c -> N.tok (c $$ fun c -> c)

  $let star : 'a. 'a N.t -> 'a list N.t = fun x ->
    fix  (S.of_list T.[AA; AAAB]) @@ fun starx ->
    (eps (injv << [] >>))
     <|>
    (x >>> starx $ fun p -> C.let_ p @@ fun p -> injv << $(C.(dyn (fst p))) :: $(C.(dyn (snd p))) >>)

  $let grammar : unit -> string list t = fun () -> star (tok T.AA <|> tok T.AAAB) (* >>> tok T.EOF *)

  $let fused : unit -> string list F.grammar = fun () ->
    F.fuse cases (N.resolve (grammar ()))

  $let unfused : string -> bool = fun s ->
    match R.Lexer.lex cases s with
    | None -> false
    | Some ts -> R.Normalized.recognize (N.resolve (grammar ())) ts
end


let single_rec_sexp_tests _ =
  begin
    assert_equal
      (`Atom)
      (Single_rec.parser "abc-def");

    assert_equal
      (`Sexp [])
      (Single_rec.parser "()");

    assert_equal
      (`Sexp [`Atom;])
      (Single_rec.parser "( aaa )");

    assert_equal
      (`Sexp [`Atom; `Sexp [`Atom; `Sexp []]; `Atom;])
      (Single_rec.parser "  (a(aa())  a)");
  end


let backtracking _ =
  begin
    assert_equal [`AA] (Backtracking.parser "aa");

    assert_equal [`AA;`AA] (Backtracking.parser "aaaa");

    assert_equal [`AAAB] (Backtracking.parser "aaab");
  end


let recognizer_sexp_tests _ =
  begin
    assert $( Expr.bool (Sexp_recognizer.(R.Fused.recognize (fused ())) "()") );
    assert $( Expr.bool (Sexp_recognizer.unfused "()") );

    assert $( Expr.bool (Sexp_recognizer.(R.Fused.recognize (fused ())) "abcdef") );
    assert $( Expr.bool (Sexp_recognizer.unfused "abcdef") );

    assert $( Expr.bool (Sexp_recognizer.(R.Fused.recognize (fused ())) "( abc )") );
    assert $( Expr.bool (Sexp_recognizer.unfused "( abc )") );

    assert (not $( Expr.bool (Sexp_recognizer.(R.Fused.recognize (fused ())) "( abc ))") ));
    assert (not $( Expr.bool (Sexp_recognizer.unfused "( abc ))") ));

    assert (not $( Expr.bool (Sexp_recognizer.(R.Fused.recognize (fused ())) "(") ));
    assert (not $( Expr.bool (Sexp_recognizer.unfused "(") ));

    assert $( Expr.bool (Sexp_recognizer.(R.Fused.recognize (fused ())) "( (abc) ((h bc) (ef g)) )") );
    assert $( Expr.bool (Sexp_recognizer.unfused "( (abc) ((h bc) (ef g)) )") );

    assert (not $( Expr.bool (Sexp_recognizer.(R.Fused.recognize (fused ())) "( (abc) ((h bc (ef g)) )") ));
    assert (not $( Expr.bool (Sexp_recognizer.unfused "( (abc) ((h bc (ef g)) )") ));

    assert (not $( Expr.bool (Sexp_recognizer.(R.Fused.recognize (fused ())) "( (abc) ((h bc) (ef g)) ).") ));
    assert (not $( Expr.bool (Sexp_recognizer.unfused "( (abc) ((h bc) (ef g)) ).") ));
  end


let tok_recognizer_tests _ =
  begin
    assert $( Expr.bool (Tok_recognizer.(R.Fused.recognize (fused ())) "aa") );
    assert $( Expr.bool (Tok_recognizer.unfused "aa") );

    assert (not $( Expr.bool (Tok_recognizer.(R.Fused.recognize (fused ())) "aaa") ));
    assert (not $( Expr.bool (Tok_recognizer.unfused "aaa") ));

    assert $( Expr.bool (Tok_recognizer.(R.Fused.recognize (fused ())) "aaaa") );
    assert $( Expr.bool (Tok_recognizer.unfused "aaaa") );

    assert $( Expr.bool (Tok_recognizer.(R.Fused.recognize (fused ())) "aaab") );
    assert $( Expr.bool (Tok_recognizer.unfused "aaab") );
  end

let suite = "Parser tests" >:::
  ["single rec sexp"
    >:: single_rec_sexp_tests;

   "backtracking parser"
    >:: backtracking;

   "recognzier sexp"
    >:: recognizer_sexp_tests;

   "tok recognzier"
    >:: tok_recognizer_tests;
  ]


let _ =
  run_test_tt_main suite
