

open OUnit2

module Single_rec =
struct
  module C = Flap.Cd
  module T = struct
    type t = Sexp_tokens.t = LPAREN | RPAREN | SYMBOL
    macro compare : t -> t -> int = fun x y -> Sexp_tokens.compare x y
    macro pp : Format.formatter -> t -> unit = fun fmt x -> Sexp_tokens.pp fmt x
  end
  module P = Flap.Parse[T]

  macro cases : unit -> (Reex.t * P.rhs) list = fun () ->
    [ Reex.chr '(', P.Return T.LPAREN;
      Reex.(>>>) (Reex.range 'a' 'z') (Reex.star (Reex.range 'a' 'z')), P.Return T.SYMBOL;
      Reex.chr ')', P.Return T.RPAREN;
      Reex.chr ' ', P.Skip ]

  macro tok' : T.t -> string P.t = fun c -> P.tok c (fun c -> c)

  macro star' : 'a. 'a P.t -> 'a list P.t = fun x ->
    P.fix (fun starx ->
      P.(<|>) (P.eps (C.injv << [] >>))
        (P.($.) (P.(>>>) x starx)
          (fun p -> C.let_ p (fun p -> C.injv << $(C.dyn (C.fst p)) :: $(C.dyn (C.snd p)) >>))))

  macro sexp : unit -> ([`Atom | `Sexp of 'sexp list] as 'sexp) P.t = fun () ->
    P.fix (fun sexp ->
      P.(<|>)
        (P.($.) (tok' T.SYMBOL) (fun _ -> C.injv << `Atom >>))
        (P.($.) (P.(>>>) (P.(>>>) (tok' T.LPAREN) (star' sexp)) (tok' T.RPAREN))
          (fun p -> C.let_ p (fun p -> C.injv << `Sexp $(C.dyn (C.snd (C.fst p))) >>))))

  macro compiled : unit -> (string -> ([`Atom | `Sexp of 'sexp list] as 'sexp)) expr = fun () ->
    match P.compile (cases ()) (sexp ()) with
    | Ok c -> c
    | Error e -> failwith e

  let parser_fn = $( compiled () )
end


module Backtracking =
struct
  module C = Flap.Cd
  module T = struct
    type t = Bt_tokens.t = AA | AAAB | EOF
    macro compare : t -> t -> int = fun x y -> Bt_tokens.compare x y
    macro pp : Format.formatter -> t -> unit = fun fmt x -> Bt_tokens.pp fmt x
  end
  module P = Flap.Parse[T]

  macro cases : unit -> (Reex.t * P.rhs) list = fun () ->
    [ Reex.str "aa",   P.Return T.AA;
      Reex.str "aaab", P.Return T.AAAB;
      Reex.str "",     P.Return T.EOF ]

  macro tok' : T.t -> string P.t = fun c -> P.tok c (fun c -> c)

  macro star' : 'a. 'a P.t -> 'a list P.t = fun x ->
    P.fix (fun starx ->
      P.(<|>) (P.eps (C.injv << [] >>))
        (P.($.) (P.(>>>) x starx)
          (fun p -> C.let_ p (fun p -> C.injv << $(C.dyn (C.fst p)) :: $(C.dyn (C.snd p)) >>))))

  macro aa : unit -> [`AA | `AAAB] P.t = fun () -> P.($.) (tok' T.AA) (fun _ -> C.injv << `AA >>)
  macro aaab : unit -> [`AA | `AAAB] P.t = fun () -> P.($.) (tok' T.AAAB) (fun _ -> C.injv << `AAAB >>)

  macro grammar : unit -> [`AA | `AAAB] list P.t = fun () -> star' (P.(<|>) (aa ()) (aaab ()))

  macro compiled : unit -> (string -> [`AA | `AAAB] list) expr = fun () ->
    match P.compile (cases ()) (grammar ()) with
    | Ok c -> c
    | Error e -> failwith e

  let parser_fn = $( compiled () )
end


module Sexp_recognizer =
struct
  module C = Flap.Cd
  module P = Single_rec.P
  module T = Single_rec.T
  module N = P.Unresolved
  module R = P.Recognizer

  macro cases : unit -> (Reex.t * P.Fused.lexer_rhs) list = fun () ->
    [ Reex.chr '(', P.Fused.Return T.LPAREN;
      Reex.(>>>) (Reex.range 'a' 'z') (Reex.star (Reex.range 'a' 'z')), P.Fused.Return T.SYMBOL;
      Reex.chr ')', P.Fused.Return T.RPAREN;
      Reex.chr ' ', P.Fused.Skip ]

  macro tok' : T.t -> string N.t = fun c -> N.tok (N.($$) c (fun c -> c))

  macro startset : unit -> P.TermSet.t = fun () -> P.TermSet.of_list [T.LPAREN; T.SYMBOL]

  macro star' : 'a. 'a N.t -> 'a list N.t = fun x ->
    N.fix (startset ()) (fun starx ->
      N.(<|>) (N.eps (C.injv << [] >>))
        (N.($.) (N.(>>>) x starx)
          (fun p -> C.let_ p (fun p -> C.injv << $(C.dyn (C.fst p)) :: $(C.dyn (C.snd p)) >>))))

  macro sexp : unit -> ([`Atom | `Sexp of 'sexp list] as 'sexp) N.t = fun () ->
    N.fix (startset ()) (fun sexp ->
      N.(<|>)
        (N.($.) (tok' T.SYMBOL) (fun _ -> C.injv << `Atom >>))
        (N.($.) (N.(>>>) (N.(>>>) (tok' T.LPAREN) (star' sexp)) (tok' T.RPAREN))
          (fun p -> C.let_ p (fun p -> C.injv << `Sexp $(C.dyn (C.snd (C.fst p))) >>))))

  macro fused_recognize : string -> bool = fun s ->
    R.FusedR.recognize (P.Fused.fuse (cases ()) (P.Resolve.resolve (sexp ()))) s

  macro unfused : string -> bool = fun s ->
    match R.Lexer.lex (cases ()) s with
    | None -> false
    | Some ts -> R.Normalized.recognize (P.Resolve.resolve (sexp ())) ts
end


module Tok_recognizer =
struct
  module C = Flap.Cd
  module T = struct
    type t = Tok_tokens.t = AA | AAAB
    macro compare : t -> t -> int = fun x y -> Tok_tokens.compare x y
    macro pp : Format.formatter -> t -> unit = fun fmt x -> Tok_tokens.pp fmt x
  end
  module P = Flap.Parse[T]
  module N = P.Unresolved
  module R = P.Recognizer

  macro cases : unit -> (Reex.t * P.Fused.lexer_rhs) list = fun () ->
    [ Reex.str "aa", P.Fused.Return T.AA;
      Reex.str "aaab", P.Fused.Return T.AAAB ]

  macro tok' : T.t -> string N.t = fun c -> N.tok (N.($$) c (fun c -> c))

  macro star' : 'a. 'a N.t -> 'a list N.t = fun x ->
    N.fix (P.TermSet.of_list [T.AA; T.AAAB]) (fun starx ->
      N.(<|>) (N.eps (C.injv << [] >>))
        (N.($.) (N.(>>>) x starx)
          (fun p -> C.let_ p (fun p -> C.injv << $(C.dyn (C.fst p)) :: $(C.dyn (C.snd p)) >>))))

  macro grammar : unit -> string list N.t = fun () -> star' (N.(<|>) (tok' T.AA) (tok' T.AAAB))

  macro fused_recognize : string -> bool = fun s ->
    R.FusedR.recognize (P.Fused.fuse (cases ()) (P.Resolve.resolve (grammar ()))) s

  macro unfused : string -> bool = fun s ->
    match R.Lexer.lex (cases ()) s with
    | None -> false
    | Some ts -> R.Normalized.recognize (P.Resolve.resolve (grammar ())) ts
end


let single_rec_sexp_tests _ =
  begin
    assert_equal
      (`Atom)
      (Single_rec.parser_fn "abc-def");

    assert_equal
      (`Sexp [])
      (Single_rec.parser_fn "()");

    assert_equal
      (`Sexp [`Atom;])
      (Single_rec.parser_fn "( aaa )");

    assert_equal
      (`Sexp [`Atom; `Sexp [`Atom; `Sexp []]; `Atom;])
      (Single_rec.parser_fn "  (a(aa())  a)");
  end


let backtracking _ =
  begin
    assert_equal [`AA] (Backtracking.parser_fn "aa");

    assert_equal [`AA;`AA] (Backtracking.parser_fn "aaaa");

    assert_equal [`AAAB] (Backtracking.parser_fn "aaab");
  end


let recognizer_sexp_tests _ =
  begin
    assert $( Expr.bool (Sexp_recognizer.fused_recognize "()") );
    assert $( Expr.bool (Sexp_recognizer.unfused "()") );

    assert $( Expr.bool (Sexp_recognizer.fused_recognize "abcdef") );
    assert $( Expr.bool (Sexp_recognizer.unfused "abcdef") );

    assert $( Expr.bool (Sexp_recognizer.fused_recognize "( abc )") );
    assert $( Expr.bool (Sexp_recognizer.unfused "( abc )") );

    assert (not $( Expr.bool (Sexp_recognizer.fused_recognize "( abc ))") ));
    assert (not $( Expr.bool (Sexp_recognizer.unfused "( abc ))") ));

    assert (not $( Expr.bool (Sexp_recognizer.fused_recognize "(") ));
    assert (not $( Expr.bool (Sexp_recognizer.unfused "(") ));

    assert $( Expr.bool (Sexp_recognizer.fused_recognize "( (abc) ((h bc) (ef g)) )") );
    assert $( Expr.bool (Sexp_recognizer.unfused "( (abc) ((h bc) (ef g)) )") );

    assert (not $( Expr.bool (Sexp_recognizer.fused_recognize "( (abc) ((h bc (ef g)) )") ));
    assert (not $( Expr.bool (Sexp_recognizer.unfused "( (abc) ((h bc (ef g)) )") ));

    assert (not $( Expr.bool (Sexp_recognizer.fused_recognize "( (abc) ((h bc) (ef g)) ).") ));
    assert (not $( Expr.bool (Sexp_recognizer.unfused "( (abc) ((h bc) (ef g)) ).") ));
  end


let tok_recognizer_tests _ =
  begin
    assert $( Expr.bool (Tok_recognizer.fused_recognize "aa") );
    assert $( Expr.bool (Tok_recognizer.unfused "aa") );

    assert (not $( Expr.bool (Tok_recognizer.fused_recognize "aaa") ));
    assert (not $( Expr.bool (Tok_recognizer.unfused "aaa") ));

    assert $( Expr.bool (Tok_recognizer.fused_recognize "aaaa") );
    assert $( Expr.bool (Tok_recognizer.unfused "aaaa") );

    assert $( Expr.bool (Tok_recognizer.fused_recognize "aaab") );
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
