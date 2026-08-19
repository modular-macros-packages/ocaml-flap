
let sexp : string -> int = $( Sexp_grammar.code () )
let csv : string -> int = $( Csv_grammar.code () )
let json : string -> int = $( Json_grammar.code () )
let intexp = $( Intexp_grammar.code () )
let ppm : string -> bool = $( Ppm_grammar.code () )
let pgn = $( Pgn_grammar.code () )

let () = begin
    assert (sexp "(a (b c))" = 3);
  assert (sexp "()" = 0);

    assert (csv "a,b\r\nc,d\r\n" = 2);

    assert (json "[1,2]" = 2);
  assert (json "{\"a\":1} true" = 3);

    assert (intexp "let x = 2 in x + 3" [] = 5);
  assert (intexp "if 1 then 2 * 21 else 0" [] = 42);

    assert (ppm "P3 2 1 255 1 2 3 4 5 6");
  assert (not (ppm "P3 2 1 255 1 2 3"));

    assert (pgn "[Event \"x\"]\n1. e4 e5 2. Nf3 Nc6 1-0" = [`WON]);

  print_endline "grammar smoke tests: all ok"
end
