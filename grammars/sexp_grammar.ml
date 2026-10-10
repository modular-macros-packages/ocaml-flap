module T = struct
  type t = Grammar_tokens.Sexp.t = LPAREN | RPAREN | ATOM
  $let compare : t -> t -> int = fun x y -> Grammar_tokens.Sexp.compare x y
  $let pp : Format.formatter -> t -> unit = fun fmt x -> Grammar_tokens.Sexp.pp fmt x
end

module G = Grammars_common.Common$(T)
open G
open Flap.Cd

$let token : 'a. T.t -> 'a expr -> 'a P.t = fun t v -> P.tok t @@ fun _ -> injv v
$let lparen : unit P.t = token T.LPAREN << () >>
$let rparen : unit P.t = token T.RPAREN << () >>
$let atom : string P.t = P.tok T.ATOM @@ fun s -> s

$let lexer : (Reex.t * P.rhs) list =
  Reex.(L.[ alpha >>> star alnum , P.Return ATOM;
            charset "\r\n \t"    , P.Skip;
            chr '('              , P.Return LPAREN;
            chr ')'              , P.Return RPAREN;
  ])

$let parser : int P.t = let open P in
  fix @@ fun sexp ->
   ((lparen >>> star sexp >>> rparen)
     $ fun p -> let_ p @@ fun p ->
                injv << List.fold_left (+) 0 (Stdlib.snd (Stdlib.fst $(dyn p))) >>)
   <|>
   (atom $ fun _ -> injv << 1 >>)

$let code : unit -> (string -> int) expr = fun () ->
  Result.get_ok (build "sexp" parser lexer)
