module T = struct
  type t = Grammar_tokens.Sexp.t = LPAREN | RPAREN | ATOM
  macro compare : t -> t -> int = fun x y -> Grammar_tokens.Sexp.compare x y
  macro pp : Format.formatter -> t -> unit = fun fmt x -> Grammar_tokens.Sexp.pp fmt x
end

module G = Grammars_common.Common[T]
module P = G.P
module L = G.L
module C = Flap.Cd

macro token : 'a. T.t -> 'a expr -> 'a P.t = fun t v -> P.tok t (fun _ -> C.injv v)
macro lparen : unit -> unit P.t = fun () -> token T.LPAREN << () >>
macro rparen : unit -> unit P.t = fun () -> token T.RPAREN << () >>
macro atom : unit -> string P.t = fun () -> P.tok T.ATOM (fun s -> s)

macro lexer : unit -> (Reex.t * P.rhs) list = fun () ->
  [ Reex.(>>>) (L.alpha ()) (Reex.star (L.alnum ())), P.Return T.ATOM;
    L.charset "\r\n \t"                             , P.Skip;
    Reex.chr '('                                    , P.Return T.LPAREN;
    Reex.chr ')'                                    , P.Return T.RPAREN;
  ]

macro parser_ : unit -> int P.t = fun () ->
  P.fix @@ fun sexp ->
   P.(<|>)
     (P.($.) (P.(>>>) (P.(>>>) (lparen ()) (P.star sexp)) (rparen ()))
       (fun p -> C.let_ p @@ fun p ->
                 C.injv << List.fold_left (+) 0 (Stdlib.snd (Stdlib.fst $(C.dyn p))) >>))
     (P.($.) (atom ()) (fun _ -> C.injv << 1 >>))

macro code : unit -> (string -> int) expr = fun () ->
  Result.get_ok (G.build "sexp" (parser_ ()) (lexer ()))
