module T = struct
  type t = Grammar_tokens.Intexp.t = LET | ID | IN | INT | TIMES | PLUS | MINUS
         | EQUAL | THEN | ELSE | IF | LPAREN | RPAREN
  $gen compare : t -> t -> int = fun x y -> Grammar_tokens.Intexp.compare x y
  $gen pp : Format.formatter -> t -> unit = fun fmt x -> Grammar_tokens.Intexp.pp fmt x
end

module G = Grammars_common.Common[T]
module P = G.P
module L = G.L
module C = Flap.Cd

type 'a term = (string * int) list -> 'a

$gen let_   : unit -> unit P.t = fun () -> P.tok T.LET    (fun _ -> C.injv << () >>)
$gen id     : unit -> string P.t = fun () -> P.tok T.ID   (fun x -> x)
$gen in_    : unit -> unit P.t = fun () -> P.tok T.IN     (fun _ -> C.injv << () >>)
$gen int0   : unit -> int P.t = fun () -> P.tok T.INT     (fun x -> C.injv << int_of_string $(C.dyn x) >>)
$gen times  : unit -> unit P.t = fun () -> P.tok T.TIMES  (fun _ -> C.injv << () >>)
$gen plus   : unit -> unit P.t = fun () -> P.tok T.PLUS   (fun _ -> C.injv << () >>)
$gen minus  : unit -> unit P.t = fun () -> P.tok T.MINUS  (fun _ -> C.injv << () >>)
$gen equal  : unit -> unit P.t = fun () -> P.tok T.EQUAL  (fun _ -> C.injv << () >>)
$gen then_  : unit -> unit P.t = fun () -> P.tok T.THEN   (fun _ -> C.injv << () >>)
$gen else_  : unit -> unit P.t = fun () -> P.tok T.ELSE   (fun _ -> C.injv << () >>)
$gen if_    : unit -> unit P.t = fun () -> P.tok T.IF     (fun _ -> C.injv << () >>)
$gen lparen : unit -> unit P.t = fun () -> P.tok T.LPAREN (fun _ -> C.injv << () >>)
$gen rparen : unit -> unit P.t = fun () -> P.tok T.RPAREN (fun _ -> C.injv << () >>)

$gen lexer : unit -> (Reex.t * P.rhs) list = fun () ->
  [ Reex.str "let"    , P.Return T.LET;
    Reex.str "in"     , P.Return T.IN;
    Reex.chr '*'      , P.Return T.TIMES;
    Reex.chr '+'      , P.Return T.PLUS;
    Reex.chr '-'      , P.Return T.MINUS;
    Reex.chr '='      , P.Return T.EQUAL;
    Reex.str "then"   , P.Return T.THEN;
    Reex.str "else"   , P.Return T.ELSE;
    Reex.str "if"     , P.Return T.IF;
    Reex.chr '('      , P.Return T.LPAREN;
    Reex.chr ')'      , P.Return T.RPAREN;
    Reex.(>>>) (L.alpha ()) (Reex.star (L.alnum ())), P.Return T.ID;
    Reex.plus (L.digit ()) , P.Return T.INT;
    L.charset "\r\n \t" , P.Skip;
  ]

$gen paren : 'a. 'a P.t -> 'a P.t = fun p ->
  P.($.) (P.(>>>) (P.(>>>) (lparen ()) p) (rparen ()))
    (fun p -> C.inj << Stdlib.snd (Stdlib.fst $(C.dyn p)) >>)

$gen ident : unit -> int term P.t = fun () ->
  P.($.) (id ()) (fun x -> C.injv << List.assoc $(C.dyn x) >>)
$gen int   : unit -> int term P.t = fun () ->
  P.($.) (int0 ()) (fun x -> C.injv << fun _ -> $(C.dyn x) >>)

$gen mkOp : 'a 'b. ('a -> 'a -> 'a) expr -> ('b -> 'a) expr -> ('b -> 'a) expr -> ('b -> 'a) expr =
  fun o e1 e2 -> << fun env -> $o ($e1 env) ($e2 env) >>

$gen maybe : 'a. 'a P.t -> 'a option P.t = fun p ->
  P.any [ P.($.) p (fun x -> C.injv << Option.Some $(C.dyn x) >>);
          P.eps (C.injv << Option.None >>) ]

$gen infixr : 'e 'o. ('o expr -> 'e expr -> 'e expr -> 'e expr) -> 'e P.t -> 'o P.t -> 'e P.t =
  fun f base op ->
  P.fix (fun g ->
    P.($.) (P.(>>>) base (maybe (P.(>>>) op g)))
      (fun x -> C.inj << match $(C.dyn x) with
                 | (e, None) -> e
                 | (e, Some(o, e')) -> $(f <<o>> <<e>> <<e'>>) >>))

$gen infixl : 'e 'o. ('o expr -> 'e expr -> 'e expr -> 'e expr) -> 'e P.t -> 'o P.t -> 'e P.t =
  fun f base op ->
  P.($.) (P.(>>>) base (P.star (P.(>>>) op base)))
    (fun p -> C.inj << let (e, oes) = $(C.dyn p) in
                       List.fold_left (fun e (o, e') -> $(f <<o>> <<e>> <<e'>>)) e oes >>)

type assoc = Left | Right

$gen mkInfix : 'e 'o. assoc ->
  ('o expr -> 'e expr -> 'e expr -> 'e expr) -> 'e P.t -> 'o P.t -> 'e P.t =
  fun a -> match a with
  | Left -> infixl
  | Right -> infixr

$gen infix : 'e 'o. ('o expr -> 'e expr -> 'e expr -> 'e expr) -> 'e P.t -> (assoc * 'o P.t) list -> 'e P.t =
  fun f base aos ->
  List.fold_left (fun base (a, o) -> mkInfix a f base o) base aos

$gen exp : unit -> int term P.t = fun () ->
  P.fix (fun exp ->
    let atom = P.any [paren exp; ident (); int ()] in
    let eqexp = infix mkOp atom
                  [Left, (P.($.) (times ()) (fun _ -> C.injv << ( * ) >>));
                   Left, P.any [(P.($.) (plus ())  (fun _ -> C.injv << ( + ) >>));
                                (P.($.) (minus ()) (fun _ -> C.injv << ( - ) >>));];
                   Left, P.($.) (equal ()) (fun _ -> C.injv << fun x y -> if (x = y) then 1 else 0 >>) ]
    in
    let letexp =
      P.($.) (P.(>>>) (P.(>>>) (P.(>>>) (P.(>>>) (P.(>>>) (let_ ()) (id ())) (equal ())) exp) (in_ ())) exp)
        (fun p -> C.inj << let (((((_,x),_),e),_),e') = $(C.dyn p) in
                           fun env -> e' ((x,e env) :: env) >>) in
    let ifexp =
      P.($.) (P.(>>>) (P.(>>>) (P.(>>>) (P.(>>>) (P.(>>>) (if_ ()) exp) (then_ ())) exp) (else_ ())) exp)
        (fun p -> C.inj << let (((((_,e1),_),e2),_),e3) = $(C.dyn p) in
                           fun env -> if e1 env <> 0 then e2 env else e3 env >>)
    in
    P.any [eqexp; letexp; ifexp])

$gen code : unit -> (string -> int term) expr = fun () ->
  Result.get_ok (G.build "intexp" (exp ()) (lexer ()))
