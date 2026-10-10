module T = struct
  type t = Grammar_tokens.Intexp.t = LET | ID | IN | INT | TIMES | PLUS | MINUS
         | EQUAL | THEN | ELSE | IF | LPAREN | RPAREN
  $let compare : t -> t -> int = fun x y -> Grammar_tokens.Intexp.compare x y
  $let pp : Format.formatter -> t -> unit = fun fmt x -> Grammar_tokens.Intexp.pp fmt x
end

module G = Grammars_common.Common$(T)
open G
open P
open Flap.Cd

type 'a term = (string * int) list -> 'a

$let let_   : unit P.t   = P.tok T.LET    @@ fun _ -> injv << () >>
$let id     : string P.t = P.tok T.ID     @@ fun x -> x
$let in_    : unit P.t   = P.tok T.IN     @@ fun _ -> injv << () >>
$let int    : int P.t    = P.tok T.INT    @@ fun x -> injv << int_of_string $(dyn x) >>
$let times  : unit P.t   = P.tok T.TIMES  @@ fun _ -> injv << () >>
$let plus   : unit P.t   = P.tok T.PLUS   @@ fun _ -> injv << () >>
$let minus  : unit P.t   = P.tok T.MINUS  @@ fun _ -> injv << () >>
$let equal  : unit P.t   = P.tok T.EQUAL  @@ fun _ -> injv << () >>
$let then_  : unit P.t   = P.tok T.THEN   @@ fun _ -> injv << () >>
$let else_  : unit P.t   = P.tok T.ELSE   @@ fun _ -> injv << () >>
$let if_    : unit P.t   = P.tok T.IF     @@ fun _ -> injv << () >>
$let lparen : unit P.t   = P.tok T.LPAREN @@ fun _ -> injv << () >>
$let rparen : unit P.t   = P.tok T.RPAREN @@ fun _ -> injv << () >>

$let lexer : (Reex.t * P.rhs) list =
  Reex.(
  L.[str "let"            , P.Return T.LET;
     str "in"             , P.Return T.IN;
     chr '*'              , P.Return T.TIMES;
     chr '+'              , P.Return T.PLUS;
     chr '-'              , P.Return T.MINUS;
     chr '='              , P.Return T.EQUAL;
     str "then"           , P.Return T.THEN;
     str "else"           , P.Return T.ELSE;
     str "if"             , P.Return T.IF;
     chr '('              , P.Return T.LPAREN;
     chr ')'              , P.Return T.RPAREN;
     alpha >>> star alnum , P.Return T.ID;
     plus digit           , P.Return T.INT;
     charset "\r\n \t"    , P.Skip;
  ])

$let paren : 'a. 'a P.t -> 'a P.t = fun p ->
  (lparen >>> p >>> rparen)
  $ (fun p -> inj << Stdlib.snd (Stdlib.fst $(dyn p)) >>)

$let ident : int term P.t = id  $ fun x -> injv << List.assoc $(dyn x) >>
$let int   : int term P.t = int $ fun x -> injv << fun _ -> $(dyn x) >>

$let mkOp : 'a 'b. ('a -> 'a -> 'a) expr -> ('b -> 'a) expr -> ('b -> 'a) expr -> ('b -> 'a) expr =
  fun o e1 e2 -> << fun env -> $o ($e1 env) ($e2 env) >>

$let any : 'a. 'a P.t list -> 'a P.t = fun gs -> List.fold_left (<|>) bot gs

$let maybe : 'a. 'a P.t -> 'a option P.t = fun p ->
  any [p $ (fun x -> injv << Option.Some $(dyn x) >>); eps (injv << Option.None >>)]

$let infixr : 'e 'o. ('o expr -> 'e expr -> 'e expr -> 'e expr) -> 'e P.t -> 'o P.t -> 'e P.t =
  fun f base op ->
  fix (fun g -> base >>> maybe (op >>> g)
                $ (fun x -> inj << match $(dyn x) with
                   | (e, None) -> e
                   | (e, Some(o, e')) -> $(f <<o>> <<e>> <<e'>>) >>))

$let infixl : 'e 'o. ('o expr -> 'e expr -> 'e expr -> 'e expr) -> 'e P.t -> 'o P.t -> 'e P.t =
  fun f base op ->
  base >>> star (op >>> base)
  $ fun p -> inj << let (e, oes) = $(dyn p) in
                    List.fold_left (fun e (o, e') -> $(f <<o>> <<e>> <<e'>>)) e oes >>

type assoc = Left | Right

$let mkInfix : 'e 'o. assoc ->
  ('o expr -> 'e expr -> 'e expr -> 'e expr) -> 'e P.t -> 'o P.t -> 'e P.t =
  function
  | Left -> infixl
  | Right -> infixr

$let infix : 'e 'o. ('o expr -> 'e expr -> 'e expr -> 'e expr) -> 'e P.t -> (assoc * 'o P.t) list -> 'e P.t =
  fun f base aos ->
  List.fold_left (fun base (a, o) -> mkInfix a f base o) base aos


$let exp : int term P.t =
          fix (fun exp ->
              let atom = any [paren exp; ident; int] in
              let eqexp = infix mkOp atom
                            [Left, (times $ fun _ -> injv << ( * ) >>);
                             Left, any [(plus  $ fun _ -> injv << ( + ) >>);
                                        (minus $ fun _ -> injv << ( - ) >>);];
                             Left, equal $ fun _ -> injv << fun x y -> if (x = y) then 1 else 0 >> ]
              in
              let letexp =
                (let_ >>> id >>> equal >>> exp >>> in_ >>> exp) $
                (fun p -> inj << let (((((_,x),_),e),_),e') = $(dyn p) in
                             fun env -> e' ((x,e env) :: env) >>) in
              let ifexp =
                (if_ >>> exp >>> then_ >>> exp >>> else_ >>> exp) $
                (fun p -> inj << let (((((_,e1),_),e2),_),e3) = $(dyn p) in
                             fun env -> if e1 env <> 0 then e2 env else e3 env >>)
              in
              any [eqexp; letexp; ifexp])

$let code : unit -> (string -> int term) expr = fun () ->
  Result.get_ok (build "intexp" exp lexer)
