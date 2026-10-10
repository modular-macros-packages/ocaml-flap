module T = struct
  type t = Grammar_tokens.Json.t = COMMA | COLON | STRING | LBRACE | RBRACE
         | LBRACKET | RBRACKET | DECIMAL | NULL | TRUE | FALSE
  $let compare : t -> t -> int = fun x y -> Grammar_tokens.Json.compare x y
  $let pp : Format.formatter -> t -> unit = fun fmt x -> Grammar_tokens.Json.pp fmt x
end

module G = Grammars_common.Common$(T)
open G
open Flap.Cd

$let (/=>) : 'a. T.t -> (string Flap.Cd.t -> 'a expr) -> 'a P.t =
  fun t f -> P.tok t @@ fun s -> injv (f s)
$let comma    : unit P.t   = T.COMMA    /=> fun _ -> << () >>
$let colon    : unit P.t   = T.COLON    /=> fun _ -> << () >>
$let string_  : string P.t = T.STRING   /=> fun s -> dyn s
$let lbrace   : unit P.t   = T.LBRACE   /=> fun _ -> << () >>
$let rbrace   : unit P.t   = T.RBRACE   /=> fun _ -> << () >>
$let lbracket : unit P.t   = T.LBRACKET /=> fun _ -> << () >>
$let rbracket : unit P.t   = T.RBRACKET /=> fun _ -> << () >>
$let decimal_ : string P.t = T.DECIMAL  /=> fun s -> dyn s
$let null     : unit P.t   = T.NULL     /=> fun _ -> << () >>
$let true_    : unit P.t   = T.TRUE     /=> fun _ -> << () >>
$let false_   : unit P.t   = T.FALSE    /=> fun _ -> << () >>

$let lexer : (Reex.t * P.rhs) list =
  Reex.(
  L.[
       chr '['           , P.Return LBRACKET;
       chr ']'           , P.Return RBRACKET;
       chr '{'           , P.Return LBRACE;
       chr '}'           , P.Return RBRACE;
       chr ','           , P.Return COMMA;
       chr ':'           , P.Return COLON;
       str "null"        , P.Return NULL;
       str "true"        , P.Return TRUE;
       str "false"       , P.Return FALSE;
       string            , P.Return STRING;
       decimal           , P.Return DECIMAL;
       charset "\r\n \t" , P.Skip;
  ])

$let commasep : int P.t -> int P.t = fun p ->
  P.(fix @@ fun ps ->
     eps (injv << 0 >>) <|> (p >>> option (comma >>> ps) $ fun p -> let_ p @@ fun p ->
     inj << match $(dyn p) with
        | (x,None) -> x
        | (x,Some (_,xs)) -> x + xs >>))
$let delim : 'a. unit P.t -> 'a P.t -> unit P.t -> 'a P.t = fun l p r ->
  P.((l >>> p >>> r) $ fun p -> injv << let ((_,v),_) = $(dyn p) in  v >>)

$let value : int P.t = P.(fix @@ fun value ->
  let member = string_ >>>
                 option (colon >>> value) $
                 fun p -> let_ p @@ fun p -> inj << match $(dyn p) with  (_,None) -> 1
                                                 | (_,Some(_,v)) -> 1 + v >> in
  let obj = delim lbrace (commasep member) rbrace
  and arr = delim lbracket (commasep value) rbracket
  in
  obj <|> arr <|> (string_ $ fun _ -> injv << 1 >>) <|>
  (decimal_ $ fun _ -> injv << 1 >>) <|>
  (null $ fun _ -> injv << 1 >>) <|>
  (true_ $ fun _ -> injv << 1 >>) <|>
  (false_ $ fun _ -> injv << 1 >>))

$let values : int P.t = P.(fix @@ fun values ->
  eps (injv << 0 >>) <|> (value >>> values $ fun p -> let_ p @@ fun p -> inj << Stdlib.fst $(dyn p) + Stdlib.snd $(dyn p) >>))

$let parser : int P.t = values

$let code : unit -> (string -> int) expr = fun () ->
  Result.get_ok (build "json" parser lexer)
