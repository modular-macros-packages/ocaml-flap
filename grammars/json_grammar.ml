module T = struct
  type t = Grammar_tokens.Json.t = COMMA | COLON | STRING | LBRACE | RBRACE
         | LBRACKET | RBRACKET | DECIMAL | NULL | TRUE | FALSE
  macro compare : t -> t -> int = fun x y -> Grammar_tokens.Json.compare x y
  macro pp : Format.formatter -> t -> unit = fun fmt x -> Grammar_tokens.Json.pp fmt x
end

module G = Grammars_common.Common[T]
module P = G.P
module L = G.L
module C = Flap.Cd

macro comma    : unit -> unit P.t = fun () -> P.tok T.COMMA    (fun _ -> C.injv << () >>)
macro colon    : unit -> unit P.t = fun () -> P.tok T.COLON    (fun _ -> C.injv << () >>)
macro string_  : unit -> string P.t = fun () -> P.tok T.STRING (fun s -> C.injv (C.dyn s))
macro lbrace   : unit -> unit P.t = fun () -> P.tok T.LBRACE   (fun _ -> C.injv << () >>)
macro rbrace   : unit -> unit P.t = fun () -> P.tok T.RBRACE   (fun _ -> C.injv << () >>)
macro lbracket : unit -> unit P.t = fun () -> P.tok T.LBRACKET (fun _ -> C.injv << () >>)
macro rbracket : unit -> unit P.t = fun () -> P.tok T.RBRACKET (fun _ -> C.injv << () >>)
macro decimal_ : unit -> string P.t = fun () -> P.tok T.DECIMAL (fun s -> C.injv (C.dyn s))
macro null     : unit -> unit P.t = fun () -> P.tok T.NULL     (fun _ -> C.injv << () >>)
macro true_    : unit -> unit P.t = fun () -> P.tok T.TRUE     (fun _ -> C.injv << () >>)
macro false_   : unit -> unit P.t = fun () -> P.tok T.FALSE    (fun _ -> C.injv << () >>)

macro lexer : unit -> (Reex.t * P.rhs) list = fun () ->
  [ Reex.chr '['      , P.Return T.LBRACKET;
    Reex.chr ']'      , P.Return T.RBRACKET;
    Reex.chr '{'      , P.Return T.LBRACE;
    Reex.chr '}'      , P.Return T.RBRACE;
    Reex.chr ','      , P.Return T.COMMA;
    Reex.chr ':'      , P.Return T.COLON;
    Reex.str "null"   , P.Return T.NULL;
    Reex.str "true"   , P.Return T.TRUE;
    Reex.str "false"  , P.Return T.FALSE;
    L.string ()       , P.Return T.STRING;
    L.decimal ()      , P.Return T.DECIMAL;
    L.charset "\r\n \t", P.Skip ]

macro commasep : int P.t -> int P.t = fun p ->
  P.fix @@ fun ps ->
  P.(<|>) (P.eps (C.injv << 0 >>))
    (P.($.) (P.(>>>) p (P.option (P.(>>>) (comma ()) ps)))
       (fun p -> C.let_ p @@ fun p ->
         C.inj << match $(C.dyn p) with
                  | (x,None) -> x
                  | (x,Some (_,xs)) -> x + xs >>))

macro delim : 'a. unit P.t -> 'a P.t -> unit P.t -> 'a P.t = fun l p r ->
  P.($.) (P.(>>>) (P.(>>>) l p) r)
    (fun p -> C.injv << let ((_,v),_) = $(C.dyn p) in v >>)

macro value : unit -> int P.t = fun () ->
  P.fix @@ fun value ->
  let member = P.($.) (P.(>>>) (string_ ()) (P.option (P.(>>>) (colon ()) value)))
                 (fun p -> C.let_ p @@ fun p ->
                   C.inj << match $(C.dyn p) with (_,None) -> 1
                          | (_,Some(_,v)) -> 1 + v >>) in
  let obj = delim (lbrace ()) (commasep member) (rbrace ())
  and arr = delim (lbracket ()) (commasep value) (rbracket ())
  in
  P.any [ obj; arr;
          P.($.) (string_ ()) (fun _ -> C.injv << 1 >>);
          P.($.) (decimal_ ()) (fun _ -> C.injv << 1 >>);
          P.($.) (null ()) (fun _ -> C.injv << 1 >>);
          P.($.) (true_ ()) (fun _ -> C.injv << 1 >>);
          P.($.) (false_ ()) (fun _ -> C.injv << 1 >>) ]

macro values : unit -> int P.t = fun () ->
  P.fix @@ fun values ->
  P.(<|>) (P.eps (C.injv << 0 >>))
    (P.($.) (P.(>>>) (value ()) values)
       (fun p -> C.let_ p @@ fun p ->
         C.inj << Stdlib.fst $(C.dyn p) + Stdlib.snd $(C.dyn p) >>))

macro code : unit -> (string -> int) expr = fun () ->
  Result.get_ok (G.build "json" (values ()) (lexer ()))
