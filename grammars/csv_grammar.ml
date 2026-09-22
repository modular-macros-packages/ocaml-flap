(* CSV parsing, mostly according to RFC 4180 but with
   - no special treatment of headers
   - mandatory terminating CRLF
   The grammar is regular, so we don't strictly need to split it into lexer and parser.
   But it's useful to split it this way to illustrate the point.

   file = [header CRLF] record *(CRLF record) [CRLF]
   header = name *(COMMA name)
   record = field *(COMMA field)
   name = field
   field = (escaped / non-escaped)
   escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE
   non-escaped = *TEXTDATA
   COMMA = %x2C
   CR = %x0D ;as per section 6.1 of RFC 2234 [2]
   DQUOTE =  %x22 ;as per section 6.1 of RFC 2234 [2]
   LF = %x0A ;as per section 6.1 of RFC 2234 [2]
   CRLF = CR LF ;as per section 6.1 of RFC 2234 [2]
   TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
 *)
module T = struct
  type t = Grammar_tokens.Csv.t = CRLF | COMMA | FIELD
  $gen compare : t -> t -> int = fun x y -> Grammar_tokens.Csv.compare x y
  $gen pp : Format.formatter -> t -> unit = fun fmt x -> Grammar_tokens.Csv.pp fmt x
end

module G = Grammars_common.Common[T]
module P = G.P
module C = Flap.Cd

$gen lexer : unit -> (Reex.t * P.rhs) list = fun () ->
  let textdata = Reex.(<|>) (Reex.range '\x20' '\x21')
                   (Reex.(<|>) (Reex.range '\x23' '\x2B') (Reex.range '\x2D' '\x7E')) in
  let dquote = Reex.chr '"' in
  let doubledquote = Reex.(>>>) dquote dquote in
  let comma = Reex.chr ',' in
  let cr = Reex.chr '\r' in
  let lf = Reex.chr '\n' in
  let crlf = Reex.(>>>) cr lf in
  let nonescaped = Reex.plus textdata in (* TODO: not quite right: we should have star textdata here *)
  let escaped    = Reex.(>>>) dquote
                     (Reex.(>>>)
                        (Reex.star (Reex.(<|>) textdata
                                      (Reex.(<|>) comma (Reex.(<|>) cr (Reex.(<|>) lf doubledquote)))))
                        dquote) in
  [ comma                          , P.Return T.COMMA;
    crlf                           , P.Return T.CRLF;
    Reex.(<|>) escaped nonescaped  , P.Return T.FIELD ]

module Parser =
struct
  $gen comma : unit -> unit P.t = fun () -> P.tok T.COMMA (fun _ -> C.injv << () >>)
  $gen crlf  : unit -> unit P.t = fun () -> P.tok T.CRLF  (fun _ -> C.injv << () >>)
  $gen field : unit -> string P.t = fun () -> P.tok T.FIELD (fun s -> C.injv (C.dyn s))

  $gen fields : unit -> int P.t = fun () ->
    P.fix @@ fun fields ->
    P.(<|>) (P.eps (C.injv << 0 >>))
      (P.($.) (P.(>>>) (P.(>>>) (comma ()) (field ())) fields)
         (fun p -> C.let_ p @@ fun p -> C.injv << 1 + $(C.dyn (C.snd p)) >>))

  $gen record : unit -> int P.t = fun () ->
    P.($.) (P.(>>>) (P.(>>>) (field ()) (fields ())) (crlf ()))
      (fun p -> C.let_ p @@ fun p -> C.injv << 1 + $(C.dyn (C.snd (C.fst p))) >>)

  $gen records : unit -> int P.t = fun () ->
    P.fix @@ fun records ->
    P.($.) (P.(>>>) (record ()) (P.option records))
      (fun p -> C.let_ p @@ fun p ->
        let r = C.dyn (C.fst p) and rs = C.dyn (C.snd p) in
        C.inj << match $rs with
                 | None -> $r
                 | Some rs -> assert ($r = rs); $r >>)

  $gen file : unit -> int P.t = fun () -> records ()
end

$gen code : unit -> (string -> int) expr = fun () ->
  Result.get_ok (G.build "csv" (Parser.file ()) (lexer ()))
