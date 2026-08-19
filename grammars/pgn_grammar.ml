module T = struct
  type t = Grammar_tokens.Pgn.t = COORDINATE | CASTLE | STAR | INT | INTDOT | MINUS | SLASH
         | STRING | LBRACKET | RBRACKET | TAG
  macro compare : t -> t -> int = fun x y -> Grammar_tokens.Pgn.compare x y
  macro pp : Format.formatter -> t -> unit = fun fmt x -> Grammar_tokens.Pgn.pp fmt x
end

module G = Grammars_common.Common[T]
module P = G.P
module L = G.L
module C = Flap.Cd

macro lexer : unit -> (Reex.t * P.rhs) list = fun () ->
  [ L.(==>) (Reex.chr '[')                     T.LBRACKET;
    L.(==>) (L.(!) "]")                        T.RBRACKET;
    L.(==>) (L.(!) "/")                        T.SLASH;
    L.(==>) (L.(!) "-")                        T.MINUS;
    L.(==>) (L.(!) "[*]")                      T.STAR;
    L.(==>) (L.string ())                      T.STRING;
    L.(==>) (Reex.(>>>) (Reex.plus (L.alpha ()))
               (Reex.(>>>) (L.digit ())
                  (Reex.star (Reex.(<|>) (L.charset "=#-+") (L.alnum ())))))
                                               T.COORDINATE;
    L.(==>) (L.(!) "O-O(-O)?([#+])?")          T.CASTLE;
    L.(==>) (Reex.(>>>) (Reex.plus (L.digit ())) (Reex.chr '.')) T.INTDOT;
    L.(==>) (Reex.plus (L.digit ()))           T.INT;
    L.(==>) (Reex.(>>>) (L.upper ()) (Reex.star (L.alpha ()))) T.TAG;
    L.skip (L.(!) "[\n]");                         (* TODO: count newline *)
    L.skip (L.(!) "[\r\n \t]");
    (* reject any "Unexpected character" *) ]

module Parser =
struct
  macro coordinate0 : unit -> string P.t = fun () -> P.tok T.COORDINATE (fun s -> C.injv (C.dyn s))
  macro castle     : unit -> unit P.t = fun () -> P.tok T.CASTLE   (fun _ -> C.injv << () >>)
  macro star_      : unit -> unit P.t = fun () -> P.tok T.STAR     (fun _ -> C.injv << () >>)
  macro int        : unit -> string P.t = fun () -> P.tok T.INT    (fun s -> C.injv (C.dyn s))
  macro intdot     : unit -> unit P.t = fun () -> P.tok T.INTDOT   (fun _ -> C.injv << () >>)
  macro minus      : unit -> unit P.t = fun () -> P.tok T.MINUS    (fun _ -> C.injv << () >>)
  macro slash      : unit -> unit P.t = fun () -> P.tok T.SLASH    (fun _ -> C.injv << () >>)
  macro string_    : unit -> string P.t = fun () -> P.tok T.STRING (fun s -> C.injv (C.dyn s))
  macro lbracket   : unit -> unit P.t = fun () -> P.tok T.LBRACKET (fun _ -> C.injv << () >>)
  macro rbracket   : unit -> unit P.t = fun () -> P.tok T.RBRACKET (fun _ -> C.injv << () >>)
  macro tag        : unit -> string P.t = fun () -> P.tok T.TAG    (fun s -> C.injv (C.dyn s))

  macro coordinate : unit -> [`CASTLE | `COORD of string ] P.t = fun () ->
    P.(<|>)
      (P.($.) (coordinate0 ()) (fun s -> C.injv << `COORD $(C.dyn s) >>))
      (P.($.) (castle ())      (fun _ -> C.injv << `CASTLE >>))

  macro move : unit -> ((unit * [`CASTLE | `COORD of string]) * [`CASTLE | `COORD of string] option) P.t =
    fun () -> P.(>>>) (P.(>>>) (intdot ()) (coordinate ())) (P.option (coordinate ()))

  macro result : unit -> [`DRAWN | `OTHER | `WON ] P.t = fun () ->
    P.(<|>)
      (P.($.) (star_ ()) (fun p -> C.let_ p @@ fun _ -> C.injv << `OTHER >>))
      (P.($.)
         (P.(>>>) (int ())
            (P.(<|>)
               (P.($.) (P.(>>>) (minus ()) (int ())) (fun p -> C.let_ p @@ fun _ -> C.injv << `WON >>))
               (P.($.) (P.(>>>) (P.(>>>) (P.(>>>) (P.(>>>) (P.(>>>) (slash ()) (int ())) (minus ())) (int ())) (slash ())) (int ()))
                  (fun p -> C.let_ p @@ fun _ -> C.injv << `DRAWN >>))))
         (fun p -> C.snd p))

  macro moves : unit -> ((unit * [`CASTLE | `COORD of string]) * [`CASTLE | `COORD of string] option) list P.t =
    fun () -> P.star (move ())

  macro metadatum : unit -> string P.t = fun () ->
    P.($.) (P.(>>>) (P.(>>>) (P.(>>>) (lbracket ()) (tag ())) (string_ ())) (rbracket ()))
      (fun p -> C.snd (C.fst p))

  macro metadata : unit -> string list P.t = fun () -> P.oneormore (metadatum ())

  macro game : unit -> [`DRAWN | `OTHER | `WON ] P.t = fun () ->
    P.($.) (P.(>>>) (P.(>>>) (metadata ()) (moves ())) (result ()))
      (fun p -> C.snd p)

  macro games : unit -> [`DRAWN | `OTHER | `WON ] list P.t = fun () -> P.oneormore (game ())
end

macro code : unit -> (string -> [`DRAWN | `OTHER | `WON ] list) expr = fun () ->
  Result.get_ok (G.build "pgn" (Parser.games ()) (lexer ()))
