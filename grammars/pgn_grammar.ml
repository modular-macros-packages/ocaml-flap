module T = struct
  type t = Grammar_tokens.Pgn.t = COORDINATE | CASTLE | STAR | INT | INTDOT | MINUS | SLASH
         | STRING | LBRACKET | RBRACKET | TAG
  $gen compare : t -> t -> int = fun x y -> Grammar_tokens.Pgn.compare x y
  $gen pp : Format.formatter -> t -> unit = fun fmt x -> Grammar_tokens.Pgn.pp fmt x
end

module G = Grammars_common.Common[T]
module P = G.P
module L = G.L
module C = Flap.Cd

$gen lexer : unit -> (Reex.t * P.rhs) list = fun () ->
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
  $gen coordinate0 : unit -> string P.t = fun () -> P.tok T.COORDINATE (fun s -> C.injv (C.dyn s))
  $gen castle     : unit -> unit P.t = fun () -> P.tok T.CASTLE   (fun _ -> C.injv << () >>)
  $gen star_      : unit -> unit P.t = fun () -> P.tok T.STAR     (fun _ -> C.injv << () >>)
  $gen int        : unit -> string P.t = fun () -> P.tok T.INT    (fun s -> C.injv (C.dyn s))
  $gen intdot     : unit -> unit P.t = fun () -> P.tok T.INTDOT   (fun _ -> C.injv << () >>)
  $gen minus      : unit -> unit P.t = fun () -> P.tok T.MINUS    (fun _ -> C.injv << () >>)
  $gen slash      : unit -> unit P.t = fun () -> P.tok T.SLASH    (fun _ -> C.injv << () >>)
  $gen string_    : unit -> string P.t = fun () -> P.tok T.STRING (fun s -> C.injv (C.dyn s))
  $gen lbracket   : unit -> unit P.t = fun () -> P.tok T.LBRACKET (fun _ -> C.injv << () >>)
  $gen rbracket   : unit -> unit P.t = fun () -> P.tok T.RBRACKET (fun _ -> C.injv << () >>)
  $gen tag        : unit -> string P.t = fun () -> P.tok T.TAG    (fun s -> C.injv (C.dyn s))

  $gen coordinate : unit -> [`CASTLE | `COORD of string ] P.t = fun () ->
    P.(<|>)
      (P.($.) (coordinate0 ()) (fun s -> C.injv << `COORD $(C.dyn s) >>))
      (P.($.) (castle ())      (fun _ -> C.injv << `CASTLE >>))

  $gen move : unit -> ((unit * [`CASTLE | `COORD of string]) * [`CASTLE | `COORD of string] option) P.t =
    fun () -> P.(>>>) (P.(>>>) (intdot ()) (coordinate ())) (P.option (coordinate ()))

  $gen result : unit -> [`DRAWN | `OTHER | `WON ] P.t = fun () ->
    P.(<|>)
      (P.($.) (star_ ()) (fun p -> C.let_ p @@ fun _ -> C.injv << `OTHER >>))
      (P.($.)
         (P.(>>>) (int ())
            (P.(<|>)
               (P.($.) (P.(>>>) (minus ()) (int ())) (fun p -> C.let_ p @@ fun _ -> C.injv << `WON >>))
               (P.($.) (P.(>>>) (P.(>>>) (P.(>>>) (P.(>>>) (P.(>>>) (slash ()) (int ())) (minus ())) (int ())) (slash ())) (int ()))
                  (fun p -> C.let_ p @@ fun _ -> C.injv << `DRAWN >>))))
         (fun p -> C.snd p))

  $gen moves : unit -> ((unit * [`CASTLE | `COORD of string]) * [`CASTLE | `COORD of string] option) list P.t =
    fun () -> P.star (move ())

  $gen metadatum : unit -> string P.t = fun () ->
    P.($.) (P.(>>>) (P.(>>>) (P.(>>>) (lbracket ()) (tag ())) (string_ ())) (rbracket ()))
      (fun p -> C.snd (C.fst p))

  $gen metadata : unit -> string list P.t = fun () -> P.oneormore (metadatum ())

  $gen game : unit -> [`DRAWN | `OTHER | `WON ] P.t = fun () ->
    P.($.) (P.(>>>) (P.(>>>) (metadata ()) (moves ())) (result ()))
      (fun p -> C.snd p)

  $gen games : unit -> [`DRAWN | `OTHER | `WON ] list P.t = fun () -> P.oneormore (game ())
end

$gen code : unit -> (string -> [`DRAWN | `OTHER | `WON ] list) expr = fun () ->
  Result.get_ok (G.build "pgn" (Parser.games ()) (lexer ()))
