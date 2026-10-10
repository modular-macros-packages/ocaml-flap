module T = struct
  type t = Grammar_tokens.Pgn.t = COORDINATE | CASTLE | STAR | INT | INTDOT | MINUS | SLASH
         | STRING | LBRACKET | RBRACKET | TAG
  $let compare : t -> t -> int = fun x y -> Grammar_tokens.Pgn.compare x y
  $let pp : Format.formatter -> t -> unit = fun fmt x -> Grammar_tokens.Pgn.pp fmt x
end

module G = Grammars_common.Common$(T)
open G

$let lexer : (Reex.t * P.rhs) list =
  Reex.(
  L.[chr '['                                    ==> LBRACKET;
    ! "]"                                       ==> RBRACKET;
    ! "/"                                       ==> SLASH;
    ! "-"                                       ==> MINUS;
    ! "[*]"                                     ==> STAR;
      string                                    ==> STRING;
      plus alpha >>> digit >>>
      star (charset "=#-+" <|> alnum)           ==> COORDINATE;
    ! "O-O(-O)?([#+])?"                         ==> CASTLE;
      plus digit >>> chr '.'                    ==> INTDOT;
      plus digit                                ==> INT;
      upper >>> star alpha                      ==> TAG;
      skip !"[\n]";                                 (* TODO: count newline *)
      skip !"[\r\n \t]";
      (* reject any "Unexpected character" *)])

module Parser =
struct
  open P
  open Flap.Cd

  $let (/=>) : 'a. T.t -> (string Flap.Cd.t -> 'a expr) -> 'a P.t =
    fun t f -> P.tok t @@ fun s -> injv (f s)
  $let coordinate : string P.t = T.COORDINATE /=> fun s -> dyn s
  $let castle     : unit P.t   = T.CASTLE     /=> fun _ -> << () >>
  $let star_      : unit P.t   = T.STAR       /=> fun _ -> << () >>
  $let int        : string P.t = T.INT        /=> fun s -> dyn s
  $let intdot     : unit P.t   = T.INTDOT     /=> fun _ -> << () >>
  $let minus      : unit P.t   = T.MINUS      /=> fun _ -> << () >>
  $let slash      : unit P.t   = T.SLASH      /=> fun _ -> << () >>
  $let string     : string P.t = T.STRING     /=> fun s -> dyn s
  $let lbracket   : unit P.t   = T.LBRACKET   /=> fun _ -> << () >>
  $let rbracket   : unit P.t   = T.RBRACKET   /=> fun _ -> << () >>
  $let tag        : string P.t = T.TAG        /=> fun s -> dyn s

  $let coordinate : [`CASTLE | `COORD of string ] P.t
                 = (coordinate $ fun s -> injv << `COORD $(dyn s) >>)
               <|> (castle     $ fun _ -> injv << `CASTLE >>)

  $let move : ((unit * [`CASTLE | `COORD of string]) * [`CASTLE | `COORD of string] option) P.t =
    intdot >>> coordinate >>> option coordinate

  $let result : [`DRAWN | `OTHER | `WON ] P.t =
    (star_ $ fun p -> let_ p @@ fun _ -> injv << `OTHER >>)
    <|>
      ((int >>>
         ((minus >>> int $ fun p -> let_ p @@ fun _ -> injv << `WON >>)
          <|>
          (slash >>> int >>> minus >>> int >>> slash >>> int $ fun p -> let_ p @@ fun _ -> injv << `DRAWN >>)))
       $ fun p -> snd p )

  $let moves : ((unit * [`CASTLE | `COORD of string]) * [`CASTLE | `COORD of string] option) list P.t =
    star move

  $let metadatum : string P.t =
    lbracket >>> tag >>> string >>> rbracket
    $ fun p -> snd (fst p)

  $let metadata : string list P.t = oneormore metadatum

  $let game : [`DRAWN | `OTHER | `WON ] P.t =
    metadata >>> moves >>> result $ fun p -> snd p

  $let games : [`DRAWN | `OTHER | `WON ] list P.t = oneormore game
end

$let code : unit -> (string -> [`DRAWN | `OTHER | `WON ] list) expr = fun () ->
  Result.get_ok (build "pgn" Parser.games lexer)
