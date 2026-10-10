
type point = { px : int; py : int; tags : string list }
type itree = ILeaf | INode of itree * int * itree

let unescape (raw : string) : string =
  let n = String.length raw in
  let b = Buffer.create n in
  let i = ref 1 in
  while !i < n - 1 do
    (match raw.[!i] with
     | '\\' ->
       if !i + 1 >= n - 1 then raise Exit;
       (match raw.[!i + 1] with
        | '"' -> Buffer.add_char b '"'
        | '\\' -> Buffer.add_char b '\\'
        | 'n' -> Buffer.add_char b '\n'
        | 't' -> Buffer.add_char b '\t'
        | 'r' -> Buffer.add_char b '\r'
        | 'b' -> Buffer.add_char b '\b'
        | _ -> raise Exit);
       i := !i + 2
     | c ->
       Buffer.add_char b c;
       incr i)
  done;
  Buffer.contents b

module T = struct
  type t = Spike_tokens.t = LP | RP | ATOM | QATOM
  $let compare : t -> t -> int = fun x y -> Spike_tokens.compare x y
  $let pp : Format.formatter -> t -> unit = fun fmt x -> Spike_tokens.pp fmt x
end

module G = Grammars_common.Common$(T)
open G
open Flap.Cd

$let lp : unit P.t = P.tok T.LP @@ fun _ -> injv << () >>
$let rp : unit P.t = P.tok T.RP @@ fun _ -> injv << () >>

$let key : string -> bool P.t = fun name ->
  P.tok T.ATOM @@ fun s ->
    let_ s @@ fun s ->
      injv << String.equal $(dyn s) $(Expr.string name) >>

$let int_atom : int P.t =
  P.tok T.ATOM @@ fun s ->
    let_ s @@ fun s ->
      injv << try int_of_string $(dyn s) with _ -> raise Stdlib.Exit >>

$let[..] str_atom : unit -> string P.t = fun () ->
  P.(tok T.ATOM (fun s -> s)
     <|>
     tok T.QATOM (fun s ->
       let_ s @@ fun s -> injv << unescape $(dyn s) >>))

$let kv : 'a. string -> 'a P.t -> 'a P.t = fun name value_g ->
  P.((lp >>> key name >>> value_g >>> rp)
     $ fun p ->
       let_ p @@ fun p ->
         injv
           << if $(dyn (snd (fst (fst p))))
              then $(dyn (snd (fst p)))
              else raise Stdlib.Exit >>)

$let lexer : (Reex.t * P.rhs) list =
  Reex.(
  L.[ plus (complement "() \t\n\r\";#|\\") , P.Return ATOM;
      chr '"' >>> (star (complement "\"\\" <|> (chr '\\' >>> any)) >>> chr '"'),
                                           P.Return QATOM;
      charset " \t\n\r"                    , P.Skip;
      chr '('                              , P.Return LP;
      chr ')'                              , P.Return RP;
  ])

$let[..] point_g : unit -> point P.t = fun () ->
  P.((lp
      >>> kv "px" int_atom
      >>> kv "py" int_atom
      >>> kv "tags"
            ((lp >>> star (str_atom ()) >>> rp)
             $ fun p -> let_ p @@ fun p -> snd (fst p))
      >>> rp)
     $ fun p ->
       let_ p @@ fun p ->
         let body = fst p in
         injv
           << { px = $(dyn (snd (fst (fst body))))
              ; py = $(dyn (snd (fst body)))
              ; tags = $(dyn (snd body))
              } >>)

$let itree_g : itree P.t =
  P.(fix @@ fun self ->
     tok T.ATOM (fun s ->
       let_ s @@ fun s ->
         injv
           << if String.equal $(dyn s) "ILeaf" then ILeaf
              else raise Stdlib.Exit >>)
     <|>
     ((lp >>> key "INode" >>> self >>> (int_atom >>> self) >>> rp)
      $ fun p ->
        let_ p @@ fun p ->
          let body = fst p in
          injv
            << if $(dyn (snd (fst (fst body))))
               then
                 INode
                   ( $(dyn (snd (fst body)))
                   , $(dyn (fst (snd body)))
                   , $(dyn (snd (snd body))) )
               else raise Stdlib.Exit >>))

$let[..] point_code : unit -> (string -> point) expr = fun () ->
  Result.get_ok (build "spike_point" (point_g ()) lexer)

$let itree_code : unit -> (string -> itree) expr = fun () ->
  Result.get_ok (build "spike_itree" itree_g lexer)
