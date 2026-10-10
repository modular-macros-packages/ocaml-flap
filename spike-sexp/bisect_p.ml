
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
module P = G.P
module C = Flap.Cd

$let lp : unit -> unit P.t = fun () -> P.tok T.LP (fun _ -> C.injv << () >>)
$let rp : unit -> unit P.t = fun () -> P.tok T.RP (fun _ -> C.injv << () >>)

$let key : string -> bool P.t = fun name ->
  P.tok T.ATOM (fun s ->
    C.let_ s (fun s ->
      C.injv << String.equal $(C.dyn s) $(Expr.string name) >>))

$let int_atom : unit -> int P.t = fun () ->
  P.tok T.ATOM (fun s ->
    C.let_ s (fun s ->
      C.injv << try int_of_string $(C.dyn s) with _ -> raise Stdlib.Exit >>))

$let[..] str_atom : unit -> string P.t = fun () ->
  P.(<|>)
    (P.tok T.ATOM (fun s -> s))
    (P.tok T.QATOM (fun s ->
       C.let_ s (fun s -> C.injv << unescape $(C.dyn s) >>)))

$let kv : 'a. string -> 'a P.t -> 'a P.t = fun name value_g ->
  P.( $ )
    (P.(>>>) (P.(>>>) (P.(>>>) (lp ()) (key name)) value_g) (rp ()))
    (fun p ->
      C.let_ p (fun p ->
        C.injv
          << if $(C.dyn (C.snd (C.fst (C.fst p))))
             then $(C.dyn (C.snd (C.fst p)))
             else raise Stdlib.Exit >>))

$let lexer : unit -> (Reex.t * P.rhs) list = fun () ->
  [ Reex.plus (G.L.complement "() \t\n\r\";#|\\"), P.Return T.ATOM;
    Reex.(>>>) (Reex.chr '"')
      (Reex.(>>>)
         (Reex.star
            (Reex.(<|>)
               (G.L.complement "\"\\")
               (Reex.(>>>) (Reex.chr '\\') Reex.any)))
         (Reex.chr '"')),
    P.Return T.QATOM;
    G.L.charset " \t\n\r", P.Skip;
    Reex.chr '(', P.Return T.LP;
    Reex.chr ')', P.Return T.RP;
  ]

$let[..] point_g : unit -> point P.t = fun () ->
  P.( $ )
    (P.(>>>)
       (P.(>>>)
          (P.(>>>)
             (P.(>>>) (lp ()) (kv "px" (int_atom ())))
             (kv "py" (int_atom ())))
          (kv "tags"
             (P.( $ )
                (P.(>>>) (P.(>>>) (lp ()) (P.star (str_atom ()))) (rp ()))
                (fun p -> C.let_ p (fun p -> C.snd (C.fst p))))))
       (rp ()))
    (fun p ->
      C.let_ p (fun p ->
        let body = C.fst p in
        C.injv
          << { px = $(C.dyn (C.snd (C.fst (C.fst body))))
             ; py = $(C.dyn (C.snd (C.fst body)))
             ; tags = $(C.dyn (C.snd body))
             } >>))

$let itree_g : unit -> itree P.t = fun () ->
  P.fix (fun self ->
    P.(<|>)
      (P.tok T.ATOM (fun s ->
         C.let_ s (fun s ->
           C.injv
             << if String.equal $(C.dyn s) "ILeaf" then ILeaf
                else raise Stdlib.Exit >>)))
      (P.( $ )
         (P.(>>>)
            (P.(>>>)
               (P.(>>>) (P.(>>>) (lp ()) (key "INode")) self)
               (P.(>>>) (int_atom ()) self))
            (rp ()))
         (fun p ->
           C.let_ p (fun p ->
             let body = C.fst p in
             C.injv
               << if $(C.dyn (C.snd (C.fst (C.fst body))))
                  then
                    INode
                      ( $(C.dyn (C.snd (C.fst body)))
                      , $(C.dyn (C.fst (C.snd body)))
                      , $(C.dyn (C.snd (C.snd body))) )
                  else raise Stdlib.Exit >>))))

$let[..] point_code : unit -> (string -> point) expr = fun () ->
  Result.get_ok (G.build "spike_point" (point_g ()) (lexer ()))

$let itree_code : unit -> (string -> itree) expr = fun () ->
  Result.get_ok (G.build "spike_itree" (itree_g ()) (lexer ()))

