
open Spike_grammar

let flap_point_raw : string -> point = $( Spike_grammar.point_code () )
let flap_itree_raw : string -> itree = $( Spike_grammar.itree_code () )

let classic_point s =
  match Parsexp.Single.parse_string_exn s with
  | Sexplib0.Sexp.List
      [ List [ Atom "px"; Atom px ]; List [ Atom "py"; Atom py ];
        List [ Atom "tags"; List tags ] ] ->
      { px = int_of_string px
      ; py = int_of_string py
      ; tags =
          List.map
            (function Sexplib0.Sexp.Atom a -> a | List _ -> raise Exit)
            tags
      }
  | _ -> raise Exit

let rec classic_itree_of_sexp = function
  | Sexplib0.Sexp.Atom "ILeaf" -> ILeaf
  | Sexplib0.Sexp.List [ Atom "INode"; l; Atom n; r ] ->
      INode
        ( classic_itree_of_sexp l
        , int_of_string n
        , classic_itree_of_sexp r )
  | _ -> raise Exit

let classic_itree s = classic_itree_of_sexp (Parsexp.Single.parse_string_exn s)

let flap_point s = try flap_point_raw s with _ -> classic_point s
let flap_itree s = try flap_itree_raw s with _ -> classic_itree s

let time name f =
  let t0 = Unix.gettimeofday () in
  f ();
  let t1 = Unix.gettimeofday () in
  Printf.printf "  %-26s %8.1f ns/op\n" name (1e9 *. (t1 -. t0) /. 2e6)

let n = 2_000_000
let rot arr i = Array.unsafe_get arr (i land 3)

let () =
  let point_strs =
    Array.init 4 (fun k ->
        Printf.sprintf "((px %d)(py %d)(tags (a b%d)))" k (k * 7) k)
  in
  print_endline "from-string (point), native:";
  time "classic (Parsexp + conv)" (fun () ->
      for i = 1 to n do
        ignore (Sys.opaque_identity (classic_point (rot point_strs i)))
      done);
  time "flap fused" (fun () ->
      for i = 1 to n do
        ignore (Sys.opaque_identity (flap_point (rot point_strs i)))
      done);
  let itree_strs =
    Array.init 4 (fun k ->
        Printf.sprintf
          "(INode (INode ILeaf %d ILeaf) 2 (INode ILeaf 3 (INode ILeaf 4 ILeaf)))"
          k)
  in
  print_endline "from-string (itree), native:";
  time "classic (Parsexp + conv)" (fun () ->
      for i = 1 to n do
        ignore (Sys.opaque_identity (classic_itree (rot itree_strs i)))
      done);
  time "flap fused" (fun () ->
      for i = 1 to n do
        ignore (Sys.opaque_identity (flap_itree (rot itree_strs i)))
      done)
