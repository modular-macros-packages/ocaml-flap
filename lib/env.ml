
module Env $(V: sig type 'a t end) =
struct
  type entry = E : 'a Vars.Var.t * 'a V.t -> entry | Alias : 'a Vars.Var.t * 'a Vars.Var.t -> entry
(* TODO: can we make this a bit more well-formed? *)
  type t = entry list
  $let empty : t = []
  $let remove : 'a. 'a Vars.Var.t -> t -> t = fun k env ->
    let f = function
        E (x,_) -> (match Vars.Var.eql x k with Some _ -> false | _ -> true)
      | Alias (x, _)  -> (match Vars.Var.eql x k with Some _ -> false | _ -> true) in
    List.filter f env
  $let bind : 'a. 'a Vars.Var.t -> 'a V.t -> t -> t = fun k v env -> E (k, v) :: remove k env
  $let alias : 'a. 'a Vars.Var.t -> 'a Vars.Var.t -> t -> t = fun x y env -> Alias (x, y) :: remove x env
  $let extend : t -> t -> t = fun l r ->
    let f e r =
      match e with
      | E (k, v) -> bind k v r
      | Alias (x, y) -> alias x y r
    in
    List.fold_right f l r
  $let (.%{}) : 'a. t -> 'a Vars.Var.t -> 'a V.t = fun l x ->
    (* Warning: this can loop if there are cyclic aliases *)
    let rec go : type a. a Vars.Var.t -> t -> t -> a V.t =
    fun x orig -> function
    | [] -> raise Not_found
    | Alias (y, z) :: env -> (match Vars.Var.eql x y with Some Type.Equal -> go z orig orig | None -> go x orig env)
    | E (y, v) :: env -> (match Vars.Var.eql x y with Some Type.Equal -> v | None -> go x orig env)
    in go x l l

  $let canonicalize : 'a. 'a Vars.Var.t -> t -> 'a Vars.Var.t = fun x l ->
    let rec go : type a. a Vars.Var.t -> t -> t -> a Vars.Var.t =
    fun x orig -> function
    | [] -> x
    | E (y, _) :: env -> (match Vars.Var.eql x y with Some Type.Equal -> x | None -> go x orig env)
    | Alias (y, z) :: env -> (match Vars.Var.eql x y with Some Type.Equal -> go z orig orig | None -> go x orig env)
    in go x l l

    type pp_fn = { m : 'a. Format.formatter -> 'a V.t -> unit }
  $let pp : pp_fn -> Format.formatter -> t -> unit =
    fun f fmt p ->
    let g = function
      | E (x, v) -> Format.fprintf fmt "@[%s ↦ @[%a@]@],@ " (Vars.Var.str x) f.m v
      | Alias (x, y) -> Format.fprintf fmt "@[%s ↝ %s@],@ " (Vars.Var.str x) (Vars.Var.str y) in
    (Format.fprintf fmt "{"; List.iter g p; Format.fprintf fmt "}")
end
