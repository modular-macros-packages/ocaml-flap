
type (_, _) eql = Refl : ('a, 'a) eql

module Var =
struct
  type 'a id = ..
  type 'a t = { id: 'a id;
                str : string;
                eql : 'b. 'b id -> ('a, 'b) eql option }
  let counter = ref 0
  let var : type a. unit -> a t =
    fun () ->
    let module M = struct
          type _ id += Id : a id
          let str = incr counter; Printf.sprintf "x%d" !counter
          let eql : type b. b id -> (a, b) eql option =
            function Id -> Some Refl | _ -> None
      end
    in M.{ id = Id; eql; str }
  let eql {eql;_} {id;_} = eql id
  let str {str;_} = str
end
