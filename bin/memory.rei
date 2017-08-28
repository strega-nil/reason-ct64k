type t;

let make: int => t;
let of_array: int => int => array int => t;
let from_iter: int => int => Iter.t int => t;
let get: t => int => int;
let set: t => int => int => unit;
