type t;

let make: int => t;
let of_array: int => int => array int => t;
let from_iter: int => int => ((int => unit) => unit) => t;
let get: t => int => int;
let set: t => int => int => unit;
