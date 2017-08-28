type t;
let make: array int => t;
let from_iter: Iter.t int => t;

let print: t => int => int => unit;

let next_inst: t => bool;
