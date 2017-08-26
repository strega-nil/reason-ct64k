type t;
let make: array int => t;
let from_iter: ((int => unit) => unit) => t;

let print: t => int => int => unit;

let next_inst: t => bool;
