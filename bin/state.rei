type t;
let make: array int => t;
let from_iter: ((int => unit) => unit) => t;
let next_inst: t => bool;
