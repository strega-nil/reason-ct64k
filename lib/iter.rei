type t 'a;
module Array: {
  let iter: array 'a => t 'a;
};

let from_func: (unit => option 'a) => t 'a;

let for_each: t 'a => ('a => unit) => unit;

let yield: 'a => t 'a;
/* chain */
let ($): t 'a => t 'a => t 'a;

let map: ('a => 'b) => t 'a => t 'b;
let flat_map: ('a => t 'b) => t 'a => t 'b;
let repeat: int => 'a => t 'a;

exception IteratorTooBig;
/**
  raises the IteratorTooBig exception if the original iterator
  goes outside the bounds of the size
*/
let exact_size: int => 'a => t 'a => t 'a;
