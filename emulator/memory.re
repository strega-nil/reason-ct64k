open Bigarray;
open Lib;

type t = Array1.t int int16_unsigned_elt c_layout;

let make n => Array1.create Int16_unsigned c_layout n;

let get arr idx => Array1.get arr idx;
let set arr idx n => Array1.set arr idx n;

let from_iter size offset iter => {
  let buff = make size;
  let idx = ref offset;
  let f n => {
    set buff !idx n;
    idx := !idx + 1;
  };
  Iter.for_each iter f;
  buff
};
let of_array size offset arr => {
  from_iter size offset (Iter.Array.iter arr)
};

