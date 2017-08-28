type t 'a = ('a => unit) => unit;
module Array = {
  let iter arr f => {
    Array.iter f arr
  };
};

let for_each iter f => iter f;

let ($) fst snd f => {
  fst f;
  snd f;
};
let map m it f => {
  it (fun el => f (m el))
};
let flat_map m it f => {
  it (fun el => m el f)
};
let rec repeat num el f => {
  f el;
  if (num > 0) {
    repeat (num - 1) el f
  } else {
    ()
  }
};
let yield el f => f el;

exception IteratorTooBig;
let exact_size sz default iter f => {
  let curr_idx = ref 0;
  let g: 'a => unit =
  fun el => {
    curr_idx := !curr_idx + 1;
    if (!curr_idx > sz) {
      raise IteratorTooBig;
    };
    f el
  };
  iter g;
  (repeat (sz - !curr_idx) default f)
};
