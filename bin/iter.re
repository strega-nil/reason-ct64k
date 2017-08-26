type t 'a = ('a => unit) => unit;
module Array = {
  let iter arr f => {
    Array.iter f arr
  };
};

let chain fst snd f => {
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

exception IteratorTooBig;
let exact_size: int => 'a => t 'a => t 'a =
fun (sz: int) (default: 'a) (iter: t 'a) (f: 'a => unit) => {
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
