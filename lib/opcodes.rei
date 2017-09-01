type opcode =
| Op_mi | Op_mv | Op_md
| Op_ld | Op_st
| Op_ad | Op_sb
| Op_nd | Op_or | Op_xr
| Op_sr | Op_sl | Op_sa
| Op_jg int 
| Op_jl int
| Op_jq int;

type full_op = {
  op: opcode,
  fst: int,
  snd: int,
};

let print_op: full_op => unit;
let memory_iter: array full_op => Iter.t int;
