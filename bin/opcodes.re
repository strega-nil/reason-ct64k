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

let print_op op => {
  let (name, label) = switch op.op {
  | Op_mi => ("mi", None)
  | Op_mv => ("mv", None)
  | Op_md => ("md", None)
  | Op_ld => ("ld", None)
  | Op_st => ("st", None)
  | Op_ad => ("ad", None)
  | Op_sb => ("sb", None)
  | Op_nd => ("nd", None)
  | Op_or => ("or", None)
  | Op_xr => ("xr", None)
  | Op_sr => ("sr", None)
  | Op_sl => ("sl", None)
  | Op_sa => ("sa", None)
  | Op_jg lbl => ("jg", Some lbl)
  | Op_jl lbl => ("jl", Some lbl)
  | Op_jq lbl => ("jq", Some lbl)
  };
  Printf.printf "%s %X %X " name op.fst op.snd;
  switch label {
  | Some lbl => print_int lbl
  | _ => ()
  };
  print_char '\n';
};

let memory_iter: array full_op => Iter.t int =
fun arr => {
  let g op f => switch op.op {
  | Op_mi => f ((0x0 lsl 12) lor op.fst); f op.snd
  | Op_mv => f ((0x1 lsl 12) lor op.fst); f op.snd 
  | Op_md => f ((0x2 lsl 12) lor op.fst); f op.snd 
  | Op_ld => f ((0x3 lsl 12) lor op.fst); f op.snd 
  | Op_st => f ((0x4 lsl 12) lor op.fst); f op.snd 
  | Op_ad => f ((0x5 lsl 12) lor op.fst); f op.snd 
  | Op_sb => f ((0x6 lsl 12) lor op.fst); f op.snd 
  | Op_nd => f ((0x7 lsl 12) lor op.fst); f op.snd 
  | Op_or => f ((0x8 lsl 12) lor op.fst); f op.snd 
  | Op_xr => f ((0x9 lsl 12) lor op.fst); f op.snd 
  | Op_sr => f ((0xA lsl 12) lor op.fst); f op.snd 
  | Op_sl => f ((0xA lsl 12) lor op.fst); f op.snd 
  | Op_sa => f ((0xB lsl 12) lor op.fst); f op.snd 
  | Op_jg lbl => f ((0xC lsl 12) lor op.fst); f op.snd; f lbl
  | Op_jl lbl => f ((0xD lsl 12) lor op.fst); f op.snd; f lbl
  | Op_jq lbl => f ((0xE lsl 12) lor op.fst); f op.snd; f lbl
  };

  Iter.flat_map g (Iter.Array.iter arr)
};
