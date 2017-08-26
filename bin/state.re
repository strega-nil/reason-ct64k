type t = Memory.t;

let initialize mem => {
  Memory.set mem 0 0x1000;
};

let make rom => { 
  let memory = Memory.of_array 65536 0x1000 rom;
  initialize memory;
  memory
};

let from_iter rom => {
  let memory = Memory.from_iter 65536 0x1000 rom;
  initialize memory;
  memory
};

let get_op memory => {
  open Opcodes;

  let ip = Memory.get memory 0;
  let op_num = (Memory.get memory ip);
  let label = (Memory.get memory (ip + 2));

  let (op, is_jmp) = switch ((op_num lsr 12) land 0xF) {
  | 0x0 => (Op_mi, false)
  | 0x1 => (Op_mv, false)
  | 0x2 => (Op_md, false)
  | 0x3 => (Op_ld, false)
  | 0x4 => (Op_st, false)
  | 0x5 => (Op_ad, false)
  | 0x6 => (Op_sb, false)
  | 0x7 => (Op_nd, false)
  | 0x8 => (Op_or, false)
  | 0x9 => (Op_xr, false)
  | 0xA => (Op_sr, false)
  | 0xB => (Op_sl, false)
  | 0xC => (Op_sa, false)
  | 0xD => (Op_jg(label), true)
  | 0xE => (Op_jl(label), true)
  | 0xF => (Op_jq(label), true)
  | _ => failwith "unreachable"
  };
  let fst = op_num land 0x7FFF;
  let snd = (Memory.get memory (ip + 1));

  if is_jmp {
    Memory.set memory 0 (ip + 3)
  } else {
    Memory.set memory 0 (ip + 2)
  };

  { op: op, fst: fst, snd: snd }
};

let execute memory op => {
  open Opcodes;

  let write dst imm => {
    if (dst == 0x200) { /* stdout */
      print_char (Char.chr (imm land 0xFF))
    } else if (op.fst == 0x201) { /* stdin */
      /*
        do nothing, because writing to stdin should do nothing
      */
      ()
    } else {
      Memory.set memory op.fst op.snd
    }
  };
  let binop f fst snd => {
    write fst (f (Memory.get memory fst) (Memory.get memory snd))
  };

  /*
    TODO(ubsan): this is notably buggy, because it uses int
    semantics, not u16 semantics
  */
  switch op.op {
  | Op_mi => write op.fst op.snd
  | Op_mv => write op.fst (Memory.get memory op.snd)
  | Op_md =>
    write op.fst (Memory.get memory (Memory.get memory op.snd))
  | Op_ld =>
    write (Memory.get memory op.fst) (Memory.get memory op.snd)
  | Op_st =>
    write
      (Memory.get memory (Memory.get memory op.snd))
      (Memory.get memory op.fst)
  | Op_ad => binop (fun x y => x + y) op.fst op.snd
  | Op_sb => binop (fun x y => x - y) op.fst op.snd
  | Op_nd => binop (fun x y => x land y) op.fst op.snd
  | Op_or => binop (fun x y => x lor y) op.fst op.snd
  | Op_xr => binop (fun x y => x lxor y) op.fst op.snd
  | Op_sr => binop (fun x y => x lsr y) op.fst op.snd
  | Op_sl => binop (fun x y => x lsl y) op.fst op.snd
  | Op_sa => binop (fun x y => x asr y) op.fst op.snd
  | Op_jg _ => failwith "unimplemented"
  | Op_jl _ => failwith "unimplemented"
  | Op_jq _ => failwith "unimplemented"
  };

  if (op == { op: Op_mi, fst: 0x0, snd: 0x0 }) {
    false
  } else {
    true
  }
};

let next_inst memory => {
  let op = get_op memory;
  execute memory op
};
