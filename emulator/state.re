open Lib;

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
  let fst = op_num land 0x0FFF;
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
    } else if (dst == 0x201) { /* stdin */
      /*
        do nothing, because writing to stdin should do nothing
      */
      ()
    } else {
      Memory.set memory dst imm
    }
  };
  let binop f fst snd => {
    let dst = Memory.get memory fst;
    let src = Memory.get memory snd;
    let res = f dst src;
    write fst res
  };
  let jump_cnd f fst snd lbl => {
    let fst = Memory.get memory fst;
    let snd = Memory.get memory snd;
    if (f fst snd) {
      Memory.set memory 0 lbl
    }
  };

  let asr16 lhs rhs => {
    let rhs = rhs land 15;
    if (((lhs lsr 15) land 1) != 0) {
      /* shift in ones */
      let tmp = lhs lsr rhs;
      let mask = 0xFFFF lxor ((1 lsl (16 - rhs)) - 1);
      tmp lor mask
    } else {
      /* shift in zeroes */
      lhs lsr rhs
    }
  };

  switch op.op {
  | Op_mi => write op.fst op.snd
  | Op_mv => write op.fst (Memory.get memory op.snd)
  | Op_md => {
    let addr = Memory.get memory op.snd;
    let imm = Memory.get memory addr;
    write op.fst imm
  }
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
  | Op_sa => binop (fun x y => asr16 x y) op.fst op.snd
  | Op_jg lbl => jump_cnd (fun x y => x > y) op.fst op.snd lbl
  | Op_jl lbl => jump_cnd (fun x y => x < y) op.fst op.snd lbl
  | Op_jq lbl => jump_cnd (fun x y => x == y) op.fst op.snd lbl
  };

  if (op == { op: Op_mi, fst: 0x0, snd: 0x0 }) {
    false
  } else {
    true
  }
};

let rec print state start end_ => {
  let print_row () => {
    Printf.printf
      "%X: [ %04X | %04X | %04X | %04X | %04X | %04X | %04X | %04X ]\n"
      start
      (Memory.get state (start + 0))
      (Memory.get state (start + 1))
      (Memory.get state (start + 2))
      (Memory.get state (start + 3))
      (Memory.get state (start + 4))
      (Memory.get state (start + 5))
      (Memory.get state (start + 6))
      (Memory.get state (start + 7));
  };

  if (start < end_) {
    print_row ();
    print state (start + 8) end_
  }
};

let next_inst memory => {
  let op = get_op memory;
  execute memory op
};
