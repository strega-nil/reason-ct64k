module Main: {
  let main: unit => unit;
} = {
  module Buffer: {
    type t;

    let make: int => t;
    let of_array: int => int => array int => t;
    let from_iter: int => int => ((int => unit) => unit) => t;
    let get: t => int => int;
    let set: t => int => int => unit;
  } = {
    open Bigarray;

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
      iter f;
      buff
    };
    let of_array size offset arr => {
      let iter arr f => Array.iter f arr;
      from_iter size offset (iter arr)
    };
  };

  module Opcodes = {
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

    let memory_iter: array full_op => (int => unit) => unit =
    fun arr f => {
      let g op => switch op.op {
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
      Array.iter g arr 
    };
  };

  module State: {
    type t;
    let make: array int => t;
    let from_iter: ((int => unit) => unit) => t;
    let next_inst: t => bool;
  } = {
    type t = Buffer.t;

    let initialize mem => {
      Buffer.set mem 0 0x1000;
    };

    let make rom => { 
      let memory = Buffer.of_array 65536 0x1000 rom;
      initialize memory;
      memory
    };

    let from_iter rom => {
      let memory = Buffer.from_iter 65536 0x1000 rom;
      initialize memory;
      memory
    };

    let get_op memory => {
      open Opcodes;

      let ip = Buffer.get memory 0;
      let op_num = (Buffer.get memory ip);
      let label = (Buffer.get memory (ip + 2));

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
      let fst = op_num land 0x7F;
      let snd = (Buffer.get memory (ip + 1));

      if is_jmp {
        Buffer.set memory 0 (ip + 3)
      } else {
        Buffer.set memory 0 (ip + 2)
      };

      { op: op, fst: fst, snd: snd }
    };

    let next_inst memory => {
      let op = get_op memory;
      Opcodes.print_op op;
      if (op == { op: Op_mi, fst: 0x0, snd: 0x0 }) {
        false
      } else {
        true
      }
    }
  };

  let main () => {
    let state = State.from_iter (Opcodes.memory_iter [|
      { op: Op_mi, fst: 0x0, snd: 0x1000 }
    |]);
    while (State.next_inst state) { () }
  };
};

Main.main ();
