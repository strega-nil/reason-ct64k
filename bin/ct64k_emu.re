module Main: {
  let main: unit => unit;
} = {
  module Buffer: {
    type t;

    let make: int => t;
    let of_array: int => int => array int => t;
    let get: t => int => int;
    let set: t => int => int => unit;
  } = {
    open Bigarray;

    type t = Array1.t int int16_unsigned_elt c_layout;

    let make n => Array1.create Int16_unsigned c_layout n;
    let get arr idx => Array1.get arr idx;
    let set arr idx n => Array1.set arr idx n;

    let of_array size offset arr => {
      let buff = make size;
      let rec fill i => {
        set buff (i + offset) (Array.get arr i);
        if (i != 0) {
          fill (i - 1)
        }
      };
      let length = Array.length arr;
      if (length != 0) {
        fill (length - 1);
      };
      buff
    };
  };

  module State: {
    type t;
    let make: array int => t;
    let next_inst: t => bool;
  } = {
    type t = {
      memory: Buffer.t,
    };

    let make rom => { 
      let memory = Buffer.of_array 65536 0x1000 rom;
      Buffer.set memory 0 0x1000;
      { memory: memory }
    };

    type opcode =
    | Op_mi | Op_mv | Op_md
    | Op_ld | Op_st
    | Op_ad | Op_sb | Op_nd
    | Op_or | Op_xr | Op_sl | Op_sa
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

    let get_op state => {
      let ip = Buffer.get state.memory 0;
      let op_num = (Buffer.get state.memory ip);
      let label = (Buffer.get state.memory (ip + 2));

      let (op, is_jmp) = switch ((op_num lsr 12) land 0xF) {
      | 0x0 => (Op_mi, false)
      | 0xF => (Op_jq(label), true)
      | _ => failwith "unimplemented"
      };
      let fst = op_num land 0x7F;
      let snd = (Buffer.get state.memory (ip + 1));

      if is_jmp {
        Buffer.set state.memory 0 (ip + 3)
      } else {
        Buffer.set state.memory 0 (ip + 2)
      };

      { op: op, fst: fst, snd: snd }
    };

    let next_inst state => {
      let op = get_op state;
      print_op op;
      if (op == { op: Op_mi, fst: 0x0, snd: 0x0 }) {
        false
      } else {
        true
      }
    }
  };

  let main () => {
    let state = State.make [|
      0x000F, 0x000F,
      0xFFFF, 0x0000, 0x0000,
      0x0000, 0x0000,
    |];
    while (State.next_inst state) { () }
  };
};

Main.main ();
