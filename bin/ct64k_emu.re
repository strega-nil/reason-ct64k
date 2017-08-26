let main () => {
  let ops: array Opcodes.full_op = [|
    { op: Opcodes.Op_mi, fst: 0x0040, snd: 0x1100 },
    { op: Opcodes.Op_mi, fst: 0x0041, snd: 0x0001 },

    { op: Opcodes.Op_md, fst: 0x0200, snd: 0x0040 },
    { op: Opcodes.Op_ad, fst: 0x0040, snd: 0x0041 },
    { op: Opcodes.Op_md, fst: 0x0200, snd: 0x0040 },
    { op: Opcodes.Op_ad, fst: 0x0040, snd: 0x0041 },
    { op: Opcodes.Op_md, fst: 0x0200, snd: 0x0040 },
    { op: Opcodes.Op_ad, fst: 0x0040, snd: 0x0041 },
    { op: Opcodes.Op_md, fst: 0x0200, snd: 0x0040 },
    { op: Opcodes.Op_ad, fst: 0x0040, snd: 0x0041 },
    { op: Opcodes.Op_md, fst: 0x0200, snd: 0x0040 },
    { op: Opcodes.Op_ad, fst: 0x0040, snd: 0x0041 },
    { op: Opcodes.Op_md, fst: 0x0200, snd: 0x0040 },
    { op: Opcodes.Op_ad, fst: 0x0040, snd: 0x0041 },
    { op: Opcodes.Op_md, fst: 0x0200, snd: 0x0040 },
    { op: Opcodes.Op_ad, fst: 0x0040, snd: 0x0041 },
    { op: Opcodes.Op_mi, fst: 0x0000, snd: 0x0000 },
    { op: Opcodes.Op_ad, fst: 0x0040, snd: 0x0041 },
    { op: Opcodes.Op_mi, fst: 0x0000, snd: 0x0000 },
  |];
  let ops = Opcodes.memory_iter ops;
  let data = Iter.map
    (fun ch => Char.code ch)
    (Iter.Array.iter [| 'h', 'e', 'l', 'l', 'o', '!', '\n' |]);

  let state = State.from_iter (
    Iter.chain (Iter.exact_size 0xFF 0 ops) data
  );
  State.print state 0x1000 0x1020;
  print_string " --- --- ---\n";
  State.print state 0x1100 0x1120;
  while (State.next_inst state) { () }
};

main ();
