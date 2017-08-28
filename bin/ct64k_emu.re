let main () => {
  let ops: array Opcodes.full_op = [|
    { op: Opcodes.Op_mi, fst: 0x0040, snd: 0x8000 },
    { op: Opcodes.Op_mi, fst: 0x0041, snd: 0x0002 },
    { op: Opcodes.Op_sa, fst: 0x0040, snd: 0x0041 },
  |];
  let ops = Opcodes.memory_iter ops;
  let data = Iter.yield 0;

  let state = State.from_iter (
    Iter.(exact_size 0xFF 0 ops $ data)
  );
  while (State.next_inst state) { () };
  State.print state 0x0040 0x0048;
};

main ();
