let main () => {
  let state = State.from_iter (Opcodes.memory_iter [|
    { op: Op_mi, fst: 0x0, snd: 0x1000 }
  |]);
  while (State.next_inst state) { () }
};

main ();
