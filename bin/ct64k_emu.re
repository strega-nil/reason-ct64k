
let main () => {
  let state = State.from_iter (Opcodes.memory_iter [|
    { op: Opcodes.Op_mi, fst: 0x200, snd: Char.code 'h' },
    { op: Opcodes.Op_mi, fst: 0x200, snd: Char.code 'e' },
    { op: Opcodes.Op_mi, fst: 0x200, snd: Char.code 'l' },
    { op: Opcodes.Op_mi, fst: 0x200, snd: Char.code 'l' },
    { op: Opcodes.Op_mi, fst: 0x200, snd: Char.code 'o' },
    { op: Opcodes.Op_mi, fst: 0x200, snd: Char.code '!' },
    { op: Opcodes.Op_mi, fst: 0x200, snd: Char.code '\n' },
  |]);
  while (State.next_inst state) { () }
};

main ();
