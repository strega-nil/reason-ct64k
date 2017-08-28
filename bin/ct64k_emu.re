let main () => {
  open Opcodes;
  let rom = [|
    { op: Op_mi, fst: 0x0040, snd: 0xB0FF },
    { op: Op_mi, fst: 0x0041, snd: 0x1006 },
    { op: Op_mi, fst: 0x0000, snd: 0x100A },
    { op: Op_mi, fst: 0x0200, snd: Char.code '\n' },
    { op: Op_mi, fst: 0x0000, snd: 0x0000 },

    { op: Op_mi, fst: 0x0042, snd: 0 },
    { op: Op_mi, fst: 0x0043, snd: 10 },
    { op: Op_mi, fst: 0x0044, snd: 15 },
    { op: Op_mi, fst: 0x0045, snd: Char.code '0' },
    { op: Op_mi, fst: 0x0046, snd: 55 },

    { op: Op_mv, fst: 0x0047, snd: 0x0040 },
    { op: Op_mi, fst: 0x0048, snd: 0x000C },
    { op: Op_sr, fst: 0x0047, snd: 0x0048 },
    { op: Op_nd, fst: 0x0047, snd: 0x0044 },
    { op: Op_jl(0x1023), fst: 0x0047, snd: 0x0043 },
    { op: Op_ad, fst: 0x0047, snd: 0x0046 },
    { op: Op_mi, fst: 0x0000, snd: 0x1025 },
    { op: Op_ad, fst: 0x0047, snd: 0x0045 },

    { op: Op_mv, fst: 0x0200, snd: 0x0047 },
    { op: Op_mv, fst: 0x0047, snd: 0x0040 },
    { op: Op_mi, fst: 0x0048, snd: 0x0008 },
    { op: Op_sr, fst: 0x0047, snd: 0x0048 },
    { op: Op_nd, fst: 0x0047, snd: 0x0044 },
    { op: Op_jl(0x1036), fst: 0x0047, snd: 0x0043, },
    { op: Op_ad, fst: 0x0047, snd: 0x0046 },
    { op: Op_mi, fst: 0x0000, snd: 0x1038 },
    { op: Op_ad, fst: 0x0047, snd: 0x0045 },

    { op: Op_mv, fst: 0x0200, snd: 0x0047 },
    { op: Op_mv, fst: 0x0047, snd: 0x0040 },
    { op: Op_mi, fst: 0x0048, snd: 0x0004 },
    { op: Op_sr, fst: 0x0047, snd: 0x0048 },
    { op: Op_nd, fst: 0x0047, snd: 0x0044 },
    { op: Op_jl(0x1049), fst: 0x0047, snd: 0x0043 },
    { op: Op_ad, fst: 0x0047, snd: 0x0046 },
    { op: Op_mi, fst: 0x0000, snd: 0x104B },
    { op: Op_ad, fst: 0x0047, snd: 0x0045 },

    { op: Op_mv, fst: 0x0200, snd: 0x0047 },
    { op: Op_mv, fst: 0x0047, snd: 0x0040 },
    { op: Op_nd, fst: 0x0047, snd: 0x0044 },
    { op: Op_jl(0x1058), fst: 0x0047, snd: 0x0043 },
    { op: Op_ad, fst: 0x0047, snd: 0x0046 },
    { op: Op_mi, fst: 0x0000, snd: 0x105A },
    { op: Op_ad, fst: 0x0047, snd: 0x0045 },
    { op: Op_mv, fst: 0x0200, snd: 0x0047 },
    { op: Op_mv, fst: 0x0000, snd: 0x0041 },
  |];

  let state = State.from_iter (Opcodes.memory_iter rom);
  while (State.next_inst state) { () };
};

main ();
