These are the assembly files for my homebrew 6502-pc.

use 
  vasm6502_oldstyle -Fbin -dotdir -esc -wdc02 -o snake.bin snake.s
to assemble the code

and
  eProg -size=32768 snake.bin
to upload it.
