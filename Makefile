bios.prg: bios.asm
	rcasm -l -v -x -d 1802 bios

clean:
	-rm bios.prg

