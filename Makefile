PROJECT = bios

$(PROJECT): $(PROJECT).asm
	rcasm -l -v -x -d 1802 bios

pico: $(PROJECT).asm
	rcasm -l -v -x -d 1802 -DPICOELF bios

elf2k: $(PROJECT).asm
	rcasm -l -v -x -d 1802 -DELF2K bios

mc: $(PROJECT).asm
	rcasm -l -v -x -d 1802 -DMC bios

clean:
	-rm $(PROJECT).prg

