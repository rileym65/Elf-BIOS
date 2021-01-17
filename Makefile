PROJECT = bios

$(PROJECT): $(PROJECT).asm
	rcasm -l -v -x -d 1802 bios 2>&1 | tee bios.lst

pico: $(PROJECT).asm
	echo Building for Pico/Elf
	rcasm -l -v -x -d 1802 -DPICOELF bios 2>&1 | tee bios.lst

elf2k: $(PROJECT).asm
	rcasm -l -v -x -d 1802 -DELF2K bios 2>&1 | tee bios.lst

mc: $(PROJECT).asm
	rcasm -l -v -x -d 1802 -DMC bios 2>&1 | tee bios.lst

clean:
	-rm $(PROJECT).prg

