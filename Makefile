PROJECT = bios

$(PROJECT): $(PROJECT).asm
	asm02 -l -L bios.asm

pico: $(PROJECT).asm
	echo Building for Pico/Elf
	rcasm -l -v -x -d 1802 -DPICOELF bios 2>&1 | tee bios.lst

elf2k: $(PROJECT).asm
	rcasm -l -v -x -d 1802 -DELF2K bios 2>&1 | tee bios.lst

mc: $(PROJECT).asm
	rcasm -l -v -x -d 1802 -DMC bios 2>&1 | tee bios.lst

mchip: $(PROJECT).asm
	rcasm -l -v -x -d 1802 -DMCHIP bios 2>&1 | tee bios.lst

clean:
	-rm $(PROJECT).prg

