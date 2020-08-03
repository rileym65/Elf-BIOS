PROJECT = bios

$(PROJECT).prg: $(PROJECT).asm
	rcasm -l -v -x -d 1802 $(PROJECT)

clean:
	-rm $(PROJECT).prg

