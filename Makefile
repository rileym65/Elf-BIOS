PROJECT = bios

$(PROJECT).prg: $(PROJECT).asm
	cpp $(PROJECT).asm -o - | sed -e 's/^#.*//' > temp.asm
	rcasm -l -v -x -d 1802 temp
	mv temp.prg $(PROJECT).prg

clean:
	-rm $(PROJECT).prg

