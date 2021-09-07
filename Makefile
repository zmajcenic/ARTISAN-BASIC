BIN_DIR = bin
SYM_DIR = symbol

.PHONY: all basic bin

all: 

basic:
	cp basic/*.BAS dsk/
	./append_ctrl_z.sh

sound:
	rasm -sp -ob sound/AKY.bin -os sound/AKY.sym sound/AKY.asm
	rasm -sp -ob sound/AKG.bin -os sound/AKG.sym sound/AKG.asm

bin:
	sjasmplus --fullpath asm\\main.asm --raw=$(BIN_DIR)\\main.bin --sym=$(SYM_DIR)\\main.sym --lst=$(SYM_DIR)\\main.lst -I.
	sjasmplus --fullpath asm\\MBGE.asm --raw=$(BIN_DIR)\\MBGE.bin --sym=$(SYM_DIR)\\MBGE.sym --lst=$(SYM_DIR)\\MBGE.lst -I.
	cp bin/MBGE.bin dsk/

compress:
	sjasmplus --fullpath game\\sprites_permanent_data.asm --raw=$(BIN_DIR)\\sprites_permanent_data.raw --sym=$(SYM_DIR)\\sprites_permanent_data.sym
	pletter $(BIN_DIR)\\sprites_permanent_data.raw
	sjasmplus --fullpath game\\cir_data.asm --raw=$(BIN_DIR)\\cir_data.raw --sym=$(SYM_DIR)\\cir_data.sym
	pletter $(BIN_DIR)\\cir_data.raw

upload:
	pscp -P 22 bin/ff.rom pi@192.168.0.3:/media/usb0/gr8net/

clean:
	rm $(BIN_DIR)/*.bin $(BIN_DIR)/*.rom $(BIN_DIR)/*.raw $(BIN_DIR)/*.plet5 $(SYM_DIR)/*.* sound/*.bin sound/*.sym

buildnum:
	date -Iseconds > buildnum.txt
	echo " DB "\"`cat buildnum.txt`\" > buildnum.inc


	