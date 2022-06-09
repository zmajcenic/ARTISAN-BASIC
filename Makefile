BIN_DIR = bin
SYM_DIR = symbol

.PHONY: all basic bin sound

all: basic sound bin

basic:
	cp basic/*.BAS dsk/
	./append_ctrl_z.sh

sound:
	rasm -sp -ob bin/AKG.bin -os symbol/AKG.sym AKG/AKG.asm
	sjasmplus --fullpath asm\\song.asm --raw=$(BIN_DIR)\\song.bin --sym=$(SYM_DIR)\\song.sym  -I.
	sjasmplus --fullpath asm\\sfx.asm --raw=$(BIN_DIR)\\sfx.bin --sym=$(SYM_DIR)\\sfx.sym  -I.
	cp bin/song.bin bin/sfx.bin dsk/

bin:
	sjasmplus --fullpath asm\\main.asm --raw=$(BIN_DIR)\\main.bin --sym=$(SYM_DIR)\\main.sym --lst=$(SYM_DIR)\\main.lst --exp=$(SYM_DIR)\\main.exp -I.
	sjasmplus --fullpath asm\\ARTISAN.asm --raw=$(BIN_DIR)\\ARTISAN.bin --sym=$(SYM_DIR)\\ARTISAN.sym --lst=$(SYM_DIR)\\ARTISAN.lst -I.
	cp bin/ARTISAN.bin dsk/

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


	