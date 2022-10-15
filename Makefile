BIN_DIR = bin
SYM_DIR = symbol

.PHONY: all basic bin sound

all: basic sound bin

basic:
	cp basic/*.ASC basic/*.BAS dsk/
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

clean:
	rm $(BIN_DIR)/*.bin dsk/*.BAS dsk/*.ASC



	