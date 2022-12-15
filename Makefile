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
	sjasmplus -DBASIC_EXTENSION=1 -DDEFUSR_EXTENSION=0 --fullpath asm\\main.asm --raw=$(BIN_DIR)\\maine.bin --sym=$(SYM_DIR)\\maine.sym --lst=$(SYM_DIR)\\maine.lst --exp=$(SYM_DIR)\\maine.exp -I.
	sjasmplus -DBASIC_EXTENSION=1 -DDEFUSR_EXTENSION=0 --fullpath asm\\ARTISAN.asm --raw=$(BIN_DIR)\\ARTISANE.bin --sym=$(SYM_DIR)\\ARTISANE.sym --lst=$(SYM_DIR)\\ARTISANE.lst -I.
	sjasmplus -DBASIC_EXTENSION=0 -DDEFUSR_EXTENSION=1 --fullpath asm\\main.asm --raw=$(BIN_DIR)\\maind.bin --sym=$(SYM_DIR)\\maind.sym --lst=$(SYM_DIR)\\maind.lst --exp=$(SYM_DIR)\\maind.exp -I.
	sjasmplus -DBASIC_EXTENSION=0 -DDEFUSR_EXTENSION=1 --fullpath asm\\ARTISAN.asm --raw=$(BIN_DIR)\\ARTISAND.bin --sym=$(SYM_DIR)\\ARTISAND.sym --lst=$(SYM_DIR)\\ARTISAND.lst -I.
	cp bin/ARTISAN*.bin dsk/

clean:
	rm $(BIN_DIR)/*.bin dsk/*.BAS dsk/*.ASC



	