#	For some reason, objcopy is inserting a bunch of stuff between
#	the .text and .data sections, so we'll need to extract them
#	separately and cat them together.
all::
	vax-dec-netbsdaout-gcc -fno-builtin -nostdinc -fpack-struct -c bdosmain.c
	vax-dec-netbsdaout-gcc -fno-builtin -nostdinc -fpack-struct -c bdosmisc.c
	vax-dec-netbsdaout-gcc -fno-builtin -nostdinc -fpack-struct -c bdosrw.c
	vax-dec-netbsdaout-gcc -fno-builtin -nostdinc -fpack-struct -c bios.c
	vax-dec-netbsdaout-gcc -fno-builtin -nostdinc -fpack-struct -c ccp.c
	vax-dec-netbsdaout-gcc -fno-builtin -nostdinc -fpack-struct -c conbdos.c
	vax-dec-netbsdaout-gcc -fno-builtin -nostdinc -fpack-struct -c dskutil.c
	vax-dec-netbsdaout-gcc -fno-builtin -nostdinc -fpack-struct -c fileio.c
	vax-dec-netbsdaout-gcc -fno-builtin -nostdinc -fpack-struct -c iosys.c
	vax-dec-netbsdaout-gcc -fno-builtin -nostdinc -fpack-struct -c boot.s
	vax-dec-netbsdaout-gcc -fno-builtin -nostdinc -fpack-struct -c arch.c
	../tools/mkimage <bdosPLM.img >bdosPLM.c
	vax-dec-netbsdaout-gcc -fno-builtin -nostdinc -fpack-struct -c bdosPLM.c
	vax-dec-netbsdaout-ld -N -Ttext 100000 -o cpm.aout -Map cpm.map \
	  boot.o bdosmain.o bdosmisc.o \
	  bdosrw.o bios.o ccp.o conbdos.o dskutil.o fileio.o iosys.o \
	  bdosPLM.o arch.o
	vax-dec-netbsdaout-objcopy -O binary -j .text cpm.aout cpm.text
	vax-dec-netbsdaout-objcopy -O binary -j .data cpm.aout cpm.data
	cat cpm.text cpm.data >cpm.bin
	../tools/mkasmimage <cpm.bin >image.s
	vax-dec-netbsdaout-gcc -fno-builtin -nostdinc -fpack-struct -c image.s
	vax-dec-netbsdaout-ld -o image.aout --section-start .text=0 -e image image.o
	/usr/sbin/mopcopy image.aout image.mop
