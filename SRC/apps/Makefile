#	For some reason, objcopy is inserting a bunch of stuff between
#	the .text and .data sections, so we'll need to extract them
#	separately and cat them together.
all::
	vax-dec-netbsdaout-gcc -fno-builtin -nostdinc -c hello.s
	vax-dec-netbsdaout-ld -N -Ttext 200 -Map hello.map -o hello.aout hello.o
	vax-dec-netbsdaout-objcopy -O binary -j .text hello.aout hello.text
	vax-dec-netbsdaout-objcopy -O binary -j .data hello.aout hello.data
	cat hello.text hello.data >HELLO.VAX
	vax-dec-netbsdaout-gcc -fno-builtin -nostdinc -c halt.s
	vax-dec-netbsdaout-ld -N -Ttext 200 -Map halt.map -o halt.aout halt.o
	vax-dec-netbsdaout-objcopy -O binary -j .text halt.aout halt.text
	vax-dec-netbsdaout-objcopy -O binary -j .data halt.aout halt.data
	cat halt.text halt.data >HALT.VAX
	vax-dec-netbsdaout-gcc -c -fno-builtin -nostdinc -Wa,-a nv32forth.s >nv32forth.lst
	vax-dec-netbsdaout-ld -N -Ttext 2000 -Map nv32forth.map -o nv32forth.aout nv32forth.o
	vax-dec-netbsdaout-objcopy -O binary -j .text nv32forth.aout CFORTH.VAX
	vax-dec-netbsdaout-gcc -c -fno-builtin -nostdinc -Wa,-a circforth.s >circforth.lst
	vax-dec-netbsdaout-ld -N -Ttext 2000 -Map nv32forth.map -o circforth.aout circforth.o
	vax-dec-netbsdaout-objcopy -O binary -j .text circforth.aout CIRC4TH.VAX
