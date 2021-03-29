	.text
	.globl _start
	.extern _end
	.extern _entry
	.extern _copyrt
	.extern _bios_outzstring
###
#
# _start
#
#	This is the system entry point when the system is loaded from
#	the boot medium. It also serves as the base of the system
#	stack, once the system has been copied into its final position.
#
#	We don't know the address at which we've been loaded, so we
#	need to be position-independent until we copy the system to
#	it's final address.

_start:

#	Stop the Ethernet controller so we know it's not dorking
#	with memory.

	movl $0x200e0000,r0
	clrw 4(r0)		# Point at CSR 0
	movw $4,(r0)		# Set the stop bit

#	Now we want to copy the whole bloody program to its final
#	location. The tricky bit here is that we're using relative
#	addresses for the source address and absolute ones for the
#	destination; this should allow us to adjust to wherever
#	we happen to get loaded by the bootstrap.
#
#	NOTE:	I'm assuming we don't have to deal with overlapping
#		copies!

	moval _start,r0
	movl $_start,r1
	movl $_end,r2
	subl2 $_start,r2
loop:
	movb (r0)+,(r1)+
	sobgtr r2,loop

#	Now we need to jump to the entry point at the final address.
#	So we need to load the absolute address somewhere and jump
#	to it.

	movl $entry,r1
	jmp (r1)

###
#
# entry
#
#	This is the initial system entry point after the bootstrap
#	has copied the system into its proper location in memory. It
#	sets up the processor and enters the C code at _entry.

entry:

##	We're now executing at the final address. Say hello.
#
#	movl $0x200a0000,r0	# base address of dz
#	movw $8,8(r0)		# disable tx of everything but chan 3
#wait:	tstw (r0)		# wait for TRDY bit
#	bgeq wait
#	movw $64,0xc(r0)	# send an @
#wait2:	tstw (r0)		# wait for it to be sent
#	bgeq wait2

##	Clear the .bss section.
#
#	moval __bss_start,r0
#	moval __end,r1
#	subl3 r0,r1,r1		# form byte count
#bssloop:
#	clrb (r0)+
#	sobgeq r1,bssloop

#	Initialize the stack pointer and system control block.

	moval _start,sp

	mtpr $scb,$17

	movl _copyrt,-(sp)
	calls $1,_bios_outzstring
	calls $0,_entry

###
#
# _traphndl
#
#	The BDOS refers to this routine as the exception handler for a
#	68K trap instruction when it initializes the system. We aren't
#	using TRAP instructions to get to the BDOS and, if we were, the
#	68K vector would be the wrong one. Nevertheless, the linker
#	needs to be satisfied, so we have to have something here.
#
#	Since we have to have a trap handler, we may as well use it
#	to deal with unhandled traps and interrupts. Consequently,
#	this routine initializes the stack and enters bios_exception.
#
###

	.globl _traphndl
	.extern _bios_exception
	.align 4
_traphndl:
	moval _start,sp
	calls $0,_bios_exception

###
#
# _halt
#
#	Temporary debug routine that halts.
#
###

	.globl _halt
	.align 4
_halt:	nop		# entry mask if called
	nop
	halt
###
#
# scb
#
#	This is the system control block. For the MicroVAX 2000, we need
#	two pages: the first containing the standard exception vectors
#	and the second containing device interrupt vectors. Since we
#	don't yet have a plan for dealing with traps and do not yet
#	need interrupts, point everything at _traphndl, above.
#
###
	.align 9
scb:
	.rept 256
#	.long _traphndl
	.long _halt
	.endr

	.end
