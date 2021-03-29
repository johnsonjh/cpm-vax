	.text
#####
#
#	This particular variant limits the math stack to a 256-byte
#	page. This is annoying, in that none of the really cool VAX
#	addressing modes work that way...
#
#####

#####
#
# __start
#
#	CP/M enters the program here. We are passed a single
#	parameter, the address of the base page. The first longword of
#	the base page contains a pointer to the BDOS, which we need
#	to squirrel away so we can call it.
#
#####

__start:
	.word 0x0ffe		# entry mask
	movl 4(ap),r0
	movl r0,__base_page
	movl (r0),__bdos_pointer
	movl sp,__initial_sp
	jmp Forth$$Begin

__bdos_pointer:
	.long 0
__base_page:
	.long 0
__initial_sp:
	.long 0

####
#
# ConIn
#
#	Receives a character from the console, returning it in R0.
#
####

__ConIn_Args:
	.long 2			# Two parameters
	.long 6			# Direct Console I/O
	.long 0xff		# Read a character

ConIn:	movl __bdos_pointer,r0
	callg __ConIn_Args,(r0)
	rsb

####
#
# ConSt
#
#	Polls the console to see if a character is available.
#
###

__ConSt_Args:
	.long 2			# Two parameters
	.long 6			# Direct Console I/O
	.long 0xfe		# Check status

ConSt:	movl __bdos_pointer,r0
	callg __ConSt_Args,(r0)
	rsb

####
#
# ConOut
#
#	Display the character on R0 on the console.
#
####

__ConOut_Args:
	.long 2			# Two parameters
	.long 6			# Direct Console I/O
__ConOut_Char:
	.long 0			# The character

ConOut:
	movl r0,__ConOut_Char
	movl __bdos_pointer,r0
	callg __ConOut_Args,(r0)
	rsb

 #****************************************************************************
 #
 #	Position-independent 32-bit VAX Forth small enough to fit in the
 #	primary cache of NV5.
 #
 #	Modified for use in a downline-boot diagnostic environment. Offsets
 #	are now 32 bits, allowing 4MB to be ALLOTted for TURBOchannel
 #	DMA skidpad.
 #
 #	Although the data manipulated by this Forth is 32-bits, 16-bit
 #	relative offsets are stored in compiled words. This is to make it
 #	1) position-independent and 2) small enough to fit in NV5's PCache.
 #	Since the target execution environment is NV5's PCache, limiting
 #	code size to 64KB using the 16-bit offsets is not a problem.
 #
 #	This forth started out as port of Allan Pratt's C-Forth, which is
 #	a port of FigFORTH to a portable C for migration to Un*x boxes.
 #	Mr. Pratt dropped some features he couldn't figure out how to implement
 #	(such as DOES> and vocabularies), and I don't understand them (yet);
 #	they remain dropped. The vocabulary is largely FigFORTH.
 #
 #	I've also dropped the "user variable" concept because I don't
 #	understand it. I believe it exists mainly to support FORTH multitasking,
 #	which I don't plan on ever using.
 #
 # revisions:
 #
 #	2005-03-28 rli: Began translation to gas.
 #
 #*****************************************************************************

 #	Macro to generate a string consisting of ASCII characters with the
 #	last bit set.

	.macro string value
	.byte 1f-0f
0:
	.ascii "\value"
1:
	.endm

 #	Macro to generate the header for a word. A header looks like this:
 #
 #	+--------+
 #	|1IS     | Length of name. S = 0 to allow name to be found
 #	+--------+ \               I = 1 for immediate word
 #	|0       |  |
 #	/\/\/\/\/\  |
 #	/\/\/\/\/\   > Characters in name
 #	|0       |  |
 #	+--------+ /
 #	|        |     Offset to previous word's name
 #	|        |
 #	+--------+
 #	|        |     Code Field Address
 #	|        |
 #	+--------+
 #	|        |     Parameter Field Address
 #	/\/\/\/\/\
 #	/\/\/\/\/\
 #	|        |
 #	+--------+
 #
 #	The code field address is the address of machine code that is
 #	executed when the word is referenced. For Forth code, this
 #	machine code pushes the IP on the stack and begins interpreting
 #	the parameter field as forth words.
 #
 #	A reference to a forth word is two levels away from machine code.
 #	The first level is a reference to the CFA of the word. The second
 #	level is the machine code that knows how to "do" the word. The
 #	forth interpreter must relocate at each level of indirection to
 #	make a PIC implementation.
 #
 #	This macro also has to handle the special case of a null Codename
 #	for the null word.

Immediate = 0x40

###
#
#	Because gas sucks so bad, we have to tell it the name of the
#	previous word so we can build the link field. A real assembler
#	allows us to keep a symbol updated as we build the links, but
#	gas fills all the links in using the final value of the symbol.
#	ARGH!
#
###
	.macro header_nfalink previousname,suffix
	.long \previousname\suffix - .
	.endm

	.macro header_label name,suffix
\name\suffix:
	.endm

	.macro header_pfaoffset name,suffix
\name\suffix = .+4
	.endm

 #	Macro for putting forth words together

	.macro offset label
	.long \label-.
	.endm

###
#
#	Generate a special header for the null word, which is immediate.
#	Since the list of words has to start somewhere and gas sucks so
#	much that it can't keep track of that for us, we now require the
#	null word to be the first word in the dictionary.
#
###

	.macro null_header
	.equ header_temp,.
	header_label "null","_nfa"
	.byte 1+0x80+0x20+0x40
	.byte 0
	header_label "null","_lfa"
	.long 0
null:
	offset DoCol
	.endm

	.macro header codename, username, previousname, flags=0
	header_label "\codename","_nfa"
	.byte (1f-0f)+\flags+0x80+0x20
0:
	.ascii "\username"
1:
	header_label "\codename","_lfa"
	header_nfalink "\previousname","_nfa"
\codename:
	header_pfaoffset "\codename","_pfa"
	.endm

 #	Macro to generate a variable. It's initialized to zero.

	.macro variable name, username, previousname
	Header "\name", "\username", "\previousname"
	offset DoVar
	.long 0
	.endm

 #	Macro to generate a constant.

	.macro constant name, username, previousname, value
	header "\name","\username","\previousname"
	offset DoCon
	.long \value
	.endm

 #	Macro to generate a forth word

	.macro colon name, username, previousname, immediate=0
	header "\name","\username","\previousname",\immediate
	offset DoCol
	.endm

 #	Macro to generate a machine code word that can be referenced as
 #	if it were a forth word; i.e., one that shows up in the dictionary.
 #	For these words, the address of the machine code that knows how to
 #	"do" the word is the address of the PFA.

	.macro code name, username, previousname, immediate=0
	header "\name","\username","\previousname",\immediate
	.long 4
	.endm

 #	Macro to compile a literal into a forth code word
 #
 #	If the literal fits into a single byte, ZBLIT is used instead of
 #	LIT; ZBLIT knows how to zero-extend bytes into a longword.
 #
 #	Unfortunately, LITERAL isn't smart enough to know about ZBLIT; in
 #	other words, the base word set can make use of this optimization,
 #	but it isn't available for any words you type in.

	.macro literal, value
 #.if value & ~255
	offset LIT
	.long \value
 #.else
 #	offset ZBLIT
 #	.byte \value
 #.endc
	.endm

 #	Macro to compile a DOES> trampoline. If I ever get serious about
 #	using DOES>, I'll want one of these.

	.macro does

	offset PSemiCode	# Point the new word's CFA at the
	.long 8			# trampoline
	offset SemiS

 #	Here's the DOES> trampoline

	subb2 $4,r7		# Make space on the math stack for the PFA
	moval 4(r9),(r7)	# Push the PFA on the stack
	movl r10,-(sp)		# Save the next IP so we can return to it
	moval 1f,r10		# Point at the DOES> tail
	jmp (r11)		# Execute the DOES> tail
1:				# DOES> tail goes here.
	.endm

 #	The initialization code is responsible for providing us with a
 #	return stack. I did this mainly so I could debug the sucker under
 #	VMS without running into problems with the tiny stack I want to
 #	use in NV5's PCache (one page of stack just isn't enough to do
 #	a QIO).

 #	Space for the math stack

	.align 8
MathSpace:
	.space 256,0

 #	Space for terminal input; when you type something at the console, it
 #	goes here.

TibBuffer:			# For BDOS function 10
	.byte 79
	.byte 0
TibSpace:
	.space 79,0
TibSpaceEnd:

 #	Space for building formatted numeric output. This is much larger
 #	than is needed for formatted numeric output, but the space may
 #	come in handy for other things.

	.space 79,0
PadSpaceEnd:

 #	Register Usage:
 #
 #	R0 -
 #	R1 -
 #	R2 -
 #	R3 -
 #	R4 -
 #	R5 -
 #	R6 - 
 #	R7 - math stack pointer
 #	R8 -
 #	R9 -  W, may be munched by CODE words
 #	R10 - IP
 #	R11 - Contains the address of Next so words can exit by JMP (R11),
 #	      one byte shorter than BRW Next
 #	R12 - VAX AP
 #	R13 - VAX FP
 #	R14 - VAX SP, used for return stack
 #	R15 - VAX PC, of course

Next:

 #C	This is the big kabloona. What it does is load the value at mem[ip]
 #C	into w, increment ip, and invoke prim. number w. This implies that
 #C	mem[ip] is the CFA of a word. What's in the CF of a word is the
 #C	number of the primitive which should be executed. For a word written
 #C	in FORTH, that primitive is "docol", which pushes ip to the return
 #C	stack, then uses w+2 (the PFA of the word) as the new ip. See 
 #C	"interp.doc" for more

 #	What I'm doing differently is using word offsets in the CFA to the
 #	word to be executed. The assumption is that the code will be
 #	relatively small (this thing is meant to be able to execute in the
 #	8K cache inside the NV5), so I don't want to be lugging around
 #	32 bit addresses. Therefore, what I actually do is add the word
 #	at mem[ip] to ip to come up with the value to be loaded in w.
 #	Then, I can't simply jump to the address stored in mem[w]; I have
 #	relocate it before I can jump to it.

	addl3 (r10),r10,r9	# Fetch the offset to next forth word and
				# convert it to an address
	addl3 (r9),r9,r8	# Fetch the offset to the machine code
				# and convert it to an address
	tstl (r10)+		# ip++;
	jmp (r8)		# Execute the word. The word finishes by
				# jumping to Next

 #	Here's the code that knows how to enter a Forth word

DoCol:
	movl r10,-(sp)		# Save the return address
	addl3 $4,r9,r10		# Start executing after docol
	jmp (r11)

 #	Here's the code that knows how to do a constant

DoCon:
	subb2 $4,r7
	movl 4(r9),(r7)
	jmp (r11)

 #	Here's the code that knows how to do a variable

DoVar:
	subb2 $4,r7
	addl3 $4,r9,(r7)
	jmp (r11)

 #	: {NUL}
 #	  R> DROP
 #	;*
 #
 #	This special word is found at the end of a command line. It is
 #	used to break out of the interpreter; at the end of the command
 #	line, the interpreter will search for the terminating null ending
 #	the line. It will find and execute this word. This word drops
 #	a return address from the return stack.

	null_header
	offset FromR
	offset DROP
	offset SemiS

 #	Branch: Add an offset to the IP.
 #
 #	The word following the the opcode is taken as a relative offset
 #	indicating the length of the branch. At entry, IP (R10) points to the
 #	word following the opcode, because Next incremented IP before executing
 #	the opcode. This word is fetched, sign-extended, and added to IP.
 #
 #	The branch code is also used by 0Branch.

	code "BRANCH","BRANCH","null"
ZBranch_Branch:
	addl2 (r10),r10		# Update IP.
	jmp (r11)

 #	0Branch: Add an offset to IP if tos == 0
 #
 #	The word following the opcode is taken as a relative offset that
 #	is to be added to IP (R10) if the top element on the math stack
 #	is zero. In either case, the top element is removed from the
 #	math stack.
 #
 #	The code to skip the relative offset if the condition is not
 #	met is used in several places.

	code "ZBranch","0BRANCH","BRANCH"
	movl (r7),r0		# get TOS
	addb2 $4,r7		# pop it
	tstl r0			# is TOS zero?
	beql ZBranch_Branch	# If so, go
ZBranch_Skip:
	tstl (r10)+		# Skip the offset
	jmp (r11)		# Next word

 #	(loop): -- loop control
 #
 #	Assumes the return stack contains:
 #	 (sp) = index
 #	4(sp) = limit
 #
 #	This word does the end of a do loop; it updates the index and
 #	compares it to the limit to see if the loop should terminate. The
 #	index and limit are store on the return stack instead of the 
 #	math stack so that the math stack is available for mutilation.
 #
 #	The word following the opcode contains a relative index that will
 #	be added to the IP (R10) if the loop is to be continued; that is,
 #	the word following the opcode contains the relative offset to the
 #	top of the loop.
 #
 #	If the loop is not to be continued, the relative index is skipped.
 #
 #	The code decides to branch based on the index being less than the
 #	limit instead of deciding to skip if the index is the same as the
 #	limit to allow LEAVE to force the loop to exit by setting the index
 #	equal to the limit; since the code increments the index first,
 #	doing it the other way would cause an infinite loop if you decided
 #	to LEAVE.
 #
 #	The label PLoop_1 is used by (+LOOP) so that it doesn't have to
 #	duplicate the test for the end of the index.

	code "PLoop","(LOOP)","ZBranch"
	incl (sp)		# Update index
PLoop_1:
	cmpl (sp),4(sp)		# Still need to loop?
	blssu ZBranch_Branch	# If not, branch to top of loop

	tstl (sp)+		# Discard index
	tstl (sp)+		# Discard limit
	brb ZBranch_Skip	# Skip offset AND do next word

 #	(+loop): -- almost the same as (loop)
 #
 #	Assumes the return stack contains:
 #	 (sp) = index
 #	4(sp) = limit
 #
 #	This procedure adds the value on top of the math stack to the
 #	loop index and then does the rest of (LOOP); it's like (LOOP)
 #	except you can specify an increment other than one.
 #
 #	The increment is discarded from the math stack.
 #
 #	Like (LOOP), (+LOOP) is followed by a word containing the relative
 #	offset to the top of the loop.

	code "PPLoop","(+LOOP)","PLoop"
	addl2 (r7),(sp)		# Add increment to index
	addb2 $4,r7		# discard increment
	brb PLoop_1		# Check for loop completion

 #	(do): limit init -- [pushed to rstack]
 #
 #	This procedure is the run-time initialization of a do loop; it takes
 #	the limit and initial value from the math stack and transfers them
 #	to the return stack to make the return stack look like what (loop)
 #	and (+loop) expect it to look like.
 #
 #	$$$

	code "PDo","(DO)","PPLoop"
	movl (r7),r0		# fetch init
	addb2 $4,r7		# discard init and point at limit
	movl (r7),-(sp)		# push limit
	movl r0,-(sp)		# push init
	addb2 $4,r7		# discard limit
	jmp (r11)

 #	: DO
 #	  COMPILE (DO) HERE LIT 3
 #	;*
 #
 #	This word compiles the start of a do loop into the code. It adds
 #	the address of (DO) into the word and copies the address of the
 #	start of the code inside the DO loop onto the math stack. This
 #	will be used later by LOOP and +LOOP.
 #
 #	The 3 marks the stuff on the math stack as belonging to a DO loop.

	colon "DO","DO","PDo",Immediate
	offset COMPILE		# Append (DO) to the current word
	offset PDo
	offset HERE		# Remember address of top of loop
	literal 3		# and that we're doing a DO loop
	offset SemiS

 #	: LOOP
 #	  LIT 3 ?PAIRS COMPILE (LOOP) O,
 #	;*
 #
 #	This word compiles the end of a DO loop for the standard increment
 #	of one. If we're currently compiling a DO loop, it appends 
 #	(LOOP) followed by the offset to the top of the loop to current
 #	word.
 #
 #	$$$

	colon "LOOP","LOOP","DO",Immediate
	literal 3		# Complain if we're not doing a DO loop
	offset QPairs
	offset COMPILE		# Append (LOOP) to the word
	offset PLoop
	offset OComma		# and the offset to the top of the loop
	offset SemiS

 #	: +LOOP
 #	  LIT 3 ?PAIRS COMPILE (+LOOP) O,
 #	;*
 #
 #	This word compiles the end of a DO loop when an increment other than
 #	one is needed. If we're currently compiling a DO loop, (+LOOP) is
 #	appended to the word followed by the offset to the top of the 
 #	loop.
 #
 #	$$$

	colon "PlusLoop","+LOOP","LOOP",Immediate
	literal 3		# Complain if we're not doing a DO loop
	offset QPairs
	offset COMPILE		# Append (+LOOP) to the word
	offset PPLoop
	offset OComma		# and the offset to the top of the loop
	offset SemiS

 #	leave: set the index = the limit of a DO
 #
 #	Assumes:
 #	 (sp) = index
 #	4(sp) = limit
 #
 #	This procedure updates the return stack such that the next
 #	(LOOP) or (+LOOP) will cause the loop to exit. It does this by
 #	clearing the limit so that the index cannot be less than the
 #	limit next time it is checked ( (LOOP) and (+LOOP) use an
 #	unsigned comparison).

	code "LEAVE","LEAVE","PlusLoop"
	clrl 4(sp)		# Clear limit
	jmp (r11)

 #	I: Copy current do loop limit to math stack
 #
 #	This word is used by the innards of a DO loop to examine the
 #	index. Since the index is the top item of the return stack, the
 #	top of the return stack is pushed onto the math stack.
 #
 #	Since R is essentially a synonym for I, it enters at RI1.

	code "I","I","LEAVE"
RI1:	subb2 $4,r7		# make space for copy of return stack
	movl (sp),(r7)		# put it on the math stack.
	jmp (r11)

 #	>R: a -- Push onto return stack
 #
 #	This word pushes the item currently on the top of the math stack
 #	onto the return stack.

	code "ToR",">R","I"
	movl (r7),-(sp)		# push top of math stack on return stack
	addb2 $4,r7		# discard top of math stack.
	jmp (r11)

 #	<R: -- a <pop from return stack>
 #
 #	This word pushes the item currently on top of the return stack
 #	onto the math stack.

	code "FromR","R>","ToR"
	subb2 $4,r7		# make space on math stack
	movl (sp)+,(r7)		# copy top of return stack over & discard it
	jmp (r11)


 #	R: Copy top of return stack to math stack
 #
 #	This word is used when you want to retrieve an item from the
 #	top of the return stack without modifying the return stack; the
 #	return stack is used as a convenient place to stash local variables.
 #
 #	Since this is essentially a synonym for I, it hops into the middle
 #	of the code for I.

	code "R","R","FromR"
	brb RI1			# See I

 #	RP@: -- return stack pointer
 #
 #	This procedure returns the current value of the return stack pointer.
 #	Again, I'm not sure how useful this is since I'm not supporting 
 #	other things needed for FORTH multitasking.

	code "RPFetch","RP@","R"
	subb2 $4,r7		# make space on math stack
	movl sp,-(r7)		# push copy of return stack pointer
	jmp (r11)

 #	RP!: --  <Initialize return stack pointer>
 #
 #	This procedure restores the return stack to its original value. 
 #	This is used when aborting.
 #
 #	I'm assuming the original caller of the FORTH system initialized
 #	the stack pointer. This lets me debug under VMS without needing
 #	a large array to handle stack requirements of (e.g.) $QIO; the
 #	VMS code can simply pass in the stack pointer it got from VMS.
 #	The I/O routines for embedded apps are responsible for allocating
 #	stack space and initializing the stack pointer before starting
 #	FORTH.

	code "RPStore","RP!","RPFetch"
	movl __initial_sp,sp
	jmp (r11)

 #	SWAP: a b -- b a
 #
 #	This procedure swaps the two items at the top of the math stack.

	code "SWAP","SWAP","RPStore"
	movl (r7),r0		# r0 <- b
	addb2 $4,r7		# point at a
	movl (r7),r1		# r1 <- a
	movl r0,(r7)		# replace a with b
	subb2 $4,r7		# make space for a
	movl r1,(r7)		# put b on top
	jmp (r11)

 #	Rot: a b c -- b c a
 #
 #	This procedure shuffles the top three items on the math stack
 #	as indicated.
 #
 #	$$$

	colon "ROT","ROT","SWAP"
	offset ToR	# ( a b c -- a b )
	offset SWAP	#         -- b a )
	offset FromR	#         -- b a c )
	offset SWAP	#         -- b c a )
	offset SemiS

 #	over: a b -- a b a
 #
 #	This procedure duplicates the second item on the math stack,
 #	pushing a new copy on the top.

	code "OVER","OVER","ROT"
	movl r7,r0	# form address of a
	addb2 $4,r0
	subb2 $4,r7	# make space for the copy
	movl (r0),(r7)	# copy a to top of stack
	jmp (r11)

 #	dup: a - a a
 #
 #	This procudure duplicates the top item of the math stack.

	code "DUP","DUP","OVER"
	movl (r7),r0	# fetch top of math stack
	subb2 $4,r7	# make space for copy
	movl r0,(r7)	# do the deed
	jmp (r11)

 #	DROP: a --
 #
 #	Forgets the item on top of the math stack.

	code "DROP","DROP","DUP"
	addb2 $4,r7	# discard top of math stack
	jmp (r11)


 #	2dup: a b -- a b a b
 #
 #	This procedure duplicates a 64-bit value on top of the math stack.

	code "TwoDUP","2DUP","DROP"
	movl r7,r0	# save address of b
	subb2 $8,r7	# make space for a quadword
	movq (r0),(r7)	# copy it
	jmp (r11)

 #	SP@: -- math stack pointer
 #
 #	This procedure returns the current value of the math stack. I return
 #	the value before it's fetched; I'm not sure it's important. I'm
 #	also not sure what use it is since I'm not doing other stuff
 #	important to FORTH multitasking.

	code "SPFetch","SP@","TwoDUP"
	movl r7,r0		# save copy of math stack pointer
	subb2 $4,r7		# make space for the copy
	movl r0,(r7)		# put copy on math stack
	jmp (r11)

 #	sp!: -- <math stack pointer is loaded with original value>
 #
 #	This procedure initializes the math stack pointer to its default
 #	value; i.e., the top of the space reserved for the math stack.
 #	This is used when aborting.

	code "SPStore","SP!","SPFetch"
	moval MathSpace,r7
	jmp (r11)

 #	: -DUP		( V -- V | V V <DUPLICATE IF V != 0 > )
 #	  DUP
 #	  0BRANCH DDUP1 ( SKIP TO END IF IT WAS ZERO )
 #	  DUP
 #	LABEL DDUP1
 #	;
 #
 #	This word duplicates the TOS if it is not zero. If the TOS is
 #	zero, it is not duplicated.

	colon "MinusDup","-DUP","SPStore"
	offset DUP
	offset ZBranch
	offset DDup1
	offset DUP
DDup1:	offset SemiS

 #	: !CSP		( SAVE CSP AT USER VAR CSP )
 #	  SP@ CSP ! ;
 #
 #	This word stores a copy of the current math stack pointer in 
 #	the variable CSP. After compilation, the compiler checks CSP
 #	against the math stack pointer to see if it's managed to pull
 #	everything off the stack.

	colon "StoreCSP","!CSP","MinusDup"
	offset SPFetch
	offset CSP
	offset Store
	offset SemiS

 #	-: a b -- a-b
 #
 #	This procedure subtracts the top item of the math stack from the
 #	next item on the math stack, returning the difference.

	code "Subtract","-","StoreCSP"
	movl r7,r0		# form address of a
	addb2 $4,r0
	subl2 (r7),(r0)		# do the subtract
	movl r0,r7		# discard b
	jmp (r11)

 #	<: a b -- a<b
 #
 #	This code pushes 1 if the second number on the math stack is
 #	less than the number on top of the math stack and 0 otherwise.
 #	This seems to me to be backwards, so I checked FigFORTH for the
 #	VAX, which gives this definition:
 #
 #	: < - 0< ;
 #
 #	which works out the same way; the 0< will push one only if the
 #	result of the subtraction is negative, which will only be the case
 #	if b>a.
 #
 #	Hmm. If I get desparate for space, I could use the FigFORTH version;
 #	looks like it's smaller.
 #
 #	$$$

	colon "Less","<","Subtract"
	offset Subtract
	offset ZLess
	offset SemiS

 #	AND: a b -- a&b
 #
 #	The top two words on the math stack are anded. Since we don't have
 #	an and instruction, we have to complement b and then BICL.
 #
 #	I don't want a BICL
 #	I just wanna ride my motorSICL
 #	And I don't wanna die
 #	I just wanna ride my motorcy
 #	cle				-- Apologies to Arlo Guthrie

	code "AND","AND","Less"
	movl r7,r0		# form address of a
	addb2 $4,r0
	mcoml (r7),(r7)		# Form one's complement of b
	bicl2 (r7),(r0)		# BICL it 
	movl r0,r7		# discard one's complement of b
	jmp (r11)

 #	OR: a b -- a or b
 #
 #	The top two items on the math stack are ORed and the result stored
 #	on the math stack.

	code "OR","OR","AND"
	movl r7,r0		# form address of a
	addb2 $4,r0
	bisl2 (r7),(r0)		# Do it
	movl r0,r7		# discard b
	jmp (r11)

 #	XOR: a b -- a xor b
 #
 #	The top two items on the math stack are XORed and the result stored
 #	on the math stack.

	code "XOR","XOR","OR"
	movl r7,r0		# form address of a
	addb2 $4,r0
	xorl2 (r7),(r0)		# do the XOR
	movl r0,r7		# discard b
	jmp (r11)

 #	+!: val addr -- <add val to mem[addr]>
 #
 #	This procedure adds the second value on the math stack to the
 #	address specified on top of the math stack.

	code "PlusStore","+!","XOR"
	movl r7,r0		# form address of val
	addb2 $4,r0
	addl2 (r0),*(r7)	# Add val to addr
	addb2 $8,r7		# discard addr and val
	jmp (r11)

 #	toggle: addr bits -- <xor mem[addr] with bits, store in mem[addr]>
 #
 #	This procedure toggles the bits specified on top of the math stack
 #	in the longword addressed by the second item on the math stack.
 #	Oddly, the ordering of operands in this operation is not the FORTH
 #	norm of making it difficult to do in a single instruction...

	code "TOGGLE","TOGGLE","PlusStore"
	movl r7,r0		# form address of addr
	addb2 $4,r0
	xorl2 (r7),*(r0)	# toggle the bits
	addb2 $8,r7		# discard bits and addr
	jmp (r11)

 #	0=: a -- (a==0)
 #
 #	Pushes a 1 on the stack if a is zero, and a zero otherwise.
 #
 #	The bits of code used here to push the one and zero are used
 #	by other comparisons.
 #
 #	$$$

	code "ZEq","0=","TOGGLE"
	tstl (r7)
	beql Yes

No:	clrl (r7)		# It's false. Push a zero.
	jmp (r11)

Yes:	movzbl $1,(r7)		# It's true. Push a one.
	jmp (r11)

 #	0< a -- (a < 0)
 #
 #	Pushes a 1 if a is negative, a 0 otherwise.

	code "ZLess","0<","ZEq"
	tstl (r7)
	blss Yes
	brb No

 #	=: a b -- (a=b)
 #
 #	Pushes a one if a and b are equal, a zero otherwise.
 #
 #	$$$

	code "Equal","=","ZLess"

				# have to fetch operands so we can
				# diddle stack pointer without
				# disturbing flags. don't want to just
				# point r0 at top of stack because that
				# precludes pre-emption (which might
				# rely on r7 always pointing at top of
				# math stack)

	movl r7,r0		# form addr of a
	addb2 $4,r0
	movl (r0),r1		# fetch a
	movl (r7),r2		# fetch b
	movl r0,r7		# discard b
	cmpl r1,r2		# compare 'em
	beql Yes
	brb No

 #	!=: a b -- (a!=b)
 #
 #	Pushes a one if a and b are not equal, a zero otherwise.
 #
 #	$$$

	code NotEq,"!=","Equal"

				# have to fetch operands so we can
				# diddle stack pointer without
				# disturbing flags. don't want to just
				# point r0 at top of stack because that
				# precludes pre-emption (which might
				# rely on r7 always pointing at top of
				# math stack)

	movl r7,r0		# form addr of a
	addb2 $4,r0
	movl (r0),r1		# fetch a
	movl (r7),r2		# fetch b
	movl r0,r7		# discard b
	cmpl r1,r2		# compare 'em
	beql No
	brb Yes

 #	u<: a b -- (a<b, unsigned)
 #
 #	Pushes a one if a is less than b unsignedly.
 #
 #	$$$

	code "ULess","U<","NotEq"

				# have to fetch operands so we can
				# diddle stack pointer without
				# disturbing flags. don't want to just
				# point r0 at top of stack because that
				# precludes pre-emption (which might
				# rely on r7 always pointing at top of
				# math stack)

	movl r7,r0		# form addr of a
	addb2 $4,r0
	movl (r0),r1		# fetch a
	movl (r7),r2		# fetch b
	movl r0,r7		# discard b
	cmpl r1,r2		# compare 'em
	blssu Yes		# If A is less, push 1
	brb No			# Otherwise, push 0

 #	: >	( CHEAP TRICK )  ( a b -- a>b )
 #	  SWAP < ;

	colon "Greater",">","ULess"
	offset SWAP
	offset Less
	offset SemiS

 #	: <>	( NOT-EQUAL )
 #	  != ;
 #
 #	The C-Forth guy used C syntax for the machine code version of !=,
 #	probably to save himself lots-o'-headaches for his fingers trained
 #	on C. I'm not quite so attached, but I'm not gonna rip it out either.

	colon "NotEqual","<>","Greater"
	offset NotEq
	offset SemiS

 #	+: a b -- a + b
 #
 #	Adds the numbers on top of the math stack.

	code "Plus","+","NotEqual"
	movl r7,r0		# form address of a
	addb2 $4,r0
	addl2 (r7),(r0)		# add 'em
	movl r0,r7		# discard b
	jmp (r11)

 #	MINUS: a -- -a
 #
 #	Forms the two's complement of the number on top of the math stack.

	code "MINUS","MINUS","Plus"
	mnegl (r7),(r7)
	jmp (r11)

 #	NOT: a -- ~a
 #
 #	Forms the one's complement of the number on top of the math stack.
 #
 #	$$$

	code "NOT","NOT","MINUS"
	mcoml (r7),(r7)
	jmp (r11)

 #	: 1+ 1 + ;	( val -- val+1 )
 #
 #	Shorthand word for incrementing top of stack.

	colon "OnePlus","1+","NOT"
	offset One		# Increment
	offset Plus
	offset SemiS

 #	: 2+ 2 + ;	( val -- val+2 )
 #
 #	Shorthand word for incrementing by two. Since I'm a 32-bit
 #	implementation, I should probably include 4+ and perhaps even
 #	8+, but I'm not gonna...

	colon "TwoPlus","2+","OnePlus"
	offset Two
	offset Plus
	offset SemiS

 #	: 1- 1 - ;	( val -- val-1 )
 #
 #	Shorthand to decrement top of stack

	colon "OneMinus","1-","TwoPlus"
	offset One
	offset Subtract
	offset SemiS

 #	: +-		( a b -- c )
 #	  0<
 #	  0BRANCH PM1
 #	  MINUS
 #	LABEL PM1
 #	;
 #
 #	This procedure applies the sign of b to a, generating c. If b is
 #	positive, a is not changed. If b is negative, a is multiplied by
 #	-1.

	colon "PlusMinus","+-","OneMinus"
	offset ZLess
	offset ZBranch
	offset PM1
	offset MINUS
PM1:	offset SemiS

 #	: ABS		( a -- b )
 #	  DUP +-
 #	;
 #
 #	This word takes the absolute value of the top of stack.

	colon "ABS","ABS","PlusMinus"
	offset DUP
	offset PlusMinus
	offset SemiS

 #	: MIN		( a b -- c )
 #	  2DUP >
 #	  0BRANCH MIN1
 #	  SWAP
 #	LABEL MIN1
 #	  DROP
 #	;
 #
 #	This word returns the smaller of the two items on top of the math
 #	stack.

	colon "MIN","MIN","ABS"
	offset TwoDUP
	offset Greater
	offset ZBranch
	offset Min1
	offset SWAP
Min1:	offset DROP
	offset SemiS

 #	: MAX		( a b -- c )
 #	  2DUP <
 #	  0BRANCH MAX1
 #	  SWAP
 #	LABEL MAX1
 #	  DROP
 #	;
 #
 #	This word returns the larger of the two items on top of the math stack.
 #
 #	$$$

	colon "MAX","MAX","MIN"
	offset TwoDUP
	offset Less
	offset ZBranch
	offset Max1
	offset SWAP
Max1:	offset DROP
	offset SemiS

 #	: *		( a b -- c )
 #	  M* DROP
 #	;
 #
 #	This word multiplies the two single-precision numbers on top of the
 #	math stack to give a single-precision result.

	colon "Star","*","MAX"
	offset MStar
	offset DROP
	offset SemiS

 #	: /MOD		( a b -- rem quo )
 #	  >R S->D R> M/
 #	;
 #
 #	This word divides the single-precision number a by the single-
 #	precision number b and returns both the remainder and the quotient.

	colon "SlashMod","/MOD","Star"
	offset ToR
	offset SignExtend
	offset FromR
	offset MSlash
	offset SemiS

 #	: /		( a b -- c )
 #	  /MOD SWAP DROP
 #	;
 #
 #	This word divides a by b, returning only the quotient as c.

	colon "Slash","/","SlashMod"
	offset SlashMod
	offset SWAP
	offset DROP
	offset SemiS

 #	: MOD		( a b -- c )
 #	  /MOD DROP
 #	;
 #
 #	This word divides a by b, returning only the remainder as c.

	colon "MOD","MOD","Slash"
	offset SlashMod
	offset DROP
	offset SemiS

 #	: */MOD		( a b c -- rem quo )
 #	  >R M* R> M/
 #	;
 #
 #	This word multiplies a by b and then divides the result by c, 
 #	returning both the remainder and the quotient of the division.
 #	The intermediate result is a double-precision number for that
 #	extra precise precision.

	colon "StarSlashMod","*/MOD","MOD"
	offset ToR
	offset MStar
	offset FromR
	offset MSlash
	offset SemiS

 #	: */			( a b c -- quo )
 #	  */MOD
 #	  SWAP DROP
 #	;
 #
 #	This word multiples a and b and then divides that result by c,
 #	returning only the quotient. The intermediate result is 
 #	double-precision.

	colon "StarSlash","*/","StarSlashMod"
	offset StarSlashMod
	offset SWAP
	offset DROP
	offset SemiS

 #	: <<	( val count -- val<<count )
 #
 #	Shifts the value left by the specified count

	code "ShiftLeft","<<","StarSlash"
	movl r7,r0		# form address of value
	addb2 $4,r0
	ashl (r7),(r0),(r0)	# do the shift, replacing value
	movl r0,r7		# discard count
	jmp (r11)

 #	: >>	( val count -- val>>count )
 #
 #	Shifts the value right by the specified count

	colon "ShiftRight",">>","ShiftLeft"
	offset MINUS		# convert to left shift count
	offset ShiftLeft	# do the shift
	offset SemiS

 #	D+: alo ahi blo bhi -- a+blo a+bhi <double add>
 #
 #	This procedure does a 64-bit add of the top two elements of the
 #	math stack, returning a 64-bit result.

	code "DPlus","D+","ShiftRight"

	movl r7,r0		# form address of blo
	addb2 $4,r0	

	movl r7,r1		# form address of ahi
	addb2 $8,r1

	movl r7,r2		# form address of alo
	addb2 $12,r2

	addl2 (r0),(r2)		# add blo to alo
	adwc (r7),(r1)		# add ahi to bhi w/ carry

	addb2 $8,r7		# discard bhi and blo

	jmp (r11)

 #	D-: alo ahi blo bhi -- a-blo a-bhi <double subtract>
 #
 #	This procedure subtracts the 64-bit number on top of the math stack
 #	from the 64-bit number in the next position on the math stack. The
 #	result is returned on the math stack.

	code "DSubtract","D-","DPlus"

	movl r7,r0		# form address of blo
	addb2 $4,r0	

	movl r7,r1		# form address of ahi
	addb2 $8,r1

	movl r7,r2		# form address of alo
	addb2 $12,r2

	subl2 (r0),(r2)		# subtract blo from alo
	sbwc (r7),(r1)		# subtract bhi from ahi w/ borrow

	addb2 $8,r7		# discard bhi and blo

	jmp (r11)

 #	DMINUS: alo ahi - -alo -ahi <double complement>
 #
 #	This procedure forms the two's complement of the 64-bit number on
 #	top of the math stack. The result is stored on the math stack.
 #	The two's complement is formed by subtracting the 64-bit number
 #	from zero.

	code "DMINUS","DMINUS","DSubtract"

	movl r7,r0		# form address of alo
	addb2 $4,r0

	clrl r1			# we'll collect borrow here
				# (VAX doesn't have 3-operand subtract
				# with carry)

	subl3 (r0),$0,(r0)	# subtract low word
	sbwc $0,r1		# propagate the borrow
	subl3 (r7),r1,(r7)	# complement high word

	jmp (r11)

 #	: D+-		( a.lo a.hi b -- c.lo c.hi )
 #	  0<
 #	  0BRANCH DPM1
 #	  DMINUS
 #	LABEL DPM1
 #	;
 #
 #	This word applies the sign of b to the double-precision number a
 #	to generate the double-precision number c. If b is positive, a
 #	is unchanged. If b is negative, a is multiplied by -1.

	colon "DPlusMinus","D+-","DMINUS"
	offset ZLess
	offset ZBranch
	offset DPM1
	offset DMINUS
DPM1:	offset SemiS

 #	: DABS		( a.lo a.hi -- b.lo b.hi )
 #	  DUP D+-
 #	;
 #
 #	This word takes the absolute value of the double-precision number
 #	on top of the math stack.

	colon "DABS","DABS","DPlusMinus"
	offset DUP
	offset DPlusMinus
	offset SemiS

 #	U*: a b -- a*b.lo a*b.hi
 #
 #	This is an unsigned double-length multiply. In this case,
 #	double-length is 64 bits, but we won't be picky.

	code "UStar","U*","DABS"

	movl r7,r0		# form address of a
	addb2 $4,r0

	emul (r7),(r0),$0,r2	# do it

	movl r2,(r0)		# save low-order result
	movl r3,(r7)		# save high-order result

	jmp (r11)

 #	U/: num.lo num.hi denom -- rem quot
 #
 #	This is a generalized unsigned divide of a double-length number
 #	by a single-length number. Other words use this routine to do
 #	their work and then pick the results they want.

	code "USlash","U/","UStar"

	movl r7,r0		# form address of num.lo
	addb2 $8,r0

	movl r7,r1		# form address of num.hi
	addb2 $4,r1

	movl (r0),r2		# fetch numerator
	movl (r1),r3

	ediv (r7),r2,r3,r4	# do the divide

	addb2 $4,r7		# discard quot

	movl r3,(r7)		# replace num.hi with quotient
	movl r4,(r0)		# replace num.lo with remainder

	jmp (r11)

 #	: S->D	( a -- a.lo a.hi )
 #
 #	This word sign-extends a single-precision number into a 
 #	double-precision number.
 #
 #	$$$

	code "SignExtend","S->D","USlash"
	clrl r0			# assume a is positive
	tstl (r7)		# are we right?
	bgeq S2D1		# if so, go
	decl r0			# If not, turn the 0 into a -1
S2D1:	subb2 $4,r7		# Make space for the ms longword
	movl r0,(r7)		# copy it to math stack
	jmp (r11)

 #	: M*		( a b -- c.lo c.hi )
 #	  2DUP XOR >R ABS SWAP ABS U* R> D+-
 #	;
 #
 #	This word multiplies the two numbers on top of the math stack to
 #	give a double-precision result.

	colon "MStar","M*","SignExtend"
	offset TwoDUP
	offset XOR
	offset ToR
	offset ABS
	offset SWAP
	offset ABS
	offset UStar
	offset FromR
	offset DPlusMinus
	offset SemiS

 #	: M/		( a.lo a.hi b -- rem quo )
 #	  OVER >R >R DABS R ABS U/
 #	  R> R XOR +- SWAP
 #	  R> +- SWAP
 #	;
 #
 #	This word divids the double-precision number a by the single-
 #	precision number b, returning both the remainder and the quotient.

	colon "MSlash","M/","MStar"
	offset OVER
	offset ToR
	offset ToR
	offset DABS
	offset R
	offset ABS
	offset USlash
	offset FromR
	offset R
	offset XOR
	offset PlusMinus
	offset SWAP
	offset FromR
	offset PlusMinus
	offset SWAP
	offset SemiS

 #	: M/MOD		( ud1.hi ud1.lo u2 -- remainder quo.hi quo.lo )
 #	  >R 0 R	( -- ud1.hi ud1.lo 0 u2 )
 #	  U/ 		( -- ud1.hi rem1 quot1 )
 #	  R> SWAP	( -- ud1.hi rem1 u2 quot1 ) 
 #	  >R		( -- ud1.hi rem1 u2 )
 #	  U/		( -- rem2 quot2 )
 #	  R>		( -- rem2 quot2 quot1 )
 #	;
 #
 #	This word divides a double-precision number by an unsigned single-
 #	precision number. 

	colon "MSlashMod","M/MOD","MSlash"
	offset ToR
	offset Zero
	offset R
	offset USlash
	offset FromR
	offset SWAP
	offset ToR
	offset USlash
	offset FromR
	offset SemiS

 #	@: addr -- mem[addr]
 #
 #	Fetches the longword at the specified address.

	code "Fetch","@","MSlashMod"
	movl *(r7),(r7)
	jmp (r11)

 #	C@: addr -- mem[addr], byte
 #
 #	Fetches the byte at the specified address. The byte is zero-extended.

	code "CFetch","C@","Fetch"
	movzbl *(r7),(r7)
	jmp (r11)

 #	2@: addr -- mem[addr+1] mem[addr]
 #
 #	This procedure fetches the quadword addressed by the top of the math
 #	stack and stores it on the math stack. I didn't use a quadword
 #	instruction because the words would be stacked in the wrong order.

	code "TFetch","2@","CFetch"
	movl r7,r0		# save addr
	movl 4(r0),(r7)		# put ms word on stack
	subb2 $4,r7		# make space for ls word
	movl (r0),(r7)
	jmp (r11)

 #	!: val addr -- <set mem[addr] = val>
 #
 #	This procedure stores a longword to the address stored at the top
 #	of the math stack. The value stored is the second item on the math
 #	stack.

	code "Store","!","TFetch"
	movl r7,r0		# form address of val
	addb2 $4,r0
	movl (r0),*(r7)		# store value
	addb2 $8,r7		# discard addr and val
	jmp (r11)

 #	C!: val addr -- <set mem[addr] = val as a byte>
 #
 #	This procedure stores a byte to the address stored at the top of
 #	the math stack. The value stored is the second item on the math
 #	stack.

	code "CStore","C!","Store"
	movl r7,r0		# form address of val
	addb2 $4,r0
	movb (r0),*(r7)		# store value
	addb2 $8,r7		# discard addr and val
	jmp (r11)

 #	2!: val1 val2 addr -- <mem[addr] = val2, mem[addr+1] = val1>
 #
 #	This procedure stores a quadword from the math stack to memory.

	code "TStore","2!","CStore"
	movl (r7),r0		# fetch addr
	addb2 $4,r7		# form addr of val2, discarding addr
	movl (r7),(r0)		# copy ls longword
	addb2 $4,r7		# form addr of val1, discarding val2
	movl (r7),4(r0)		# copy ms longword
	addb2 $4,r7		# discard val1
	jmp (r11)

 #	: ++		( ADDR -- <INCREMENTS VAL AT ADDR> )
 #	  1 SWAP +! ;
 #
 #	This is an extension from C-Forth. Looked useful to me, so I
 #	kept it.

	colon "PlusPlus","++","TStore"
	offset One
	offset SWAP
	offset PlusStore
	offset SemiS

 #	: --		( ADDR -- <DECREMENTS VAL AT ADDR> )
 #	  -1 SWAP +! ;
 #
 #	This is an extension from C-Forth. Looked useful to me, so I
 #	kept it.

	colon "MinusMinus","--","PlusPlus"
	literal -1
	offset SWAP
	offset PlusStore
	offset SemiS

 #	: CVTWL@	( addr -- sign-extended word )
 #			( fetch a sign-extended word )
 #
 #	This word fetchs a 16-bit word and sign-extends it to a 32-bit
 #	longword before pushing it on the math stack. This is needed for
 #	fetching relative offset from the code portion of a FORTH word,
 #	for example.

	code "CVTWLFetch","CVTWL@","MinusMinus"
	cvtwl *(r7),(r7)
	jmp (r11)

 #	: W!		( value address -- )
 #			store a word
 #
 #	This word stores a 16-bit word at the specified address.

	code "WStore","W!","CVTWLFetch"
	movl (r7),r0	# fetch addr
	addb2 $4,r7	# form address of value and discard addr
	movw (r7),(r0)	# store word
	addb2 $4,r7	# discard value
	jmp (r11)

 #	cmove: source dest number --
 #
 #	This procedure moves number bytes from source to dest. MOVC3
 #	won't work because the regions might overlap and MOVC3 is too
 #	highly optimized for that (perhaps our minds are _too_ highly
 #	trained, Vroomfondel).

	code "CMOVE","CMOVE","WStore"

	movl (r7),r0		# r0 <- number
	addb2 $4,r7		# discard number
	movl (r7),r1		# r1 <- dest
	addb2 $4,r7		# discard dest
	movl (r7),r2		# r2 <- source
	addb2 $4,r7		# discard source

CMove1:	movb (r2)+,(r1)+	# mov a byte
	sobgtr r0,CMove1	# lather, rinse, repeat

	jmp (r11)

 #	: FILL		( START COUNT VALUE -- <FILL COUNT BYTES, FROM
 #			  START, WITH VALUE )
 #
 #	This word fills memory starting at START with the VALUE. Memory
 #	is filled as bytes, so if VALUE is bigger than a byte it's
 #	truncated during the storage process. COUNT bytes are filled.
 #	If COUNT is 0, no bytes are filled.
 #
 #	$$$

	code "FILL","FILL","CMOVE"

	movl (r7),r0		# r0 <- value
	addb2 $4,r7		# discard value
	movl (r7),r1		# r1 <- count
	addb2 $4,r7		# discard count
	movl (r7),r2		# r2 <- start
	addb2 $4,r7		# discard start

	tstl r1			# Did we get a zero count?
	beql Fill1		# If so, go

Fill0:	movb r0,(r2)+		# store a byte
	sobgtr r1,Fill0		# lather, rinse, repeat

Fill1: jmp (r11)		# Return to caller

 #	: ERASE		( START COUNT -- <ZERO OUT MEMORY> )
 #	  0 FILL
 #	;
 #
 #	This word fills a chunk of memory with zeros.

	colon "ERASE","ERASE","FILL"
	offset Zero
	offset FILL
	offset SemiS

 #	: BLANKS	( START COUNT -- <FILL WITH BLANKS> )
 #	  BL FILL
 #	;
 #
 #	This word fills a chunk of memory with blanks.

	colon "BLANKS","BLANKS","ERASE"
	offset BL		# start count 32
	offset FILL
	offset SemiS

 #	LIT: Push word following onto math stack
 #
 #	The opcode is followed by a longword. This longword is fetched
 #	via IP and pushed onto the math stack. The longword is then skipped.

	code "LIT","LIT","BLANKS"
	subb2 $4,r7		# make space on math stack
	movl (r10)+,(r7)	# Push the literal and update IP.
	jmp (r11)

 #	ZBLIT: Push byte following onto math stack
 #
 #	The opcode is followed by a byte. This byte is fetched via IP,
 #	zero-extended, and pushed on the math stack. The byte is then
 #	skipped.

	code "ZBLIT","ZBLIT","LIT"
	subb2 $4,r7		# make space on math stack
	movzbl (r10)+,(r7)	# Push the literal and update IP.
	jmp (r11)

 #	OLIT: Push word referred to by following offset onto math stack
 #
 #	This opcode is followed by a relative offset. The address to
 #	which that offset refers is pushed on the math stack.

	code "OLIT","OLIT","ZBLIT"
	subb2 $4,r7		# make space on math stack
	addl3 r10,(r10)+,(r7)	# Relocate and push the offset.
	jmp (r11)

 #	: LITERAL
 #	  STATE @
 #	  0BRANCH L1
 #	  COMPILE LIT ,
 #	LABEL L1
 #	;*
 #
 #	This word compiles a single-precision literal into the current
 #	word if we're compiling. If we're not compiling, it's a NOP so you
 #	can say LITERAL 0 at the OK prompt and not get an error.
 #
 #	This word is not smart enough to optimize byte-sized literals into
 #	a reference to ZBLIT followed by the byte. Sigh; maybe sometime,
 #	although I doubt this code will survive much beyond the NV5 <-> APECS
 #	demo.

	colon "LITERAL","LITERAL","OLIT",Immediate
	offset STATE		# Are we compiling?
	offset Fetch
	offset ZBranch		# If not, go
	offset L1
	offset COMPILE		# Otherwise, do the LIT thing
	offset LIT
	offset Comma
L1:	offset SemiS

 #	: DLITERAL
 #	  STATE @
 #	  0BRANCH D1
 #	  SWAP LITERAL LITERAL
 #	LABEL D1
 #	;*
 #
 #	This procedure compiles a double-precision literal into the code, if
 #	we're compiling. If not compiling, it's a NOP; this may be viewed as
 #	a bug or a feature (typing DLITERAL 0 at the OK prompt does _not_
 #	put a double-precision 0 on the math stack).

	colon "DLITERAL","DLITERAL","LITERAL"
	offset STATE		# Are we compiling?
	offset Fetch
	offset ZBranch		# If not, go.
	offset D1
	offset SWAP		# We need to push second on stack first.
	offset LITERAL		# Do a single-precision literal for second.
	offset LITERAL		# Do a single-precision literal for first.
D1:	offset SemiS

 #	: HERE		( -- DP )
 #	  DP @ ;
 #
 #	This word returns the current end of the dictionary.

	colon "HERE","HERE","DLITERAL"
	offset DP
	offset Fetch
	offset SemiS

 #	: allot			( n -- )
 #	  here + dp !		( allocate specified number of bytes from
 #				  the directory )
 #	;
 #
 #	This word allocates space at the end of the dictionary. It adds
 #	the specified number to the dictionary pointer, advancing the
 #	dictionary pointer past the specified number of bytes.

	colon "ALLOT","ALLOT","HERE"
	offset HERE
	offset Plus
	offset DP
	offset Store
	offset SemiS

 #	: ,		( V -- <PLACES V AT DP AND INCREMENTS DP>)
 #	  HERE !
 #	  4 ALLOT ;
 #
 #	This word compiles a longword into the dictionary by storing
 #	the longword at here and advancing the pointer over it.

	colon "Comma",",","ALLOT"
	offset HERE	# Store the word at the end of the dictionary
	offset Store
	literal 4	# Advance the end of the dictionary over it
	offset ALLOT
	offset SemiS

 #	: w,		( w -- <places word at dp and increments dp> )
 #	  here w!
 #	  2 allot ;
 #
 #	This word is needed since I'm using 16-bit offsets for all of my
 #	threads. It stores a word at the end of the dictionary and advances
 #	the dictionary over it.

	colon "WComma","W,","Comma"
	offset HERE	# Store the 16-bit word
	offset WStore
	offset Two	# Advance the dictionary past it
	offset ALLOT
	offset SemiS

 #	: C,	( C -- <COMPILE A CHARACTER. SAME AS , WHEN WORDSIZE = 1 > )
 #	  HERE C!
 #	  1 ALLOT ;
 #
 #	This word compiles a byte into the dictionary; it stores the byte
 #	at the end of the dictionary and advances the dictionary past it.

	colon "CComma","C,","WComma"
	offset HERE	# Store the byte
	offset CStore
	offset One	# Advance the dictionary past it
	offset ALLOT
	offset SemiS

 #	: o,			( n -- )
 #	  here - ,
 #	;
 #
 #	This word compiles a relative offset INto the dictionary; that is,
 #	it compiles a word offset to the specified address.

	colon "OComma","O,","CComma"
	offset HERE
	offset Subtract
	offset Comma
	offset SemiS

 #	key: -- c <get next char from input>
 #
 #	This word waits for a character to be typed at the console and
 #	returns that character.

	code "KEY","KEY","OComma"
	bsbw ConIn		# Get the character
	subb2 $4,r7		# make space on math stack
	movzbl r0,(r7)		# Push it
	jmp (r11)

 #	?key: -- 0 ( no character available )
 #	         non-zero ( character available )
 #
 #	This word checks to see if a character is available at the
 #	console.

	code "QKey","?KEY","KEY"
	bsbw ConSt		# Check status
	subb2 $4,r7		# make space on math stack
	movzbl r0,(r7)		# Push it
	jmp (r11)

 #	emit: c -- <put char to output>
 #
 #	This word displays a character on the console.
 #
 #	$$$

	code "EMIT","EMIT","QKey"
	movzbl (r7),r0		# Get the character
	addb2 $4,r7		# discard character
	bsbw FConOut		# Print it
	jmp (r11)

invis:	.byte 0

 #	FConOut: routine to conditionally display info on the console;
 #
 #	This routine is a wrapper for the BIOS ConOut that allows console
 #	display to be inhibitted during download from a bit-banged serial
 #	port (such as the SROM port of the NV5). If output is invisible,
 #	ConOut is not called. If output is not invisible, ConOut is called.

FConOut:
	tstb invis
	beql fconout1
	brw ConOut
fconout1: rsb

 #	.off
 #
 #	This word turns EMIT off so that it won't print anything. This is
 #	needed when downloading code through the bit-banged serial port so
 #	the NV5 can get ready for the next character without wasting time
 #	echoing the character.
 #
 #	$$$

	code "DotOff",".OFF","EMIT"
	clrb invis
	jmp (r11)

 #	.on
 #
 #	This word turns EMIT back on after it has been disabled by .off.
 #	Used to end a download sequence.
 #
 #	$$$

	code "DotOn",".ON","DotOff"
	movb $1,invis
	jmp (r11)

 #	?terminal: <see if op. interrupted <like w/^C>>
 #
 #	Since we can't check for ^C on the bit-banged console port of
 #	the NV5, and we don't know what to do with keys that come in
 #	which aren't ^Cs anyway, I'm stubbing it.
 #
 #	Beware the infinite loop...

	code "QTerminal","?TERMINAL","DotOn"
	subb2 $4,r7		# make space on math stack
	clrl (r7)		# claim not interrupted
	jmp (r11)

 #	: CR			( -- )
 #	  LIT 13 EMIT
 #	  LIT 10 EMIT
 #
 #	This word advances the cursor to the next line by displaying a
 #	carriage return/linefeed sequence.
 #
 #	$$$

	colon "CR","CR","QTerminal"
	literal 13		# Display the carriage return
	offset EMIT
	literal 10
	offset EMIT		# Display the linefeed
	offset SemiS

 #	: SPACE ( EMIT A SPACE )
 #	  BL EMIT ;
 #
 #	Display a space on the screen.

	colon "SPACE","SPACE","CR"
	offset BL
	offset EMIT
	offset SemiS

 #	: HEX		( GO TO HEXADECIMAL BASE )
 #	  LIT 0x10 BASE ! ;
 #
 #	This word causes the system to use hexadecimal for all numeric
 #	I/O.

	colon "HEX","HEX","SPACE"
	literal 0x10
	offset BASE
	offset Store
	offset SemiS

 #	: DECIMAL	( GO TO DECIMAL BASE )
 #	  LIT 0x0A BASE !
 #	;
 #
 #	This word causes the system to use decimal for all numeric I/O.

	colon "DECIMAL","DECIMAL","HEX"
	literal 10
	offset BASE
	offset Store
	offset SemiS

 #	digit: 		( c base -- 0 ) of c isn't a digit
 #			( c base -- v 1 ) if c is a digit
 #
 #	This word looks to see if the ASCII character c is a digit in
 #	the number base specified by base. If it is not a digit, 0 is
 #	pushed. If it is a digit, the numeric value of that digit is
 #	pushed followed by a 1.

	code "DIGIT","DIGIT","DECIMAL"
	movl (r7),r1		# Get the base
	addb2 $4,r7		# discard base
	movl (r7),r0		# Get the character
	addb2 $4,r7		# dscard character

	cmpl $0x30,r0		# Is it smaller than '0'?
	bgtru Digit_Nope	# No, go
	cmpl $0x3a,r0		# Is it bigger than '9'?
	blssu Digit_Alpha

	subb2 $0x30,r0		# Convert it to a number (there won't
	brb Digit_Test		# be any borrows)

Digit_Alpha:
	bicb2 $0x20,r0		# Make certain it's uppercase
	cmpl $0x41-1,r0		# Is it too small to be an alphadigit?
	bgtru Digit_Nope	# Yes, go
	cmpl $0x5a,r0		# Is it too big to be an alphadigit?
	blssu Digit_Nope	# Yes, go

	subb2 $0x41-10,r0	# Make it 10 through 35
Digit_Test:
	cmpl r0,r1		# Check against the base
	bgtru Digit_Nope	# If too big, go

	subb2 $4,r7		# make space for value
	movl r0,(r7)		# It's a digit. Push the value
	subb2 $4,r7		# make space for flag
	movzbl $1,(r7)		# Push TRUE
	jmp (r11)

Digit_Nope:			# It's not a digit
	subb2 $4,r7		# make space for flag
	clrl (r7)		# Push FALSE
	jmp (r11)

 #	: TYPE		( addr n -- ) ( displays n bytes from addr )
 #
 #	This word displays n bytes starting at addr on the console. It
 #	depends on the BIOS not mutilating R5 and R6.
 #
 #	$$$

	code "TYPE","TYPE","DIGIT"
	movl (r7),r5		# Count to R5
	addb2 $4,r7		# discard count
	movl (r7),r6		# Address to R6
	addb2 $4,r7		# discard address
Type1:	movzbl (r6)+,r0		# Get the character & bump address
	bsbw FConOut		# Display the character
	sobgtr r5,Type1		# Repeat until done
	jmp (r11)

 #	: COUNT			( ADDR -- ADDR+1 COUNT )
 #
 #	This word fetches the byte at ADDR and increments ADDR. This takes
 #	a string of the form <count><body> (such as all of the in-line
 #	strings) and converts it to a form suitable for TYPE.
 #
 #	$$$

	code "COUNT","COUNT","TYPE"
	movzbl *(r7),r0		# Fetch count
	incl (r7)		# bump address
	subb2 $4,r7		# make space for count
	movl r0,(r7)		# push count
	jmp (r11)

 #	: (.")			( PRINT A COMPILED STRING )
 #	  R COUNT
 #	  DUP 1+ R> + >R TYPE
 #	;
 #
 #	This word is the run-time action of ."; it prints out a string
 #	embedded in the word.

	colon "PDotQuote","(.\")","COUNT"
	offset R		# return address
	offset COUNT		# ra+1 count
	offset DUP		# ra+1 count count
	offset OnePlus		# ra+1 count count+1
	offset FromR		# ra+1 count count+1 ra
	offset Plus		# ra+1 count ra+count+1 (skip string)
	offset ToR		# ra+1 count
	offset TYPE
	offset SemiS

 #	: ."			( COMPILE A STRING IF COMPILING,
 #				  OR PRINT A STRING IF INTERPRETING )
 #	  LIT '"'
 #	  STATE @
 #	  0BRANCH QUOTE1
 #	  COMPILE (.") WORD HERE C@ 1+ ALLOT	( IF )
 #	  BRANCH QUOTE2
 #	LABEL QUOTE1
 #	  WORD HERE COUNT TYPE			( ELSE )
 #	LABEL QUOTE2
 #	;*
 #
 #	This word behaves differently depending on whether the FORTH system
 #	is compiling or interpreting.
 #
 #	If the FORTH system is compiling, it compiles the string immediately
 #	following it into the current word after (."). When the word is 
 #	executed, the string will be printed.
 #
 #	If the FORTH system is not compiling, it prints out the string
 #	immediately following it in the terminal input buffer.

	colon "DotQuote",".\"","PDotQuote",Immediate
	literal 0x22	# quote
	offset STATE
	offset Fetch
	offset ZBranch
	offset Quote1
	offset COMPILE
	offset PDotQuote
	offset WORD
	offset HERE
	offset CFetch
	offset OnePlus
	offset ALLOT
	offset BRANCH
	offset Quote2
Quote1:	offset WORD
	offset HERE
	offset COUNT
	offset TYPE
Quote2:	offset SemiS

 #	: QUERY
 #	  TIB @		( ADDRESS OF BUFFER )
 #	  B/BUF @	( SIZE OF BUFFER )
 #	  EXPECT	( GET A LINE )
 #	  O IN !	( PREPARE FOR INTERPRET )
 #	;
 #
 #	This word reads a line from the console into the terminal input
 #	buffer. It also initializes IN, a variable used by the interpreter
 #	to keep track of its progress in the terminal input buffer.

#	colon "QUERY","QUERY","DotQuote"
#	offset TIB
#	offset Fetch
#	offset BSlashBuf
#	offset Fetch
#	offset EXPECT
#	offset Zero
#	offset IN
#	offset Store
#	offset SemiS

	colon "QUERY","QUERY","DotQuote"

	offset PTib	# Fetch a line via BDOS function 10
	offset Fetch
	literal 10
	offset BDOS
	offset DROP
	offset CR

	offset Zero	# -- 0
	offset PTib	# Drop a null at the end of the line.
	offset Fetch
	offset OnePlus  # -- 0, addr of length read
	offset CFetch	# -- 0, length read
	offset TIB
	offset Fetch
	offset Plus	# -- 0, TIB + length read
	offset CStore

	offset Zero	# Initialize IN
	offset IN
	offset Store

	offset SemiS

 #	enclose: addr c -- addr first last next
 #
 #	This word looks through memory starting at addr to find a word
 #	enclosed by the specified delimiter. The search ends either when
 #	the delimiter is found or a null is found. WORD uses this word
 #	to hack the TIB into pieces.
 #
 #	First is the offset from addr to the first character in the word;
 #		that is, the offset to the first non-delimiter character
 #	Last is the offset from addr to the character after the last
 #		character in the word; that is, to the delimiter following
 #		the word
 #	Next is the offset from addr to the character at which the search
 #		for the next word should begin; that is, the character
 #		after the delimiter that ended the word

	code "ENCLOSE","ENCLOSE","QUERY"

	movzbl (r7),r0		# delim = pop();
	addb2 $4,r7

	movl (r7),r1		# current = pop(); push( current );
	clrl r2			# offset = -1;
	decl r2
	decl r1			# current--;

encl1:
	incl r1			# current++;
	incl r2			# offset++;
	cmpb (r1),r0		# if( mem[current] == delim ) goto encl1;
	beql encl1

	subb2 $4,r7
	movl r2,(r7)		# push( offset );

	tstb (r1)		# if( mem[current] == NULL ) {
	bneq encl2
	subb2 $4,r7
	addl3 $1,r2,(r7)	#   offset++; push( offset ); offset--;
	subb2 $4,r7
	movl r2,(r7)		#   push( offset );
	jmp (r11)		#   return
				# }

encl2:
	incl r1			# current++;
	incl r2			# offset++;
	cmpb (r1),r0		# if( mem[current] == delim ) goto encl4
	beql encl4
	tstb (r1)		# if( mem[current != NULL ) goto encl2
	bneq encl2

				# /* mem[current] is null.. */
	subb2 $4,r7
	movl r2,(r7)		# push( offset );
	subb2 $4,r7
	movl r2,(r7)		# push( offset );
	jmp (r11)		# return;

encl4:				# /* Found the trailing delimiter */
	subb2 $4,r7
	movl r2,(r7)		# push( offset );
	subb2 $4,r7
	addl3 $1,r2,(r7)	# offset++; push( offset );
	jmp (r11)		# return;

 #	: HOLD		( C -- <PLACE C AT --HLD> )
 #	  HLD -- HLD @ C!
 #	;
 #
 #	This word is used to place something in the pad during formatted
 #	numeric output. The current location in the buffer is decremented
 #	and the byte on top of the math stack is stored.

	colon "HOLD","HOLD","ENCLOSE"
	offset HLD
	offset MinusMinus
	offset HLD
	offset Fetch
	offset CStore
	offset SemiS

 #	: (NUMBER)		( num.lo num.hi addr -- num.lo num.hi addr )
 #	LABEL NUM1
 #	  1+
 #	  DUP >R C@ BASE @ DIGIT
 #	  0BRANCH NUM2			( WHILE )
 #	  SWAP BASE @ U* DROP
 #	  ROT BASE @ U* D+
 #	  DPL @ 1+
 #	  ZBRANCH NUM3
 #	  DPL ++			( IF )
 #	LABEL NUM3
 #	  R>				( ENDIF )
 #	  BRANCH NUM1			( REPEAT )
 #	LABEL NUM2
 #	  R>
 #	;
 #
 #	This word collects ASCII digits starting at the specified address
 #	into a binary double-length number. The digits are added to the
 #	original double-length number on the stack; it is expected that
 #	conversion will be interrupted when a decimal point is found and
 #	resumed afterwards.
 #
 #	As digits are accumulated, the count of digits after the decimal
 #	point is maintained in DPL. If DPL is -1, it is assumed that a
 #	decimal point has not yet been found and DPL is not modified. If
 #	DPL is not -1, it is assumed that a decimal point has been found.
 #	For each digit that is succesfully converted, DPL is incremented.
 #
 #	Conversion continues until a non-digit character is encountered.
 #	At exit, addr is the address of this non-digit character.

	colon "PNumber","(NUMBER)","HOLD"
Num1:	offset OnePlus
	offset DUP
	offset ToR
	offset CFetch
	offset BASE
	offset Fetch
	offset DIGIT
	offset ZBranch
	offset Num2
	offset SWAP
	offset BASE
	offset Fetch
	offset UStar
	offset DROP
	offset ROT
	offset BASE
	offset Fetch
	offset UStar
	offset DPlus
	offset DPL
	offset Fetch
	offset OnePlus
	offset ZBranch
	offset Num3
	offset DPL
	offset PlusPlus
Num3:	offset FromR
	offset BRANCH
	offset Num1
Num2:	offset FromR
	offset SemiS

 #	: NUMBER		( addr -- val.lo val.hi )
 #	  0 0 ROT DUP 1+ C@
 #	  LIT '-' = DUP >R + -1
 #	LABEL N1			( BEGIN )
 #	  DPL ! (NUMBER) DUP C@ BL !=
 #	  0BRANCH N2			( WHILE )
 #	  DUP C@ LIT '.' != 0 ?ERROR 0	( . )
 #	  BRANCH N1			( REPEAT )
 #	LABEL N2
 #	  DROP R>
 #	  0BRANCH N3			( IF )
 #	  DMINUS
 #	LABEL N3
 #	;
 #
 #	This word converts an ASCII digit string beginning at addr to a
 #	binary number, returning the double-length value of that number.
 #
 #	Conversion is performed using (NUMBER). When (NUMBER) stops, the
 #	offending character is examined. If it is a blank, the conversion
 #	is done and NUMBER exits. If it is a decimal point, DPL is set to
 #	zero to record the fact that a decimal point has been found and
 #	(NUMBER) is re-entered. If the character is neither a blank nor
 #	a decimal point, MSG # 0 is displayed.
 #
 #	By the way, I believe this word is the reason WORD insists that
 #	there be a blank following the word that it has copied from TIB.
 #	If INTERPRET cannot find a word in the dictionary, it assumes it
 #	must be a number and hands it over to NUMBER. If the word did not
 #	have a blank after it, the conversion would end on a non-blank
 #	character and NUMBER would issue MSG # 0. What can I say? FORTH
 #	seems to be held together by side-effects...

	colon "NUMBER","NUMBER","PNumber"
	offset Zero
	offset Zero
	offset ROT
	offset DUP
	offset OnePlus
	offset CFetch
	literal 0x2d		# <^A/-/>
	offset Equal
	offset DUP
	offset ToR
	offset Plus
	literal -1
N1:	offset DPL
	offset Store
	offset PNumber
	offset DUP
	offset CFetch
	offset BL
	offset NotEq
	offset ZBranch
	offset N2
	offset DUP
	offset CFetch
	literal 0x2e		# <^A/./>
	offset NotEq
	offset Zero
	offset QError
	offset Zero
	offset BRANCH
	offset N1
N2:	offset DROP
	offset FromR
	offset ZBranch
	offset N3
	offset DMINUS
N3:	offset SemiS

 #	: SPACES		( a -- )
 #	  0 MAX -DUP 0BRANCH SPACES1
 #	  0 (DO)
 #	LABEL SPACES2
 #	      SPACE
 #	  (LOOP) SPACES2
 #	LABEL SPACES1
 #	;
 #
 #	Prints the specified number of spaces on the console.

	colon "SPACES","SPACES","NUMBER"
	offset Zero
	offset MAX
	offset MinusDup
	offset ZBranch
	offset Spaces1
	offset Zero
	offset PDo
Spaces2: offset SPACE
	offset PLoop
	offset Spaces2
Spaces1: offset SemiS

 #	: <#		( -- )
 #	  PAD HLD !
 #	;
 #
 #	This word initializes variables used by the formatted output words.
 #	Basically, it points HLD at the end of a region following the end
 #	of the dictionary. As formatted output is generated, it moves towards
 #	the dictionary.

	colon "LessHash","<#","SPACES"
	offset PAD		# Get address of end of scratchpad
	offset HLD		# Initialize HLD
	offset Store
	offset SemiS

 #	: #>		( d.hi d.lo -- addr count )
 #	  DROP DROP 	( d.hi d.lo -- )
 #	  HLD @ 	( -- addr )
 #	  PAD OVER -	( -- addr count )
 #	;
 #
 #	This word ends formatted numeric output. It drops the double-precision
 #	number from the math stack and returns the address and size of the
 #	formatted output.

	colon "HashGreater","#>","LessHash"
	offset DROP
	offset DROP
	offset HLD
	offset Fetch
	offset PAD
	offset OVER
	offset Subtract
	offset SemiS

 #	: SIGN		( d.hi d.lo -- d.hi d.lo )
 #	  ROT 0< 	( d.hi d.lo -- d.hi d.lo 1 <if negative> )
 #	  0BRANCH SIGN1	( -- d.hi d.lo )
 #	  LIT '-' HOLD	( stash the minus sign )
 #	LABEL SIGN1
 #	;
 #
 #	This word stuffs a '-' into the formatted numeric string if the
 #	number being converted is negative.

	colon "SIGN","SIGN","HashGreater"
	offset ROT
	offset ZLess
	offset ZBranch
	offset Sign1
	literal 0x2d		# <^A/-/>
	offset HOLD
Sign1:	offset SemiS

 #	: #		( d1 -- d2 )
 #	  BASE @ 		( d1 -- d1 base )
 #	  M/MOD 		( -- remainder d1/base.hi d1/base.lo )
 #	  ROT 			( -- d1/base remainder )
 #	  LIT 9 OVER		( -- d1/base remainder 9 remainder ) 
 #	  < 			( -- d1/base remainder 1 <if remainder < 9> )
 #	0BRANCH #1		( -- d1/base remainder )
 #	  LIT 7 +		( -- d1/base remainder+7 )
 #				( 7 is offset to make 'A' come after '9' )
 #	LABEL #1
 #	  LIT '0' + 		( -- d1/base remainder+'0' )
 #	  HOLD			( -- d1/base )
 #	;
 #
 #	This word generates the next digit from a double-length number 
 #	on the top of the math stack. The result is the original double-length
 #	number divied by the current base.

	colon "Hash","#","SIGN"
	offset BASE
	offset Fetch
	offset MSlashMod
	offset ROT
	literal 9
	offset OVER
	offset Less
	offset ZBranch
	offset Hash1
	literal 7
	offset Plus
Hash1:	literal 0x30		# <^A/0/>
	offset Plus
	offset HOLD
	offset SemiS

 #	: #S
 #	LABEL #S1
 #	  # 2DUP OR 0= ZBRANCH #S1
 #	;
 #
 #	This word generates digits until the number being converted has
 #	gone to zero.

	colon "HashS","#S","Hash"
HashS1:	offset Hash
	offset TwoDUP
	offset OR
	offset ZEq
	offset ZBranch
	offset HashS1
	offset SemiS

 #	: D.R		( d.lo d.hi n -- )
 #	  >R SWAP	( d.lo d.hi n -- d.hi d.lo d.hi )
 #	  OVER DABS	( -- d.hi |d.lo d.hi| )
 # 	  <# #S 	( -- d.hi )
 #	  SIGN #> 	( -- pad count )
 #	  R> OVER 	( -- pad count n count )
 #	  - SPACES 	( -- pad count )
 #	  TYPE		( -- )
 #	;
 #
 #	This word displays the double-precision number d in the current
 #	base in an n character field.

	colon "DDotR","D.R","HashS"
	offset ToR
	offset SWAP
	offset OVER
	offset DABS
	offset LessHash
	offset HashS
	offset SIGN
	offset HashGreater
	offset FromR
	offset OVER
	offset Subtract
	offset SPACES
	offset TYPE
	offset SemiS

 #	: .R		( a n -- )
 #	  >R S->D R> D.R
 #	;
 #
 #	This word prints the number a in an n character field. It does this
 #	by sign-extending a and then printing the resulting double-precision
 #	number using D.R.

	colon "DotR",".R","DDotR"
	offset ToR
	offset SignExtend
	offset FromR
	offset DDotR
	offset SemiS

 #	: D.
 #	  0 D.R SPACE
 #	;
 #
 #	This word displays a double-precision number.

	colon "DDot","D.","DotR"
	offset Zero
	offset DDotR
	offset SPACE
	offset SemiS

 #	: .		( a -- )
 #	  S->D D.
 #	;
 #
 #	This word prints the single-precision number on top of the math
 #	stack. It does this by sign-extending the number to a double-length
 #	integer and printing the resulting double-length integer.

	colon "Dot",".","DDot"
	offset SignExtend	# ( a -- s.a a )
	offset DDot		# ( s.a a -- ) 		Print it
	offset SemiS		# 			We're done

 #	: ?		( a -- )
 #	  @ .
 #	;
 #
 #	This word displays what is in memory at the location addressed by the
 #	top of stack.

	colon "Question","?","Dot"
	offset Fetch
	offset Dot
	offset SemiS

 #	: U.
 #	  0 D.
 #	;
 #
 #	This word displays an unsigned single-precision number. It does this
 #	by zero-extending the number to double-precision and then displaying
 #	the double-precision result.

	colon "UDot","U.","Question"
	offset Zero
	offset DDot
	offset SemiS

 #	: expect		( buffer maxsize -- )
 #
 #	Can't use BDOS here because BDOS expects maxsize to be in
 #	memory in front of buffer.
 #
 #	This procedure reads a line into the specified buffer.
 #
 #	This is really primitive. The only non-printing characters it knows
 #	about are BS, DEL, and CR. All other whitespace is ignored.
 #
 #	If you have typed past the end of the buffer, the new character is
 #	ignored; it is not echoed so that you will know you've done something
 #	wrong.
 #
 #	Input ends when you press CR. At that time, a CR/LF is echoed and
 #	EXPECT exits.
 #
 #	If you press BS or DEL and you aren't at the beginning of the buffer,
 #	BS/space/BS will be echoed and EXPECT will back up one position in
 #	the buffer.

	code "EXPECT","EXPECT","UDot"
	movl (r7),r9		# Maximum buffer size to R9
	addb2 $4,r7		# discard maxsize
	movl (r7),r8		# Buffer start address to R8
	addb2 $4,r7		# discard address
	addl2 r8,r9		# Address of location after buffer to R9
	movl r8,r6		# Current address to R6

Expect1:
	bsbw ConIn		# Get a key
	bicb2 $0x80,r0		# Just to be sure
	cmpb $13,r0		# Is it a <CR>?
	beql Expect_CR		# If so, go
	cmpb $8,r0		# Is it a <BS>
	beql Expect_BS		# If so, go
	cmpb $127,r0
	beql Expect_BS
	cmpb $32,r0		# Is it other whitespace?
	bgtr Expect1		# If so, we don't know how to deal with it,
				# so ignore it and hope it'll go away.

 #	Received a printable character

	cmpl r6,r9		# Is the buffer full?
	beql Expect1		# If so, ignore the character

	movb r0,(r6)+		# Otherwise, store the buffer
	bsbw FConOut		# Echo it
	brb Expect1		# And wait for another

 #	Received a backspace

Expect_BS:
	cmpl r8,r6		# Is the buffer empty?
	beql Expect1		# If so, ignore it
	decl r6			# Subtract the character from the buffer
	movzbl $8,r0		# And from the screen
	bsbw FConOut
	movzbl $32,r0
	bsbw FConOut
	movzbl $8,r0
	bsbw FConOut
	brb Expect1		# And wait for another

 #	Received a CR

Expect_CR:
	movzbl $13,r0		# Echo for the user
	bsbw FConOut
	movzbl $10,r0
	bsbw FConOut
	cmpb r6,r9		# Room for null terminator?
	beql Expect_Exit	# Nope; just leave
	clrb (r6)		# otherwise, terminate the booger
Expect_Exit:
	jmp (r11)		# Back to ForthOMat

 #	: ID.			( NFA -- <PRINT ID OF A WORD> )
 #	  dup @ lit 0x1f and 	( nfa wordlength )
 #	  swap 1+ swap		( nfa+1 wordlength )
 #	  type space
 #	;
 #
 #	I've redone this one because I think the original code was
 #	idiotic; it calculated the string length by subtracting LFA from
 #	NFA (after first taking a detour through PFA because that's what
 #	LFA expects) rather than just fetching the damn thing from the
 #	NFA where it started! The guy also seems to want to lay down blanks
 #	on top of memory he's just gonna overwrite anyway (see WORD).
 #
 #	This word prints the name field of a word. This is primarily used
 #	by VLIST to display the contents of the dictionary.

	colon "IDDot","ID.","EXPECT"
	offset DUP		# Don't want to lose nfa when we fetch
	offset Fetch
	literal 0x1f
	offset AND
	offset SWAP
	offset OnePlus
	offset SWAP
	offset TYPE
	offset SPACE
	offset SemiS

 #	;S: Loads IP from return stack
 #
 #	This procedure ends a FORTH word; it is the runtime action performed
 #	by ;. It copies a value from the return stack into IP (R10) and resumes
 #	execution at the new IP. The IP was originally stashed by DoCol when 
 #	the word was entered. 
 #
 #	; refers to this word during compilation.

	code "SemiS","\x3bS","IDDot"	# ";S"
	movl (sp)+,r10
	jmp (r11)

 #	: [		( BEGIN EXECUTING )
 #	  0 STATE !
 #	;*
 #
 #	This word takes the FORTH system out of compiling mode.

	colon "LBracket","[","SemiS",Immediate
	offset Zero
	offset STATE
	offset Store
	offset SemiS

 #	: ]		( END EXECUTING )
 #	  LIT 0xC0 State !
 #	;*
 #
 #	This word puts the FORTH system in compiling mode.

	colon "RBracket","]","SemiS",Immediate
	literal 0xc0
	offset STATE
	offset Store
	offset SemiS

 #	: :		( DEFINE A WORD )
 #	  ?EXEC
 #	  !CSP
 #	  CREATE ]	( MAKE THE WORD HEADER AND BEGIN COMPILING )
 #	  (;CODE) DOCOL
 #	;*
 #
 #	This word makes a new FORTH word; it makes an entry in the
 #	dictionary with a CFA that points to DoCol, the code segment
 #	that knows how to enter a FORTH word.
 #
 #	Unfortunately, DoCol doesn't show up in the dictionary, so you
 #	can't define your own word equivalent to :...
 #
 #	I'm not convinced : has to be immediate...

	colon "Colon",":","RBracket",Immediate
	offset QExec
	offset StoreCSP
	offset CREATE
	offset RBracket
	offset PSemiCode
	offset DoCol
	offset SemiS

 #	: ;		( END A DEFINITION )
 #	  ?CSP		( CHECK THAT WE'RE DONE )
 #	  COMPILE ;S	( PLACE ;S AT THE END )
 #	  SMUDGE [	( MAKE THE WORD FINDABLE AND BEGIN INTERPRETING
 #	;*
 #
 #	This word finishes the definition of a word started with :. It
 #	appends the address of ;S, the code segment that knows how to
 #	exit a FORTH word, to the current word and takes the FORTH system
 #	back to interpreting mode.

	colon "Semicolon","\x3b","Colon",Immediate	# "\;",Immediate
	offset QCSP
	offset COMPILE
	offset SemiS
	offset SMUDGE
	offset LBracket
	offset SemiS

 #	: CONSTANT
 #	  CREATE SMUDGE ,
 #	  (;CODE) DOCON
 #	;
 #
 #	This word creates a constant. A constant is a word that, when executed,
 #	puts the longword in its Parameter Field onto the math stack. This word
 #	creates a new word, points its CFA at the segment of code that knows
 #	how to do a constant (DoCon), and compiles the item on top of the math
 #	stack into the word's parameter field.
 #
 #	DoCon doesn't show up in the dictionary, so you can't write your own
 #	word equivalent to CONSTANT...

	colon "CONSTANT","CONSTANT","Semicolon"
	offset CREATE
	offset SMUDGE
	offset Comma
	offset PSemiCode
	offset DoCon
	offset SemiS

 #	: VARIABLE
 #	  CONSTANT
 #	  (;CODE) DOVAR
 #	;
 #
 #	This word creates a variable in the dictionary. A variable is
 #	similar to a constant, except that it places the address of its
 #	parameter field on the math stack instead of the longword stored
 #	there. This word creates the variable by first creating a constant
 #	and then changing the CFA of the last word created to point to
 #	DoVar, the code that knows how to do a variable.
 #
 #	Unfortunately, DoVar doesn't show up in the dictionary, so you 
 #	can't write your own code equivalent to VARIABLE...

	colon "VARIABLE","VARIABLE","CONSTANT"
	offset CONSTANT
	offset PSemiCode
	offset DoVar
	offset SemiS

 #	: CODE
 #	  CREATE 4 HERE 4 - W!
 #	;
 #
 #	This word starts the definition of a word containing machine code.
 #	It does whatever : does, then mutilates the CFA of the word to
 #	point to the word's PFA; that is, it modifies the word to indicate
 #	that the code with knows how to execute the word is in the word's
 #	PFA.
 #
 #	$$$

	colon "CODE","CODE","VARIABLE"
	offset CREATE
	literal 4		# Offset to code that knows how to do the word
	offset HERE		# Here is now the address of the PFA
	literal 4		# Form address of CFA
	offset Subtract
	offset Store
	offset SemiS

 #	: EDOC
 #	  ?csp
 #	  lit 'JMP (R11)' W, smudge [
 #	;*
 #
 #	This word ends the definition of a word containing machine code.
 #	It puts the end instruction ( JMP (R11) ) at the end of the word
 #	and returns the FORTH system to interpretation mode.
 #
 #	Since EDOC properly terminates a machine code word, the minimal
 #	machine code word is:
 #
 #	CODE BOOGER EDOC
 #
 #	$$$

	colon "EDOC","EDOC","CODE",Immediate
	offset QCSP
	offset LIT
	jmp (r11)		# We want the instruction JMP (R11)
	.WORD 0			# LITERAL has to be a longword
	offset WComma
	offset LBracket
	offset SMUDGE
	offset SemiS

 #	: (;CODE)	( fetch the word after (;code), relocate, and
 #			  toss on the CFA of the current word
 #	  R 		( Address of offset )
 #	  dup @ + 	( Fetch it and form absolute address )
 #	  LATEST PFA CFA ( Address where we want to store it )
 #	  swap over	( -- addr what addr )
 #	  - swap !	( Form offset AND store )
 #	  r> 4 + >r	( skip the offset )
 #	;
 #
 #	This word is followed by a word parameter. The word parameter specifies
 #	the address of the code that is to be executed when the latest word
 #	defined is entered; the parameter is, of course, a relative offset
 #	to the code.

	colon "PSemiCode","(\x3b\CODE)","EDOC"	# "(;CODE)"
	offset R
	offset DUP
	offset Fetch
	offset Plus
	offset LATEST
	offset PFA
	offset CFA
	offset SWAP
	offset OVER
	offset Subtract
	offset SWAP
	offset Store
	offset FromR
	literal 4
	offset Plus
	offset ToR
	offset SemiS

 #	: -FIND			( -- pfa length 1 <if found> )
 #				( -- 0 <if not found> )
 #	  BL WORD		append the word we're searchin' for to
 #				the dictionary
 #	  HERE LATEST (FIND)
 #	;
 #
 #	This word searches the dictionary for the word at the end of the
 #	dictionary; WORD copies a word from TIB to the end of the
 #	dictionary. If the word is not found in the dictionary, 0 is
 #	placed on the math stack. Otherwise, the parameter field address
 #	of the found word, the length byte from the found word's name
 #	field, and a 1 are placed on the stack.
 #
 #	INTERPRET depends on the entire length byte from the found word
 #	being pushed onto the stack rather than just the length of the word.
 #	The length byte of a FORTH word includes some flags, most importantly
 #	for this discussion the immediate flag. INTERPRET compares STATE
 #	against the returned length byte to determine whether to compile
 #	or execute the found word; if the actual length byte is not returned,
 #	INTERPRET will never decide to execute immediate words during
 #	compilation.

	colon "MinusFind","-FIND","PSemiCode"
	offset BL
	offset WORD
	offset HERE
	offset LATEST
	offset PFind
	offset SemiS

 #	: (		( COMMENT )
 #	  LIT ')'	( CLOSING PAREN )
 #	  WORD
 #	;*
 #
 #	This word ignores a comment. Bytes are collected at the end of the
 #	dictionary until the matching close paren. These bytes are then
 #	ignored.

	colon "Comment","(","MinusFind",Immediate
	literal 0x29		# <^A/)/>
	offset WORD
	offset SemiS

 #	: lfa			( pfa -- lfa )
 #	  8 -
 #	;
 # 
 #	This word takes the mythical "parameter field address" and 
 #	returns the link field address. The parameter field address is
 #	the area after the first word of code; variables and constants
 #	etc. store data after a code word that does the work expected
 #	of a variable (fetching the address of the variable) or
 #	constant (fetching the value of the constant).

	colon "LFA","LFA","Comment"
	literal 8
	offset Subtract
	offset SemiS

 #	: cfa			( pfa -- cfa )
 #	  4 -
 #	;
 #
 #	This word takes the mythical "parameter field address" and
 #	returns the address of the code field, the start of code for
 #	the word.

	colon "CFA","CFA","LFA"
	literal 4
	offset Subtract
	offset SemiS

 #	: nfa			( pfa -- nfa )
 #	  lfa
 #	label nfa1
 #	  1- dup c@ lit 0x80 and 0branch nfa1
 #	;
 #
 #	This word takes the mythical "parameter field address" and
 #	returns the address of the name field; this is the first byte
 #	of the word and contains the name length, smudge, and immediate
 #	flags.
 #
 #	This word works by first backing up to the LFA (which immediately
 #	follows the name field) and then searching backwards for a byte
 #	with bit 7 set. This should be the length byte.

	colon "NFA","NFA","CFA"
	offset LFA		# Take us to the link field address
NFA1:	offset OneMinus		# Let's look at the previous byte
	offset DUP		# (don't want to lose it when we fetch)
	offset CFetch
	literal 0x80		# Is bit 7 set?
	offset AND
	offset ZBranch		# If not, keep looking
	offset NFA1
	offset SemiS		# If so, we're done

 #	: pfa		( nfa -- pfa )
 #	  dup @
 #	  lit 0x1f and +  ( take us to the lfa )
 #	  lit 9 +	  ( skip lfa, cfa, and length byte )
 #	;
 #
 #	This procedure finds the mythical "parameter field address" given
 #	the name field address. It uses the length byte attached to the
 #	name to skip the name and then bumps past the lfa and cfa.

	colon "PFA","PFA","NFA"
	offset DUP		# don't want to lose nfa when we fetch
	offset Fetch		# get the name length
	literal 0x1f		# strip off smudge, immediate, and findme
	offset AND
	offset Plus		# add it to the nfa
	literal 9
	offset Plus		# skip the lfa, cfa, and length byte
	offset SemiS

 #	: SMUDGE		( TOGGLE COMPLETION BIT OF LATEST WORD )
 #	  LATEST		( WHEN THIS BIT=1, WORD CAN'T BE FOUND )
 #	  LIT 0x20 TOGGLE
 #	;
 #
 #	This word toggles the "smudge" bit of the last word that was
 #	compiled. Until smudge is cleared, the word can't be found.
 #
 #	Not that this is a useful feature; we don't always begin searching
 #	with the latest word compiled and we always smudge a word when we're
 #	done compiling it.

	colon "SMUDGE","SMUDGE","PFA"
	offset LATEST
	literal 0x20
	offset TOGGLE
	offset SemiS

 #	: LATEST		( NFA OF LAST WORD DEFINED )
 #	  CURRENT @ ;
 #
 #	This fetches the address of the current word being worked on.
 #	Originally, it was some sort of "user" variable, so a double
 #	fetch was needed. I've made it a normal variable, so we only
 #	need a single fetch.

	colon "LATEST","LATEST","SMUDGE"
	offset CURRENT
	offset Fetch
	offset SemiS

 #	: create		( -- pfa )
 #	  bl word		( tack word on end of dictionary )
 #	  here dup 		( here will change after allot )
 #	  c@ 1+ allot		( allocate space for word & length )
 #	  dup			( still need old here to update current )
 #	  lit 0xa0 toggle	( set top bit in first char & smudge )
 #	  latest o,		( store address of previous word )
 #	  current !		( update current word pointer )
 #	  (;code) dovar		( originally, word will look like variable )
 #	  4 allot		( allocate space taken by CFA )
 #	;
 #
 #	This word builds a header for a word. It points the CFA at the
 #	code that knows how to execute a variable; this means that,
 #	unless the CFA is adjusted, the word will act like a variable. A
 #	compiling word should fill in the CFA of the word with the
 #	address of a machine-code routine that knows how to enter
 #	the word.
 #
 #	Oh yeah; although the word behaves like a variable, no space is
 #	allocated for the PFA.

	colon "CREATE","CREATE","LATEST"
	offset BL
	offset WORD
	offset HERE
	offset DUP		# here here
	offset CFetch		# here length (smudge not set yet)
	offset OnePlus		# here length+1 (account for length byte)
	offset ALLOT		# here (note, real here has changed; old
				#       here is still on stack)
	offset DUP		# here here 
	literal 0xa0		# here here 0xa0
	offset TOGGLE		# here (smudge now set)
	offset LATEST		# here latest
	offset OComma		# here
	offset CURRENT		# here current
	offset Store		#
	offset PSemiCode
	offset DoVar
	literal 4
	offset ALLOT
	offset SemiS

 #	: compile	( fetch the word following and compile it onto the
 #			  end of the dictionary )
 #	  R 		( Address of offset )
 #	  dup @ + 	( Fetch it and form absolute address )
 #	  o,
 #	  r> 4 + >r	( skip the offset )
 #	;
 #
 #	This word takes the offset immediately following a reference to it
 #	and compiles a reference to that word at the end of the
 #	dictionary. The reference is (of course) a word offset, so it has
 #	to be first un-PICced to get the address being referenced and then
 #	rePICced when it's compiled into the new word.

	colon "COMPILE","COMPILE","CREATE"
	offset R
	offset DUP
	offset Fetch
	offset Plus
	offset OComma
	offset FromR
	literal 4
	offset Plus
	offset ToR
	offset SemiS

 #	: [COMPILE]	( COMPILE THE NEXT WORD, EVEN IF IT'S IMMEDIATE )
 #	  -FIND 0= 0 ?ERROR DROP CFA o,
 #	;*
 #
 #	This word locates a word in the dictionary and compiles a reference
 #	to the word into the word being built, conveniently ignoring the
 #	IMMEDIATE bit; if you want to make a word that does something similar
 #	to :, you can [COMPILE] : to make a reference to : inside your word.

	colon "BracketsCOMPILE","[COMPILE]","COMPILE"
	offset MinusFind
	offset ZEq
	offset Zero
	offset QError
	offset DROP
	offset CFA
	offset OComma
	offset SemiS

 #	: IMMEDIATE	( MAKE MOST-RECENT WORD IMMEDIATE )
 #	  LATEST LIT 0x40 TOGGLE
 #	;
 #
 #	This word toggles the IMMEDIATE bit of the last word that was
 #	compiled. If the IMMEDIATE bit is set, the word is executed during
 #	compile mode instead of a reference to the word being compiled into
 #	the word being built.

	colon "IMMEDIATE","IMMEDIATE","BracketsCOMPILE"
	offset LATEST
	literal 0x40
	offset TOGGLE
	offset SemiS

 #	: forget
 #	  ' 		( find the word to forget: -- pfa )
 #	  dup 		( pfa pfa )
 #	  nfa 		( pfa nfa )
 #	  dp !		( pfa )
 #	  lfa 		( lfa )
 #	  dup		( lfa lfa )
 #	  @ +		( nfa of previous word )
 #	  dup		( pfa nfa nfa )
 #	  current !	( pfa nfa )
 #	;
 #
 #	This word locates the named word in the dictionary and updates the
 #	dictionary pointer and various other variables to believe that word
 #	is the last word that was defined.
 #
 #	Please don't FORGET built-in words...

	colon "FORGET","FORGET","IMMEDIATE"
	offset Tick
	offset DUP
	offset NFA
	offset DP
	offset Store
	offset LFA
	offset DUP
	offset Fetch
	offset Plus
	offset DUP
	offset CURRENT
	offset Store
	offset SemiS

 #	: vlist		( display the dictionary )
 #	  latest	( nfa of last word in dictionary )
 #	label vlist1
 #	  dup id.	( display the name of this word )
 #	  pfa lfa	( lfa )
 #	  dup @		( lfa OffsetToPreviousWord )
 #	  dup		( lfa offset offset )
 #	  0branch vlist2 ( if offset is zero, we're done )
 #	  +		( make offset to previous nfa )
 #	  branch vlist1
 #	label vlist2
 #	  drop drop	( lost lfa and link of last word )
 #	;
 #
 #	This word lists the contents of the dictionary in order from the
 #	last word to the first word.

	colon "VLIST","VLIST","FORGET"
	offset LATEST	# ( nfa )		Start at the last word
vlist1: offset DUP	# ( nfa nfa )
	offset IDDot	# ( nfa )		Display its name
	offset PFA	# ( pfa )
	offset LFA	# ( lfa )
	offset DUP	# ( lfa lfa )
	offset Fetch	# ( lfa link )		Get link to previous word
	offset DUP	# ( lfa link link )
	offset ZBranch	# ( lfa link )		If it's zero, we're done
	offset vlist2
	offset Plus	# ( lfa+link = nfa of previous word )
	offset BRANCH	#			Advance to next word
	offset vlist1
vlist2:	offset DROP
	offset DROP
	offset SemiS

 #	: '		( -- pfa <if interpreting> )
 #	  -FIND 0= 0 ?ERROR DROP LITERAL
 #	;*
 #
 #	This word searches the dictionary for a word and returns the address
 #	of its parameter field.
 #
 #	It's essentially a more user-friendly front-end to -FIND.

	colon "Tick","'","VLIST"
	offset MinusFind
	offset ZEq
	offset Zero
	offset QError
	offset DROP
	offset LITERAL
	offset SemiS

 #	: word		( c -- <get next word to end of dictionary,
 #			  delimited with c or null > )
 #	  tib @
 #	  in @ + swap enclose	( get the word )
 #	  here lit 0x22 blanks  ( blank space after the word )
 #	  in +! over - >r r here c! + here 1+ r> cmove
 #
 #	This routine looks at the current data in the terminal input buffer
 #	(tib) for a word delimited by the specified character. The string
 #	containing that word is appended to the dictionary (starting at
 #	here) after a byte giving the length of the word.
 #
 #	Note that word is sort of trusting in that it expects you to not
 #	type a word in longer than 32 characters. The original code also
 #	has this problem. Maybe I should fix this... *** note ***
 #
 #	The main reason for the blanks bit seems to be to guarantee that
 #	NUMBER will find a blank when it finishes fiddling with the word.

	colon "WORD","WORD","Tick"	# c
	offset TIB		# Where to look for the word
	offset Fetch		# c tib
	offset IN		# Current offset in tib
	offset Fetch		# c tib in
	offset Plus		# c tib+in
	offset SWAP		# tib+in c
	offset ENCLOSE		# tib+in first last next
	offset HERE 
	literal 0x22		# tib+in first last next here 0x22
	offset BLANKS		# tib+in first last next
	offset IN		# Bump IN past the word
	offset PlusStore	# tib+in first last
	offset OVER		# tib+in first last first
	offset Subtract		# tib+in first wordlength
	offset ToR
	offset R		# tib+in first wordlength
	offset HERE		# tib+in first wordlength here
	offset CStore		# Store the length of the word
				# tib+in first
	offset Plus		# tib+bin+first
	offset HERE		# tib+in+first here
	offset OnePlus		# tib+in+first here+1
	offset FromR		# tib+in+first here+1 wordlength
	offset CMOVE		# shazam!
	offset SemiS

 #	: execute		( cfa -- )
 #
 #	Given the address of a FORTH word, the word is executed. INTERPRET
 #	uses this to execute words you type at the console.

	code "EXECUTE","EXECUTE","WORD"
	movl (r7),r9		# Get address of routine to execute
	addb2 $4,r7		# discard cfa
	addl3 (r9),r9,r8	# Fetch the offset AND convert to an address
	jmp (r8)		# Execute the word.

 #	: (find)		( target start -- 0 <if not found>
 #					       -- pfa lengthbyte  1 <if found> )
 #
 #	Search the directory for the specified word

	code "PFind","(FIND)","EXECUTE"

	movl r7,r2		# r2 <- address of start
	movl r7,r3		# r3 <- address of target
	addb2 $4,r3

PFind1:

 #	First we have to compare the length bytes. We need to strip flag
 #	bits off the length byte at Start except for the smudge bit.

	bicb3 $0xe0,*(r2),r0	# Strip the bits
	cmpb r0,*(r3)		# Do the length bytes match?
	bneq PFind2		# If not, it can't be a match

 #	We have strings of the same length. Now compare the strings

	movl (r2),r4		# Get addresses
	movl (r3),r5
	incl r4			# Bump past length byte
	incl r5
	movzbl *(r3),r9		# Need a word string length

#	cmpc3 r9,(r4),(r5)	# Compare them
0:	cmpb (r4)+,(r5)+	# MicroVAX II doesn't do CMPC3
	bneq PFind2
	sobgtr r9,0b

 #	The strings matched. Diddle the stack.

	bicb3 $0xe0,*(r2),r1	# Figure out where the PFA is
	movzbl r1,r1
	addl3 r1,(r2),r0	# Here's the LFA
	movzbl *(r2),r1		# Get raw length byte again
	addl3 $9,r0,(r3)	# There's the PFA, and don't forget the
				# length byte...
	movzbl r1,(r2)		# And the length
	subb2 $4,r7		# make space for the flag
	movzbl $1,(r7)		# And success
	jmp (r11)

 #	The strings didn't match. Look at the next one.

PFind2:

	bicb3 $0xe0,*(r2),r0	# Get length of string
	movzbl r0,r0
	addl2 r0,(r2)		# Make address of LFA
	incl (r2)		# (account for the length byte)
	movl *(r2),r0		# Get link field value
	tstl r0			# Is it the end flag?
	beql PFind3
	addl2 r0,(r2)		# Update the start address
	brb PFind1		# Keep going

 #	Went to the end of the dictionary

PFind3:

	addb2 $4,r7		# discard starting address
	clrl (r7)		# replace target address with failure.
	jmp (r11)

 #	: ERROR		( N -- <ISSUE ERROR #N> )
 #	  HERE COUNT TYPE (.") "? ERR # " ( THEN )
 #	  .
 #	  SP!		( EMPTY THE STACK )
 #	  QUIT		( THEN )
 #	;
 #
 #	This word is invoked if there's some sort of error. It displays the
 #	string at the end of the dictionary to hopefully give some context
 #	for the error; INTERPRET works by building words at the end of the
 #	dictionary and then searching for them. This should be the name of
 #	the last word INTERPRET searched for.
 #
 #	$$$

	colon "ERROR","ERROR","PFind"
	offset HERE		# Display name of last word searched for.
	offset COUNT
	offset TYPE
	offset PDotQuote	# And Huh?
	string "? MSG # "
	offset Dot
	offset SPStore		# Initialize math stack
	offset QUIT		# Initialize the rest of the system
	offset SemiS

 #	: ?ERROR	( F N -- <IF F, DO ERROR #N> )
 #	  SWAP
 #	  0BRANCH QERR1
 #	  ERROR		( IF <YOU CAN'T RETURN FROM ERROR> )
 #	LABEL QERR1
 #	  DROP		( THEN )
 #	;
 #
 #	If F is non-zero, error message #N is displayed.

	colon "QError","?ERROR","ERROR"
	offset SWAP	# ( F N -- N F )
	offset ZBranch	# If F is zero, go. ( N F -- N )
	offset QErr1
	offset ERROR	# Argh! Complain.
QErr1:	offset DROP	# ( N -- )
	offset SemiS

 #	: ?COMP		( GIVE ERR#17 IF NOT COMPILING )
 #	  STATE @ 0= LIT 17 ?ERROR
 #	;
 #
 #	This word is used by various compiling words to verify that the
 #	FORTH system is compiling. If not, it will abort and give error
 #	# 17.
 #
 #	The variable STATE is checked to see if we're compiling. STATE is
 #	0 while interpreting and non-0 while compiling.

	colon "QComp","?COMP","QError"
	offset STATE
	offset Fetch
	offset ZEq		# Complement STATE; return 1 if 0 & vice-versa
	literal 17
	offset QError
	offset SemiS

 #	: ?EXEC		( GIVE ERR #18 IF NOT EXECUTING )
 #	  STATE @ LIT 18 ?ERROR
 #	;
 #
 #	If we're not compiling, we must be "executing". I don't know why
 #	this isn't ?INTERP, but I'm not complaining.

	colon "QExec","?EXEC","QComp"
	offset STATE
	offset Fetch
	literal 18
	offset QError
	offset SemiS

 #	: ?PAIRS	( GIVE ERR #19 IF PAIRS DON'T MATCH )
 #	  - LIT 19 ?ERROR
 #	;
 #
 #	This word is used by compiling words like THEN, LOOP, etc. to make
 #	sure we're ending the correct type of structure.
 #
 #	While compiling a structure, one of these words will push their
 #	information and a code for the type of structure on the stack.
 #	This word just makes certain the top two words of the stack are
 #	the same.

	colon "QPairs","?PAIRS","QExec"
	offset Subtract
	literal 19
	offset QError
	offset SemiS

 #	: ?CSP		( GIVE ERR#20 IF CSP & SP DON'T MATCH )
 #	  SP@ CSP @ - LIT 20 ?ERROR
 #	;
 #
 #	I'm not sure I know what this is about; I think it's used occasionally
 #	to verify all of the information put on the stack while compiling has
 #	come back off.

	colon "QCSP","?CSP","QPairs"
	offset SPFetch
	offset CSP
	offset Fetch
	offset Subtract
	literal 20
	offset QError
	offset SemiS

 #	: ?stack		( error if math stack overflow or underflow )
 #
 #	I've stubbed this out because if there's a math stack overflow my
 #	return stack is gonna be hosed anyway.

	colon "QStack","?STACK","QCSP"
	offset SemiS

 #	: IF
 #	  COMPILE 0BRANCH HERE 0 , 2
 #	;*
 #
 #	This word compiles an IF into the code. If drops a conditional
 #	branch followed by space for the offset. The location of the
 #	offset is remembered for future use by ELSE and THEN.

	colon "IF","IF","QStack",Immediate
	offset COMPILE
	offset ZBranch
	offset HERE
	offset Zero
	offset Comma
	offset Two
	offset SemiS

 #	: ELSE
 #	  2 ?PAIRS COMPILE BRANCH HERE 0 , SWAP 2 ENDIF 2
 #	;*
 #
 #	This word tosses in the false part of a conditional. It does two
 #	things:
 #
 #	1)	it compiles a branch at the end of the true part of the
 #		conditional to point at the end of the whole thing.
 #
 #	2)	it resolves the branch left open by IF to point to the
 #		starting of the false code.

	colon "ELSE","ELSE","IF",Immediate
	offset Two
	offset QPairs
	offset COMPILE
	offset BRANCH
	offset HERE
	offset Zero
	offset Comma
	offset SWAP
	offset Two
	offset THEN
	offset Two
	offset SemiS

 #	: THEN
 #	  ?COMP 2 ?PAIRS HERE OVER - SWAP !
 #	;*
 #
 #	This word ends a conditional. It resolves the branch left by either
 #	IF or ELSE to point HERE.
 #
 #	$$$

	colon "THEN","THEN","ELSE",Immediate
	offset QComp
	offset Two
	offset QPairs
	offset HERE
	offset OVER
	offset Subtract
	offset SWAP
	offset Store
	offset SemiS

 #	: BEGIN
 #	  ?COMP HERE 1
 #	;*
 #
 #	This word starts a BEGIN loop. It remembers where the top of the
 #	loop is for use by UNTIL, WHILE, REPEAT, and AGAIN.

	colon "BEGIN","BEGIN","THEN",Immediate
	offset QComp
	offset HERE
	offset One
	offset SemiS

 #	: UNTIL
 #	  1 ?PAIRS COMPILE 0BRANCH O,
 #	;*
 #
 #	Here's the end of a BEGIN...UNTIL thingie. It compiles a branch that
 #	will go back to the top of the loop if the item on top of the math
 #	stack is 0.
 #
 #	$$$

	colon "UNTIL","UNTIL","BEGIN",Immediate
	offset One
	offset QPairs
	offset COMPILE
	offset ZBranch
	offset OComma
	offset SemiS

 #	: AGAIN
 #	  ?COMP
 #	  1 ?PAIRS COMPILE BRANCH O,
 #	;*
 #
 #	COMPILEs an unconditional branch to the beginning of the loop.
 #
 #	$$$

	colon "AGAIN","AGAIN","UNTIL",Immediate
	offset QComp
	offset One
	offset QPairs
	offset COMPILE
	offset BRANCH
	offset OComma
	offset SemiS

 #	: REPEAT
 #	  ?COMP
 #	  >R >R AGAIN R> R> 2 -
 #	  ENDIF
 #	;*
 #
 #	COMPILEs an unconditional branch back to the top of the loop and
 #	then fills in the booger left behind by WHILE.

	colon "REPEAT","REPEAT","AGAIN",Immediate
	offset QComp
	offset ToR
	offset ToR
	offset AGAIN
	offset FromR
	offset FromR
	offset Two
	offset Subtract
	offset THEN
	offset SemiS

 #	: WHILE
 #	  IF 2+
 #	;*
 #
 #	COMPILEs an exit to the loop if the top of stack is zero. REPEAT
 #	fills in the booger left behind by IF.

	colon "WHILE","WHILE","REPEAT",Immediate
	offset IF
	offset TwoPlus
	offset SemiS

 #	: NOP ; ( DO-NOTHING )
 #
 #	A classic that should exist in any language...

	colon "NOP","NOP","WHILE"
	offset SemiS

 #	: QUIT
 #	  [
 #	LABEL Q1
 #	  RP! CR QUERY INTERPRET	( BEGIN )
 #	  STATE @ 0=
 #	  0BRANCH Q2
 #	  (.") "OK"			( IF )
 #	LABEL Q2
 #	  BRANCH Q1			( ENDIF AGAIN )
 #	;
 #
 #	This word is the main loop for the FORTH system. It initializes
 #	the return stack, gets a command line, and interprets it. After
 #	the command line is done, OK is printed if we're not compiling.

	colon "QUIT","QUIT","NOP"
	offset LBracket
Q1:	offset RPStore
	offset CR
	offset QUERY
	offset INTERPRET
	offset STATE
	offset Fetch
	offset ZEq
	offset ZBranch
	offset Q2
	offset PDotQuote
	string "OK"
Q2:	offset BRANCH
	offset Q1
	offset SemiS

 #	: ABORT
 #	  .on
 #	  SP! DECIMAL ?STACK CR
 #	  .CPU				( PRINT THE GREETING )
 #	  ( FORTH )
 #	  QUIT
 #	;
 #
 #	This word starts the ball rolling; the low-level machine code causes
 #	this word to be executed at startup. It initializes the math stack,
 #	prints a banner, and does QUIT to get and execute a command line.
 #
 #	$$$

	colon "ABORT","ABORT","QUIT"
	offset DotOn
	offset SPStore
	offset DECIMAL
	offset QStack
	offset CR
	offset DotCPU
	offset QUIT
	offset SemiS

 #	: .CPU
 #	  (.") "NV5 PCACHE FORTH"
 #	;
 #
 #	This word displays a banner identifying the FORTH system.

	colon "DotCPU",".CPU","ABORT"
	offset PDotQuote
	string "CP/M-VAX FORTH"
	offset CR
	offset SemiS

 #	: INTERPRET
 #	LABEL I1
 #	  -FIND			( BEGIN )
 #	  0BRANCH I2
 #	  STATE @ <		( IF )
 #	  OBRANCH I3
 #	  CFA o,
 #	  BRANCH I4
 #	LABEL I3
 #	  CFA EXECUTE		( ELSE )
 #	LABEL I4
 #	  ?STACK		( ENDIF )
 #	  BRANCH I5
 #	LABEL I2
 #	  HERE NUMBER DPL @ 1+
 #	  0BRANCH I6
 #	  DLITERAL		( IF )
 #	  BRANCH I7
 #	LABEL I6
 #	  DROP LITERAL		( ELSE )
 #	LABEL I7
 #	  ?STACK		( ENDIF ENDIF )
 #	LABEL I5
 #	  BRANCH I1		( AGAIN )
 #	;
 #
 #	Here's the FORTH interpreter. Given a line in TIB, it whacks off
 #	a word from the line and looks it up in the dictionary. If it
 #	can't find the word, it assumes it's a number.
 #
 #	Having located a word, what it does with the word depends on the
 #	state it's in:
 #
 #	- If interpreting, the word is executed.
 #	- If compiling, the word is executed if it's an IMMEDIATE word.
 #	- If compiling, a reference to the word is added to the end of the
 #	  dictionary if it's not an IMMEDIATE word.
 #
 #	Having parsed a number, what it does with the number depends on the
 #	state it's in:
 #
 #	- If interpreting, the number is pushed on the stack.
 #	- If compiling, the number is added as a literal to the end of
 #	  the dictionary.
 #
 #	(all this work is actually done by LITERAL or DLITERAL...)
 #
 #	Strangely enough, INTERPRET seems to believe a number is a double-
 #	precision number if it contains a decimal point. I don't understand
 #	_that_ one...
 #
 #	You have probably noticed that INTERPRET has no exit. What happens 
 #	at the end of a line? Well, I'll tell you, but I'm not happy about
 #	it; this is one of my biggest gripes about FORTH: the whole damn
 #	system is held together by side-effects.
 #
 #	INTERPRET expects to be fed a line that was read in using EXPECT.
 #	EXPECT tosses a null at the end of a line (assuming there's room for
 #	it, of course). ENCLOSE knows that a null ends the line, so it won't
 #	search past a null. WORD therefore sees a one-byte word at the end
 #	of every line containing null. INTERPRET dutifully searches for the
 #	null word. The null word is immediate, so it will be executed even
 #	if we're compiling.
 #
 #	The null word DROPS AN ITEM FROM THE RETURN STACK and then returns.
 #	So, when the null word returns, it doesn't return to INTERPRET. Oh,
 #	no, that would be too good for null. It returns to the word that
 #	called INTERPRET.
 #
 #	OK, suppose I type something that's not in the dictionary. What
 #	happens? Well, NUMBER sees that (NUMBER) gave up on something that's
 #	not a space or a decimal point, so it ERRORs out. ERROR calls QUIT,
 #	which initializes the return stack and does a new QUERY/INTERPRET.
 #	In short, INTERPRET gets aborted.
 #
 #	What a mess!!!

	colon "INTERPRET","INTERPRET","DotCPU"
I1:	offset MinusFind	# ( pfa length 1 ) or ( 0 )
	offset ZBranch		# If not found, go
	offset I2
	offset STATE
	offset Fetch		# ( pfa state )
	offset Less
	offset ZBranch		# Go if not ( pfa )
	offset I3
	offset CFA		# ( cfa )
	offset OComma		# COMPILE CFA into word
	offset BRANCH
	offset I4
I3:	offset CFA		# ( pfa -- cfa )
	offset EXECUTE		# do it
I4:	offset QStack		# Check for overflow
	offset BRANCH
	offset I5
I2:	offset HERE		# Couldn't find the word; is it a number?
	offset NUMBER		# ( low high )
	offset DPL		# Is it double precision?
	offset Fetch		# ( low high dpl )
	offset OnePlus
	offset ZBranch		# Go if it's single precision
	offset I6
	offset DLITERAL		# It's double precision
	offset BRANCH
	offset I7
I6:	offset DROP		# ( low )
	offset LITERAL
I7:	offset QStack
I5:	offset BRANCH
	offset I1
	offset SemiS		# Like we'll ever get here...

 #	: exit		( exits current word )
 #	  R> DROP ;
 #
 #	This is an ACE word to exit the current word. It can't be used in
 #	a DO loop, as it just drops its return address from the stack causing
 #	a return to the caller's caller.

	colon "EXIT","EXIT","INTERPRET"
	offset FromR
	offset DROP
	offset SemiS

 #;	Breakpoint opportunity: a machine code word that gives
 #;	VAX DEBUG have a symbol it can break at
 #
 #	Code BPT
 #BreakHere::
 #	JMP (R11)

 #	User Variables. These are intended to be addressed by an offset
 #	to the "user pointer" to facilitate multitasking. I'm just making
 #	them normal variables.

	variable "PTib","(TIB)","EXIT"
	variable "TIB","TIB","PTib"	# Terminal input goes here
	variable "DP","DP","TIB"	# End of directory
	variable "IN","IN","DP"		# Current offset INto TIB
	variable "CURRENT","CURRENT","IN" # NFA of word we're workin' on
	variable "STATE","STATE","CURRENT" # 0 = interpreting?
	variable "BASE","BASE","STATE"	# Current number base
	variable "DPL","DPL","BASE" # 1 = NUMBER decided it's double-precision
	variable "CSP","CSP","DPL"	# Used as a temp to wind back stack?
	variable "HLD","HLD","CSP"	# Buffer for formatted output
	variable "BSlashBuf","B/BUF","HLD" # Size of TIB

	constant "BL","BL","BSlashBuf",32 # Space

	constant "PAD","PAD","BL",0	# This isn't really a contant; it's 
					# initialized by COLD and never written
					# by FORTH code.

 #	These constants are used frequently enough in the code that it
 #	makes sense (i.e., saves bytes) to make them words, even with the
 #	ZBLIT hack.

	constant "Zero","0","PAD",0
	constant "One","1","Zero",1
	constant "Two","2","One",2

	code "HALT","HALT","Two"
	halt
	jmp (r11)

#	BASEPAGE	-- address of base page
#
#	Pushes the address of the base page on the stack.

	colon "BASEPAGE","BASEPAGE","HALT"
	offset OLIT
	offset __base_page
	offset SemiS

#	BDOS		parameter, function -- result
#
#	Calls the BDOS, pushing the return value.

	code "BDOS","BDOS","BASEPAGE"
	movl r7,r0		# form address of parameter
	addb2 $4,r0
	movl (r0),-(sp)		# push parameter
	movl (r7),-(sp)		# push func
	movl r0,r7		# discard func
	movl __bdos_pointer,r0	# call BDOS
	calls $2,(r0)
	movl r0,(r7)		# Replace parameter with return value
	jmp (r11)


#	DOES>		--
#
#	Compiles a trampoline that allows the behavior of a created
#	word to be specified in FORTH. Compilation continues after the
#	trampoline is compiled to allow definition of the behavior.

	colon "Does","DOES>","BDOS",Immediate


#	What we're trying to do is compile the following sequence onto
#	the end of the word we're building:
#
#	(;CODE) 8		( point CFA of new word at trampoline )
#	;S			( Exit the creating word )
#
#	followed by the trampoline.

	offset OLIT		# Compile reference to (;CODE)
	offset PSemiCode
	offset OComma

	literal 8		# compile the 4
	offset Comma

	offset OLIT		# Compile reference to ;S
	offset SemiS
	offset OComma

#	Now do the trampoline. It consists of the following
#	code:
#
#	82 04 57	subb2 $4,r7	# Make space on the math stack
#	de a9 04 67	moval 4(r9),r7	# put PFA on math stack
#	d0 5a 7e	movl r10,-(sp)	# save return address
#	de af 03 5a	moval 1f,r10	# point IP at DOES> tail
#	17 6b		jmp (r11)	# NEXT
#			1:		# DOES> tail starts here
#	As longwords, that's:
#
#	de570482
#	d06704a9
#	afde7e5a
#	6b175a03

	literal 0xde570482
	offset Comma
	literal 0xd06704a9
	offset Comma
	literal 0xafde7e5a
	offset Comma
	literal 0x6b175a03
	offset Comma

#	And we're done

	offset SemiS
	
#	FCB		--
#
#	Creates an FCB, initializing it to a blank filename.

	colon "FCB","FCB","Does"
	offset CREATE

#	Make the FCB refer to the default drive

	literal 0
	offset CComma

#	Make spaces for the filename.ext

	literal 0x20202020
	offset Comma
	literal 0x20202020
	offset Comma

	literal 0x2020
	offset WComma
	literal 0x20
	offset CComma

#	Clear the reserved bytes

	offset Zero
	offset Comma

#	Clear the allocation map

	offset Zero
	offset Comma
	offset Zero
	offset Comma
	offset Zero
	offset Comma
	offset Zero
	offset Comma

#	Clear current record (1 byte) and random record (3 bytes)

	offset Zero
	offset Comma

#	Behave like a variable: 
#
#		DOES> ;
#
#	which leaves a copy of the PFA on the stack.

	does
	offset SemiS
	
Forth$$LastWord_NFA:

	code "COLD","COLD","FCB"

 #	When we are started, the BIOS/Loader/whatever enters here

Forth$$Begin:
	moval Next,r11
	moval TibBuffer,PTib_pfa
	moval TibSpace,TIB_pfa
	moval TibSpaceEnd,r0
	moval TibSpace,r1
	subl3 r0,r1,BSlashBuf_pfa
 #	movl $(TibSpaceEnd - TibSpace),BSlashBuf_pfa
	moval DictEnd,DP_pfa
	clrl IN_pfa
	moval Forth$$LastWord_NFA,CURRENT_pfa
	clrl STATE_pfa
	movl $10,BASE_pfa
	clrl DPL_pfa
	moval PadSpaceEnd,PAD_pfa
	moval MathSpace,r7	# initialize math stack pointer
	
	moval ABORT+4,r10
	jmp (r11)

DictEnd:
	.end
