	.text
	subb2 $4,r7
	moval 4(r9),(r7)
	movl r10,-(sp)
	moval 1f,r10
	jmp (r11)
1:
	.end
