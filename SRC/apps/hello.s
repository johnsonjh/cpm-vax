	.text
	.word 0		# entry mask

#	print "Hello world!"

	moval message,-(sp)
	movl $9,-(sp)
	movl 4(ap),r0
	movl (r0),r0
	calls $2,(r0)

	ret

message: .ascii "Hello world!$"

	.end
