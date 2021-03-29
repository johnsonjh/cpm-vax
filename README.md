# CP/M-VAX

This is a port of CP/M-68K to the MicroVAX 2000. It combines the system-independent portion of CP/M-68K (which was originally written in C) with support code to make it work on a VAX, specifically the MicroVAX 2000. Currently, the system is downloaded via the Ethernet using MOP and uses a RAMdisk included as part of the image.

## What you need

In order to run CP/M-VAX, you will need:

* A MicroVAX 2000 with at least 2MB of RAM.
* A serial console connected via a BCC08 (or equivalent) cable.
* A host from which the image may be booted. I'm using NetBSD/x86 with mopd. 
* The system image, [BIN/cpm-vax.bin](BIN/cpm-vax.bin).

As you can well imagine, there are not many applications available for CP/M-VAX. In the disk image, you will find two: CFORTH.VAX and CIRC4TH.VAX. Both are non-standard FORTH systems I created to allow me to fiddle with CP/M-VAX. Neither are very exciting.

It's been a while since I played with this, but I'm pretty certain CIRC4TH.VAX was the one I was using. It's a bit more robust than CFORTH.VAX because it uses a circular math stack, making it impossible for the stack to go wandering off into memory and corrupting the system.

## Running CP/M-VAX

You need to arrange for your host system to boot cpm-vax.bin for your MicroVAX 2000. Since I'm using mopd on NetBSD, this involves placing the file in /tftpboot/mop using a filename derived from the Ethernet MAC address of the target. Here's what a session looks like from copying the image to running CIRC4TH:

```text
stench> cp cpm-vax.bin /tftpboot/mop/08002b03f1c4.SYS
stench> kermit
C-Kermit 8.0.209, 17 Mar 2003, for NetBSD
 Copyright (C) 1985, 2003,
  Trustees of Columbia University in the City of New York.
Type ? or HELP for help.
(/home/rivie/booger/) C-Kermit>set line /dev/tty01
(/home/rivie/booger/) C-Kermit>set baud 9600
/dev/tty01, 9600 bps
(/home/rivie/booger/) C-Kermit>set carrier off
(/home/rivie/booger/) C-Kermit>c
Connecting to /dev/tty01, speed 9600
 Escape character: Ctrl-\ (ASCII 28, FS): enabled
Type the escape character followed by C to get back,
or followed by ? to see other options.
----------------------------------------------------



KA410-A V1.2           

F...E...D...C...B...A...9...8...7...6...5...4_..3_..2_..1...


 ?  E  0040  0000.0045
 ?  D  0050  0000.0005
 ?  C  0080  0000.4001
 ?  6  00A0  0400.6001


 83 BOOT SYS
-ESA0
CP/M-68K(tm), Version 1.2, Copyright (c) 1984, Digital Research
A>dir
A: CFORTH   VAX : CIRC4TH  VAX 
A>circ4th

CP/M-VAX FORTH

VLIST
COLD FCB DOES> BDOS BASEPAGE HALT 2 1 0 PAD BL B/BUF HLD CSP DPL BASE
STATE CURRENT IN DP TIB (TIB) EXIT INTERPRET .CPU ABORT QUIT NOP WHILE
REPEAT AGAIN UNTIL BEGIN THEN ELSE IF ?STACK ?CSP ?PAIRS ?EXEC ?COMP
?ERROR ERROR (FIND) EXECUTE WORD ' VLIST FORGET IMMEDIATE [COMPILE]
COMPILE CREATE LATEST SMUDGE PFA NFA CFA LFA ( -FIND (;CODE) EDOC CODE
VARIABLE CONSTANT ; : ] ;S ID. EXPECT U. ? . D. .R D.R #S # SIGN #> <#
SPACES NUMBER (NUMBER) HOLD ENCLOSE QUERY ." (.") COUNT TYPE DIGIT
DECIMAL HEX SPACE CR ?TERMINAL .ON .OFF EMIT ?KEY KEY O, C, W, , ALLOT
HERE DLITERAL LITERAL OLIT ZBLIT LIT BLANKS ERASE FILL CMOVE W! CVTWL@
-- ++ 2! C! ! 2@ C@ @ M/MOD M/ M* S->D U/ U* DABS D+- DMINUS D- D+ >> <<
*/ */MOD MOD / /MOD * MAX MIN ABS +- 1- 2+ 1+ NOT MINUS + <> > U< != =
0< 0= TOGGLE +! XOR OR AND < - !CSP -DUP SP! SP@ 2DUP DROP DUP OVER ROT
SWAP RP! RP@ R R> >R I LEAVE +LOOP LOOP DO (DO) (+LOOP) (LOOP) 0BRANCH
BRANCH  OK
0 BDOS

A>
```

Like I said, not terribly exciting.

## Source code

The sources are derived from CP/M-68K. I have made a few minor changes to the system to allow compilation using a modern C compiler. I was using GCC 2.95.3 built as a cross-compiler for NetBSD/VAX. However, I have been unable to build GCC 2.95.3 since I have upgraded my PC to NetBSD 3.0. Consequently, I am currently unable to rebuild the system.

The sources, as of the last time I quit fiddling with it [2006-Feb-24], are reproduced here. I believe they survived my recent disk crash, but (since I can't currently rebuild them), I am not absolutely certain of that.

When you unpack that tar file, you will find four subdirectories:

* apps contains the source code for the few applications I have. These include CFORTH (built from nv32forth.s) and CIRC4TH (build from circforth.s) as well as a few other little things like a "Hello world!" program and whatnot. Not everything in this directory works. I fiddled with the system interface a bit, and I'm not certain what is good and what is not.

* exchange-0.1 contains a program that allows manipulation of CP/M disk images. This is a version of CP/M-68K configured to allow it to run under Unix. I use this to manipulate the 8" SSSD disk image included as part of the system executable.

* mop contains the system source. The disk image is bdosPLM.img; if you want to add files to or remove files from the built-in disk image, this is the file to manipulate.

* tools contains a few tools needed to build the system. The important ones are:  * mkimage, which converts a binary file to a C character array initialization containing the file. This is used to include the disk image into the system. 
  * mkasmimage, which converts a binary file to assembly source. This is the result of a number of arguments with binutils, gcc, and mopcopy that I lost. 

  Another required tool is mopcopy, included with NetBSD, which attaches a MOP boot block to an image. Part of the purpose of mkasmimage is to hide some details of the system so that mopcopy will just throw up its hands in disgust and enter the image at the beginning.

## Conclusion

Yeah, I know it's all ugly, but I never got around to cleaning it up. Some of the particularly strange bits in the Makefile are due to a problem I was having with VAX binutils; objcopy seemed to want to emit at least 512 bytes for an .aligned section, so I had to pull out the sections individually and paste them together.

 - Roger Ivie
