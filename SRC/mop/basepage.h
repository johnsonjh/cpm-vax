/***
 *
 * VAX_BasePage_t
 *
 *	This type declares the base page, which contains information
 *	describing the environment in which a program loaded into the
 *	TPA finds itself. 
 *
 *	The fields contained herein are a mixture of those offered
 *	by CP/M-68K and by CP/M-80. Since addresses are 32-bits on
 *	the VAX, we cannot simply clone the CP/M-80 base page. Since
 *	we are not (currently) using trap instructions to get to the
 *	BDOS, we have to provide a way for the program to find the
 *	BDOS entry point; we cannot simply clone the CP/M-68K base
 *	page.
 *
 *	Although I'm currently only supporting a .COM-style program
 *	structure, I'm keeping some of the more sophisticated fields
 *	offered by CP/M-68K in the hope that I'll eventually get around
 *	to doing a more sophisticated program loader.
 *
 *	This structure lies at the bottom of the TPA; the program
 *	is loaded above it. The program is also passed a pointer to
 *	the base page.
 *
 * revisions:
 *
 *	2005-03-04 rli: original attempt; version 0.
 *
 *	2005-04-01 rli: My bad; FCBs are 36 bytes long, not 35.
 *
 *	2005-04-09 rli: Botched reserved field before CommandTail.
 *
 * fields:
 *
 *	- Bdos: A pointer to the BDOS entry point. 
 *
 *	- TpaBase: The base address of the TPA. This is also the
 *	  starting address of this structure.
 *
 *	- TpaTop: The address immediately following the TPA. The
 *	  byte pointed to by this field is NOT in the TPA, but is
 *	  the byte immediately following the TPA. 
 *
 *	- Entry: A pointer to the program entry point.
 *
 *	- InitialStackPointer: The value that should be loaded into
 *	  the stack pointer by the Enter function before the
 *	  program is entered.
 *
 *	- TextBase: The starting address of the .TEXT segment. With
 *	  the .COM program loader, this is also the program entry
 *	  point.
 *
 *	- TextSize: The length, in bytes, of the .TEXT segment.
 *
 *	- DataBase: The starting address of the .DATA segment. For
 *	  the .COM program loader, the entire program is assumed to
 *	  reside in the .TEXT segment. This field therefore receives
 *	  the address of the byte following the .TEXT segment.
 *
 *	- DataSize: The length, in bytes, of the .DATA segment. For the
 *	  .COM-style program loader, this is zero.
 *
 *	- BssBase: The starting address of the .BSS segment. For the
 *	  .COM-style program loader, the entire program is assumed to
 *	  reside int he .TEXT segment. This field therefore receives
 *	  the address of the byte following the .TEXT segment.
 *
 *	- BssSize: The length, in bytes, fo the .BSS segment. For the
 *	  .COM-style program loader, this is zero.
 *
 *	- Entry: A pointer to the program entry point. For the
 *	  .COM-style program loader, this is the base of the .TEXT
 *	  segment.
 *
 *	- LoadedFrom: The number of the drive from which the program was
 *	  loaded.
 *
 *	- Fcbs: Two FCBs parsed from the command line. Fcbs[ 0 ] is the
 *	  first, while Fcbs[ 1 ] is the second.
 *
 *	- FirstFcb: The first parsed FCB from the command line.
 *
 *	- CommandTail: The command tail and default DMA buffer.
 *
 ***/

typedef struct VAX_BasePage_s {
  short int (*Bdos)(			/* :0x000 */
    unsigned short int func,
    unsigned long int parm );
  void *TpaBase;			/* :0x004 */
  void *TpaTop;				/* :0x008 */
  void (*Entry)();			/* :0x00c */
  void *InitialStackPointer;		/* :0x010 */
  void *TextBase;			/* :0x014 */
  unsigned int TextSize;		/* :0x018 */
  void *DataBase;			/* :0x01c */
  unsigned int DataSize;		/* :0x020 */
  void *BssBase;			/* :0x024 */
  unsigned int BssSize;			/* :0x028 */
  unsigned char LoadedFrom;		/* :0x02c */
  unsigned char Reserved1[ 3 ];
  unsigned char Fcbs[ 2 ][ 36 ];	/* :0x030 */
  unsigned char Reserved2[ 8 ];         /* :0x07c */
  unsigned char CommandTail[ 128 ];	/* :0x080 */
  unsigned char Reserved3[ 256 ];	/* :0x100 */
} VAX_BasePage_t;			/* :0x200 */

/***
 *
 * VAX_LoadParameterBlock_t
 *
 *	This data structure is used for the Program Load function, which
 *	is implemented in the BIOS. A pointer to a structure of this
 *	type is passed into the function. The information about the
 *	memory region into which the program is to be loaded is used
 *	by the function, which returns the information about the
 *	location of the base page and the entry point.
 *
 *	The intent is that the base page will be located at the bottom
 *	of the load area *after* the specified address has been rounded
 *	up to the next VAX page. The base page will be constructed
 *	such that the TPA in which it resides is as specified by the
 *	load parameter block *after* the start address has been rounded
 *	up to the next VAX page and the ending address has been rounded
 *	down to the previous VAX page. The loading process reads the
 *	image and fills in the following fields of the base page:
 *
 *	- TpaBase, which receives the address of the base page.
 *	- TpaTop, which receives the address of the byte in the
 *	  first page following the memory area into which the
 *	  program has been loaded.
 *	- TextBase
 *	- TextSize
 *	- DataBase
 *	- DataSize
 *	- BssBase
 *	- BssSize
 *
 *	The remainder of the fields must be filled in by the caller
 *	before the base page can be handed off to the Enter function
 *	(for which there is no analogue in either CP/M-68K or CP/M-80)
 *	to start the program running.
 *	
 *	The .COM-style program loader only supports loading programs
 *	into a TPA beginning at zero. The size of the .BSS section cannot
 *	be determined from a .COM-style file, so a program that uses
 *	a .BSS section cannot rely on the BssSize or FreeSpace fields
 *	of the base page; such a program needs to make other
 *	arrangements to determine how much memory is available.
 *
 * revisions:
 *
 *	2005-03-05 rli: original attempt.
 *
 * fields:
 *
 *	- Fcb: A pointer to the FCB from which the program is to be
 *	  read.
 *
 *	- TpaBase: The base address of the memory region into which the
 *	  program is to be loaded. This is rounded up to the next VAX
 *	  page boundary before the program is loaded.
 *
 *	- TpaTop: The address of the byte immediately following the
 *	  memory region into which the program is to be loaded. This
 *	  is rounded down to the previous VAX page boundary before the
 *	  program is loaded.
 *
 *	- BasePage: Receives a pointer to the base page for the program.
 *
 *	- Flags: Loader control flags. CP/M-68K defines the following flags:
 *
 *	  - Bit 0: The program is loaded into the bottom of the
 *	    TPA when clear and the top when set. The .COM-style
 *	    program loader does not support this bit.
 *
 ***/

typedef struct VAX_LoadParameterBlock_s {
  unsigned char *Fcb;			/* :0x00 */
  void *TpaBase;			/* :0x04 */
  void *TpaTop;				/* :0x08 */
  VAX_BasePage_t *BasePage;		/* :0x0c */
  unsigned int Flags;			/* :0x10 */
} VAX_LoadParameterBlock_t;


/***
 *
 * VAX_BiosParameterBlock_t
 *
 *	A pointer to a structure of this type is passed to BDOS function
 *	50, DIRECT BIOS CALL. It provides the information needed to call
 *	a BIOS function, to wit: the function number and a parameter to
 *	be passed to the function.
 *
 * revisions:
 *
 *	2005-03-05 rli: original attempt.
 *
 * fields:
 *
 *	- Function: The number of the BIOS Function to be called.
 *
 *	- Parameter1: The first parameter to be passed to that function, if
 *	  one is needed.
 *
 *	- Parameter2: The second parameter to be passed to that
 *	  function, if one is needed.
 *
 *	- ReturnValue: Receives a copy of the return value from the
 *	  BIOS function. This allows 32-bit values to be returned
 *	  within the BDOS' 16-bit return value limit. 
 *
 ***/

typedef struct VAX_BiosParameterBlock_s {
  unsigned int Function;		/* :0x00 */
  unsigned long int Parameter1;		/* :0x04 */
  unsigned long int Parameter2;		/* :0x08 */
  unsigned long int ReturnValue;	/* :0x0c */
} VAX_BiosParameterBlock_t;

