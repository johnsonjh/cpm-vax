/************************************************************************
 *
 * file: arch.c
 *
 *	This file contains routines to manage architecture-specific
 *	issues for the operating system. Among them are:
 *
 *	- A couple of library routines used by the OS that were
 *	  supplied in assembly language.
 *
 *	- Dispatch of calls to the BDOS, for which a couple of functions
 *	  were peeled off by the trap handler before being passed on
 *	  to the BDOS proper (and we're taking that opportunity to add
 *	  a BDOS function).
 *
 *	- System initialization.
 *
 *	- Loading of programs.
 *
 *	- The portion of exception handling that can be done in C.
 *
 * revisions:
 *
 *	2005-03-07 rli: Split off from bios.c.
 *
 ************************************************************************/ 
 
/***
 *
 *	PROTOTYPES
 *
 ***/

unsigned short int bdos( unsigned short int func, unsigned long int parm);
extern void start( void );

/***
 *
 * bios_outzstring
 *
 *	This routine displays a null-terminated string using only the
 *	BIOS CONOUT service. It is used for diagnostics and to display
 *	an error message on an unhandled exception.
 *
 * revisions:
 *
 *	2005-03-07 rli: Added these comments.
 *
 * formal parameters:
 *
 *	- Victim: A pointer to the string to be displayed.
 *
 * informal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	none.
 *
 * side effects:
 *
 *	none.
 *
 ***/

void bios_outzstring( unsigned char *Victim )
{
  while( *Victim ) {
    bios_conout( *Victim );
    Victim++;
  }
}

/***
 *
 * bios_outhex
 *
 *	This diagnostic function is used to display a hexadecimal
 *	integer, if necessary.
 *
 * revisions:
 *
 *	2005-03-07 rli: Added these comments.
 *
 * formal parameters:
 *
 *	- Victim: The value to be displayed.
 *
 * informal parameters:
 *
 *	none
 *
 * return value:
 *
 *	none
 *
 * side effects:
 *
 *	none
 *
 ***/

void bios_outhex( unsigned long int Victim )
{
  static unsigned char Digit[] = "0123456789abcdef";
  int Count;

  /* I'm sure you know the drill; a nybble at a time beginning
   * with the most-significant.
   */

  for( Count = 0; Count < 8; Count++ ) {
    bios_conout( Digit[ Victim >> 28 ] );
    Victim = Victim << 4;
  }
}

/***
 *
 * udiv
 *
 *	This routine is used by the BDOS to do some division,
 *	simultaneously obtaining both the quotient and the remainder.
 *
 * revisions:
 *
 *	2005-03-07 rli: Added these comments.
 *
 * formal parameters:
 *
 *	- dividend: The dividend.
 *
 *	- divisor: The divisor.
 *
 *	- remainder: A pointer to somewhere to store the remainder.
 *
 * informal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	the quotient.
 *
 * side effects:
 *
 *	none.
 *
 ***/

unsigned short int udiv( 
  signed long int dividend, 
  unsigned short int divisor,
  unsigned short int *remainder )
{
  *remainder = dividend % divisor;
  return dividend / divisor;
}

/***
 *
 * swap
 *
 *	This routine is used by the BDOS to swap words read from the
 *	disk (e.g., from the allocation map) into host byte order.
 *
 *	The code contained herein figures out what type of swapping
 *	should be performed the first time it is called. This can
 *	be hard-coded for a particular system, if you like.
 *
 * revisions:
 *
 *	2005-03-07 rli: Added these comments
 *
 * formal parameters:
 *
 *	- victim: The word to be swapped.
 *
 * informal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	The swapped word.
 *
 * side effects:
 *
 *	none.
 *
 ***/

unsigned short int swap( unsigned short int victim )
{
  static int swaptype= -1;
  static unsigned short int testpattern=0x0102;
  unsigned short int temp;

  if( swaptype < 0 ) {
    if( *(char *)&testpattern == 0x01 ) {
      swaptype = 1;
    } else {
      swaptype = 0;
    }
  }

  if( swaptype == 0 ) return victim;

  temp = ( (victim & 0xff ) << 8 ) |
    ( ( victim & 0xff00 ) >> 8 );
  return temp;
}

/***
 *
 * Miscellaneous CCP variables.
 *
 *	These variables are, for some reason, declared and initialized
 *	outside the CCP. While I can see initializing some of them,
 *	I don't see why they are actually declared outside the CCP.
 *
 *	For the most part, I don't yet know what they do.
 *
 ***/ 

unsigned char submit = 0;
unsigned char morecmds = 0;
unsigned char autost = 0;
signed char usercmd[ 130 ];

#include "basepage.h"
/***
 *
 * entry
 *
 *	This routine does initial system startup. It is invoked in the
 *	following cases:
 *
 *	- Initial system boot.
 *	- Invocation of bios function 0: COLD BOOT
 *	- Invocation of bios function 1: WARM BOOT
 *
 *	Since the state of the stack is unknown (we may have been 
 *	called from a program loaded into the TPA), the first thing
 *	we need to do is initialize the stack. This has several
 *	implications: we cannot refer to parameters, automatic
 *	variables, or the return address once we've initialized
 *	the stack.
 *
 * revisions:
 *
 *	2005-03-05 rli: Rework to initialize the stack pointer.
 *
 * formal parameters:
 *
 *	none.
 *
 * informal parameters:
 *
 *	- start: The value that should be loaded into the stack pointer.
 *
 * return value:
 *
 *	does not return.
 *
 * side effects:
 *
 *	Re-initializes the system and enters the CCP.
 *
 ***/ 

void entry( void )
{
  static unsigned short int diskuser;

  /* Initialize the stack pointer.
   */

  asm( "moval _start,sp" );

  /* Initialize the BIOS. This returns a word containing the boot
   * drive number and default user number in the low byte.
   */

  diskuser = bios_boot();

  /* Initialize the BDOS.
   */

  bdosinit();

  /* Set the default drive and user number to the values returned
   * by the BIOS initialization routine.
   */

  bdos( 32, diskuser >> 8 );
  bdos( 14, diskuser & 0xff );

  /* Enter the CCP. If it returns, start over.
   */

  ccp();
  entry();
}

/***
 *
 * bdos_DirectBiosCall
 *
 *	This routine implements BDOS function 50, DIRECT BIOS CALL,
 *	which allows a transient program to call a BIOS function.
 *
 * revisions:
 *
 *	2005-03-05 rli: original version.
 *
 * formal parameters:
 *
 *	- BiosParameterBlock: A pointer to the VAX_BiosParameterBlock_t
 *	  that describes the function to be called.
 *
 * informal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	0 - The function was called.
 *	1 - Invalid or unsupported function number.
 *
 * side effects:
 *
 *	Depends upon the BIOS function called.
 *
 ***/

unsigned short int bdos_DirectBiosCall( 
  VAX_BiosParameterBlock_t *BiosParameterBlock )
{
  switch( BiosParameterBlock->Function ) {
    case 0: /* cold boot */
    case 1: /* warm boot */
      entry();
    case 2: /* console status */
      BiosParameterBlock->ReturnValue = bios_const();
      break;
    case 3: /* console input */
      BiosParameterBlock->ReturnValue = bios_conin();
      break;
    case 4: /* console output */
      bios_conout( (unsigned char)BiosParameterBlock->Parameter1 );
      break;
    case 5: /* list output */
      bios_list( (unsigned char)BiosParameterBlock->Parameter1 );
      break;
    case 6: /* auxiliary output */
      bios_punch( (unsigned char)BiosParameterBlock->Parameter1 );
      break;
    case 7: /* auxiliary input */
      BiosParameterBlock->ReturnValue = bios_reader();
      break;
    case 8: /* restore current drive to track 0 */
      bios_home();
      break;
    case 9: /* select disk */
      BiosParameterBlock->ReturnValue = ( unsigned long int)bios_seldsk(
        (unsigned char)BiosParameterBlock->Parameter1,
        (unsigned char)BiosParameterBlock->Parameter2 );
      break;
    case 10: /* set track number */
      bios_settrk( (unsigned short int)BiosParameterBlock->Parameter1 );
      break;
    case 11: /* set sector number */
      bios_setsec( (unsigned short int)BiosParameterBlock->Parameter1 );
      break;
    case 12: /* set dma address */
      bios_setdma( (void *)BiosParameterBlock->Parameter1 );
      break;
    case 13: /* read sector */
      BiosParameterBlock->ReturnValue = bios_read();
      break;
    case 14: /* write sector */
      BiosParameterBlock->ReturnValue = bios_write(
        (unsigned short int)BiosParameterBlock->Parameter1 );
      break;
    case 15: /* list status */
      BiosParameterBlock->ReturnValue = bios_listst();
      break;
    case 16: /* sector translate */
      BiosParameterBlock->ReturnValue = bios_sectran(
        (unsigned short int)BiosParameterBlock->Parameter1,
        (unsigned short int *)BiosParameterBlock->Parameter2 );
      break;
    case 17: /* there is no function 17 */
      return 1;
    case 18: /* get address of memory region table */
      BiosParameterBlock->ReturnValue = (unsigned long int)bios_getmrt();
      break;
    case 19: /* get i/o byte */
      BiosParameterBlock->ReturnValue = bios_getiobyte();
      break;
    case 20: /* set i/o byte */
      bios_setiobyte( (unsigned short int)BiosParameterBlock->Parameter1);
      break;
    case 21: /* flush buffers */
      bios_flush();
      break;
    case 22: /* set exception handler address */
      return 1; /* not currently supported */
    default:
      return 1;
  }
  return 0;
}

/***
 *
 * bdos_EnterProgram
 *
 *	BDOS FUNCTION 64 ENTER PROGRAM
 *
 *	This routine enters a program that has been loaded via
 *	BDOS function 59, LOAD PROGRAM. Given the address of the
 *	base page, it initializes the stack and enters the program.
 *	Upon return, it restores the stack pointer and exits.
 *
 *	When the program is entered, the stack contains the original
 *	stack pointer (so this routine can find the original stack)
 *	followed by the stack frame for the program entry point.
 *
 * revisions:
 *
 *	2005-03-05 rli: original attempt.
 *
 * formal parameters:
 *
 *	- BasePage: Pointer to the base page describing the program to be
 *	  entered.
 *
 * informal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	Zero.
 *
 * side effects:
 *
 *	Depends on the program.
 *
 ***/

unsigned short int bdos_EnterProgram( VAX_BasePage_t *BasePage )
{

  /* Set up the new stack pointer and save a copy of the old stack
   * pointer on it, so we can find it if we get back here.
   */

  asm( "movl sp,r0" );
  asm( "movl r0,-(sp)" );

  /* Track down and call the entry point. We might be able to get
   * away with doing this in C, but I'm not absolutely certain of
   * that.
   */

  asm( "movl 4(ap),r0" );	/* Fetch BasePage */
  asm( "movl r0,-(sp)" );	/* Pass it on to the program. */
  asm( "movl 12(r0),r0" );	/* Fetch the entry point. */
  asm( "calls $1,(r0)" );	/* Call it. */

  /* If we get back here, the program exited by returning. We need
   * to restore the original stack pointer so we can return.
   */

  asm( "movl (sp)+,r0" );	/* I'm not sure I can trust movl (sp)+,sp */
  asm( "movl r0,sp" );

  return 0;
}
   
/***
 *
 * bdos
 *
 *	A user program that calls the BDOS goes through here. Among
 *	other things, this gives us a whack at the call so we can peel
 *	off functions performed here (such as 50 DIRECT BIOS CALL). 
 *	If it's not a function performed by the BIOS, it is passed on to 
 *	the actual BDOS.
 *
 * revisions:
 *
 *	2005-03-05 rli: original attempt.
 *
 * formal parameters:
 *
 *	- func: The number of the BDOS function to be performed.
 *
 *	- parm: The parameter to be passed to that function, if one is
 *	  is needed. 
 *
 * informal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	whatever is returned by the BDOS call.
 *
 * side effects:
 *
 *	depends on the BDOS call.
 *	
 ***/

unsigned short int bdos( unsigned short int func, 
  unsigned long int parm )
{
  switch( func ) {
    case 50: /* direct bios call */
      return bdos_DirectBiosCall( 
        (VAX_BiosParameterBlock_t *)parm );
    case 64: /* enter program */
      return bdos_EnterProgram(
        (VAX_BasePage_t *)parm );
    default:
      return _bdos( func, (unsigned short int)parm, (void *)parm );
  }
}

/***
 *
 * pgmld
 *
 *	BDOS FUNCTION 59: LOAD PROGRAM
 *
 *	This routine implements BDOS function 59, which loads a program
 *	into a specified region of memory.
 *
 *	This initial implementation deals only with .COM-style
 *	executables, files that contain a dump of memory beginning
 *	at absolute address 0x200. The file is read into memory and
 *	base page variables relating to that file are initialized.
 *
 * revisions:
 *
 *	2005-03-05 rli: original version.
 *
 * formal parameters:
 *
 *	- infop: A pointer to a VAX_LoadParameterBlock_t describing the
 *	  memory region into which the program is loaded.
 *
 *	- dmaaddress: The current default DMA address. This is used
 *	  primarily to restore that default after the program is
 *	  loaded.
 *
 * informal parameters:
 *
 *	- start: This is the base of the operating system. If the
 *	  top of the memory region is within a page of this address,
 *	  the call fails. This is intended to ensure there is some
 *	  stack space available for the system.
 *
 *	- userentry: The routine that a program in the TPA calls to
 *	  get to the BDOS. This is located in boot.s to ensure all
 *	  registers are properly saved.
 *
 * return value:
 *
 *	- 0: Success. The program has been loaded and may be entered
 *	  after the remainder of the base page has been initialized.
 *
 *	- 1: Insufficient memory to load the program, a bad program
 *	  header (not applicable here), or the load parameter
 *	  block is not acceptable.
 *
 *	- 2: A read error occurred while loading the file into memory.
 *	     (except that the BDOS doesn't report disk errors...)
 *
 *	- 3: Bad relocation bits exist in the program file (not
 *	  applicable here).
 *
 * side effects:
 *
 *	Lots of BDOS calls are made.
 *
 ***/

unsigned short int pgmld( void *infop, void *dmaaddress )
{
  VAX_LoadParameterBlock_t *lpb = (VAX_LoadParameterBlock_t *)infop;
  unsigned short int ReadStatus;
  unsigned char *NextSector;
  unsigned char *TpaTop;
  extern void userentry( unsigned short int func, unsigned long int parm );

  /* Check the load parameter block to make certain it is acceptable.
   * We're kind of stupid, so we can only load programs into regions 
   * beginning at address 0.
   */

  if( lpb->TpaBase != (void *)0 ) {
    return 1;
  }

  /* Round the top address of the region down to the previous VAX page
   * and make certain we won't get too close to the base of the system
   * we're reserving a page below the system for the default system
   * stack).
   */

  TpaTop = (unsigned char *)( 
    ( (unsigned long int)lpb->TpaTop ) 
    & ~0x1ff);

  if( ( (unsigned long int)TpaTop ) >
    ( ( (unsigned long int)&start ) - 512 ) ) {
    return 1;
  }

  /* The TPA region looks OK. Initialize the base page pointer.
   */

  lpb->BasePage = (void *)lpb->TpaBase;

  /* Load the file into the TPA a sector at a time.
   */

  for( NextSector = (unsigned char *)0x200; 
    NextSector < TpaTop; NextSector += 128 ) {

    /* First, set the DMA address to the place we want to store this
     * sector.
     */

    bdos( 26, (unsigned long int)NextSector );

    /* Read the sector.
     */

    ReadStatus = bdos( 20, (unsigned long int)lpb->Fcb );
    if( ReadStatus != 0 ) break;

  }

  /* We have left the read loop. If we haven't run into the end of
   * the file, the program is too large to fit in the TPA. Exit
   * with a complaint.
   */

  if( ReadStatus == 0 ) {
    bdos( 26, (unsigned long int)dmaaddress );
    return 1; 
  }

  /* We read to the end of the file before running out of memory
   * space. This means we now know how large the program is. Fill
   * in the base page and exit with success.
   */

  lpb->BasePage->Bdos = (void *)bdos;
  lpb->BasePage->TpaBase = (void *)lpb->TpaBase;
  lpb->BasePage->TpaTop = (void *)TpaTop;
  lpb->BasePage->Entry = (void *)0x200;
  lpb->BasePage->InitialStackPointer = (void *)TpaTop;
  lpb->BasePage->TextBase = (void *)0x200;
  lpb->BasePage->TextSize = ( (unsigned long int)NextSector ) - 0x200;
  lpb->BasePage->DataBase = (void *)NextSector;
  lpb->BasePage->DataSize = 0;
  lpb->BasePage->BssBase = (void *)NextSector;
  lpb->BasePage->BssSize = 0;

  bdos( 26, (unsigned long int)dmaaddress );
  return 0;
}

/***
 *
 * bios_exception
 *
 *	This routine is called when an unhandled exception is
 *	encountered. It displays an error message and restarts the
 *	system.
 *
 * revisions:
 *
 *	2005-03-05 rli: original version.
 *
 * formal parameters:
 *
 *	none.
 *
 * informal parameters:
 *
 *	The stack is assumed to have been initialized.
 *
 * return value:
 *
 *	does not return.
 *
 * side effects:
 *
 *	system restart.
 *
 ***/

bios_exception( void )
{
  bios_outzstring( "\r\nUnhandled exception. Restarting.\r\n" );
  entry();
}

/***
 *
 * initexc
 *
 *	This routine is called at system initialization time to set up
 *	the exception vectors. I'm not quite certain how this is
 *	supposed to work.
 *
 *	It looks like there's a set of user exception handlers kept
 *	in a list in the BDOS. When an exception occurs, the service
 *	routine examines that list to determine how it should be
 *	handled. A user program can set up a handler to be called
 *	when the exception occurs. The service routine examines the
 *	entry for the exception in the BDOS table. If a user exception
 *	handler is defined, it is invoked.
 *
 *	This routine is expected to do two things:
 *
 *	- Initialize (or re-initialize) the hardware exception vectors.
 *
 *	- Trim user exception vectors that lie outside the current TPA;
 *	  that is, exceptions that have not been defined by a TSR.
 *
 *	Since I'm currently ignoring exceptions this routine currently
 *	does nothing.
 *
 * revisions:
 *
 *	2005-03-05 rli: original version.
 *
 * formal parameters:
 *
 *	- parm: A pointer to the BDOS exception table.
 *
 * informal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	none.
 *
 * side effects:
 *
 *	none.
 *
 ***/

void initexc( void *parm )
{
}

/***
 *
 * load68k
 *
 *	This routine is called to load a program of a type that is not
 *	listed in load_tbl, below. What this means is that the user
 *	has entered the entire file name (with extension) as the
 *	command and the file exists. The CCP has successfully opened
 *	the file, but cannot find the appropriate loader in load_tbl.
 *	So it punts by calling this routine.
 *
 *	I'm not terribly interested in handling unknown executable
 *	formats at the moment, so we'll return an error.
 *
 * revisions:
 *
 *	2005-03-05 rli: original version.
 *
 * formal parameters:
 *
 *	none.
 *
 * informal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	- 0, indicating a general program load error.
 *
 * side effects:
 *
 *	none.
 *
 ***/

unsigned short int load68k( void )
{
  return 0;
}

/***
 *
 * bios_LoadVax
 *
 *	This routine loads and starts a program with the .VAX extension.
 *	Such programs are in the same naive format as a CP/M-80 .COM
 *	file: a memory dump of the TPA.
 *
 *	The file is loaded into the current TPA using BDOS function 59.
 *	The portion of the base page not initialized by that function
 *	is filled in, and the program is entered using BDOS function 64,
 *	which is unique to CP/M-VAX.
 *
 * revisions:
 *
 *	2005-03-05 rli: original version.
 *
 * formal parameters:
 *
 *	none.
 *
 * informal parameters:
 *
 *	- user: The user number that was in effect before the CCP
 *	  started searching for the program. The CCP will try both
 *	  user 0 and the current user. The original user number
 *	  needs to be restored before the program is entered.
 *
 *	- cmdfcb: The FCB open on the command file.
 *
 *	- tail: The command tail.
 *
 * return value:
 *
 *	- See pgmld
 *
 *	- This routine does not return if the load was successful.
 *
 * side effects:
 *
 *	With luck, the program is loaded and entered.
 *
 ***/

unsigned short int bios_LoadVax( void )
{
  extern unsigned char user;
  extern unsigned char cmdfcb[];
  extern unsigned char *tail;
  VAX_LoadParameterBlock_t lpb;
  struct tpab_s {
    unsigned short int flags;
    void *Base;
    void *Top;
  } tpab;
  unsigned short int Status;
  unsigned short int fill_fcb( unsigned short int which, 
    unsigned char *fcb );
  unsigned char *src,*dst;

  /* We need to load the program into the current TPA. This means
   * we have to ask the BDOS for the TPA, since it can be changed
   * by a program (and the changes aren't passed on to the BIOS).
   */

  tpab.flags = 0;
  bdos( 63, (unsigned long int)&tpab );
  lpb.TpaBase = tpab.Base;
  lpb.TpaTop = tpab.Top;

  /* Load the program into the current TPA.
   */

  lpb.Fcb = cmdfcb;
  Status = bdos( 59, (unsigned long int)&lpb );
  if( Status != 0 ) return Status;

  /* Copy the command tail over. The tail is ended by either a null
   * or an exclamation point.
   */

  src = tail;
  dst = (lpb.BasePage->CommandTail) + 1;
  do {
    *dst = *src;
    if( ( *src == 0 ) || ( *src == '!' ) ) break;
    dst++; src++;
  } while( 1 );
  lpb.BasePage->CommandTail[ 0 ] =
    (unsigned long int)src - (unsigned long int)tail;
  /* The program has been loaded. Now we need to fill in the remainder
   * of the base page.
   */

  fill_fcb( 1, lpb.BasePage->Fcbs[ 0 ] );
  fill_fcb( 2, lpb.BasePage->Fcbs[ 1 ] );

  /* Set the default DMA address, which traditionally points to the
   * portion of the base page containing the command tail.
   */

  bdos( 26, (unsigned long int)lpb.BasePage->CommandTail );

  /* The base page is set up. Enter the program.
   */

  Status = bdos( 64, (unsigned long int)lpb.BasePage );

  /* The BDOS does not expect this function to return if there is no
   * error.
   */

  entry();
}

/***
 *
 * load_tbl
 *
 *	This table serves two purposes:
 *
 *	- It informs the system about the recognized executable extensions
 *	- It tells the system how to load a file of each type (except
 *	  for .SUB files, which have special handling in the CCP).
 *
 *	When the CCP is searching for a command file, it tries each of
 *	the extensions listed here, in order, for both the current user
 *	and user zero. If one of the files can be opened, the handler
 *	is called to load that file (except for .SUB files, which use
 *	special handline in the CCP).
 *
 *	Although .SUB files are specially recognized by the CCP, that
 *	type needs to be listed here if the CCP is to automatically
 *	search for that extension. If the type is not listed, the
 *	escape hatch noted above for load68k can be used by explicitly
 *	specifying the .SUB type (in this case, the special handling
 *	should get invoked and load68k will not be called).
 *
 * revisions:
 *
 *	2005-03-05 rli: original version.
 *
 ***/

struct _filetyps
{
	signed char *typ;
	unsigned short int (*loader) ( void );
	signed char user_c;
	signed char user_0;
}
load_tbl[] = {
   { "VAX", bios_LoadVax, 0, 0 },
   { "\0", 0, 0, 0 }
 };


