/**************************************************************
 *
 * file: bios.c
 *
 *	This file contains the BIOS to allow execution of CP/M-VAX
 *	on the MicroVAX-2000. The assumed system configuration is:
 *
 *	- A MicroVAX-2000.
 *	- A BCC08 cable, which turns serial port #3 into a console.
 *
 *	Currently, no disk I/O is performed. Stay tuned.
 *
 *	Several things SHOULD be here, but are not. Initialization
 *	of the SCB is in boot.s, the final stage bootstrap. It should
 *	be here because the requirements of the SCB vary from model
 *	to model. As it is, a port to (say) a VAX-11/780 would require
 *	a bit of work in boot.s as well as changes here.
 *
 * revisions:
 *
 *	2005-03-07 rli: Split arch.s off from this file.
 *
 **************************************************************/

/******************
 *
 *	TYPES
 *
 ******************/

/***
 *
 * bios_dpb_t
 *
 *	This is the disk parameter block. It describes a disk to
 *	the system.
 *
 * revisions:
 *
 *	2005-03-07 rli: Added these comments.
 *
 ***/

typedef struct bios_dpb_s {
  unsigned short int spt;		/* sectors per track */
  unsigned char bsh;			/* block shift factor */
  unsigned char blm;			/* block mask */
  unsigned char exm;			/* extent mask */
  unsigned char reserved1;
  unsigned short int dsm;		/* number of blocks in disk */
  unsigned short int drm;		/* number of directory entries */
  unsigned short int reserved2;		/* initial allocation vector in -80 */
  unsigned short int cks;		/* size of checksum vector */
  unsigned short int off;		/* reserved tracks */
} bios_dpb_t;

/***
 *
 * bios_dph_t
 *
 *	This is the disk parameter header. It contains pointers to the
 *	various data structures needed to run a disk.
 *
 * revisions:
 *
 *	2005-03-07 rli: original version.
 *
 ***/

typedef struct bios_dph_s {
  unsigned short *xlt;			/* sector translation table address */
  unsigned short int scratch[ 3 ];	/* scratch words used by BIOS */
  unsigned char *dirbuf;		/* scratch sector for directory use */
  bios_dpb_t *dpb;			/* address of disk parameter block */
  unsigned char *csv;			/* checksum vector */
  unsigned char *alv;			/* allocation vector */
} bios_dph_t;

/***
 *
 * bios_mrt_t
 *
 *	This is the memory region table. It describes the RAM to the
 *	operating system.
 *
 *	We're reserving a page below the start of the operating system
 *	to hold the initial stack.
 *
 * revisions:
 *
 *	2005-03-07 rli: original version.
 *
 ***/

typedef struct bios_mrt_s {
  unsigned short int count;		/* number of regions in table */
  void *base;				/* base address of first region */
  unsigned long int length;		/* length of first region */
} bios_mrt_t;

/*******************
 *
 *	GLOBALS
 *
 *******************/

/***
 *
 * bios_8inch_dpb
 *
 *	This disk parameter block describes a standard 8" SSSD floppy
 *	diskette. We're currently using this format for a RAM disk.
 *
 * revisions:
 *
 *	2005-03-07 rli: Added these comments.
 *
 ***/

bios_dpb_t bios_8inch_dpb = {
   26,  /* 128-byte sectors per track */
    3,  /* block shift */
    7,  /* block mask */
    0,  /* extent mask */
    0,  /* reserved */
  242,  /* allocation blocks per disk - 1 */
   63,  /* number of directory entries - 1 */
    0,  /* reserved; initial allocation vector in CP/M-80 */
   16,  /* size of check vector */
    2   /* track offset */
};

/***
 *
 * bios_dirbuf
 *
 *	This is a scratch buffer used to hold sectors while the BDOS
 *	performs directory manipulation. It is shared between all
 *	of the disks.
 *
 * revisions:
 *
 *	2005-03-07 rli: Added these comments.
 *
 ***/
 
unsigned char bios_dirbuf[ 128 ];

/***
 *
 * bios_drivea_csv
 *
 *	The system uses the check vector to determine whether or not
 *	a diskette has been changed. The vector contains one byte
 *	for each directory entry. The operating system forms a checksum
 *	across each directory entry when the disk is logged in to allow
 *	a diskette change to be detected.
 *
 *	Each drive requires a separate check vector. This is the vector
 *	for drive A:, the 8" SSSD RAM disk.
 *
 * revisions:
 *
 *	2005-03-07 rli: Added these comments.
 *
 ***/

unsigned char bios_drivea_csv[ 64 ];

/***
 *
 * bios_drivea_alv
 *
 *	This is the allocation vector for drive A:, the 8" SSSD RAM
 *	disk. The operating system uses this vector to locate free
 *	blocks when it needs to allocate a block to a file. The vector
 *	contains one bit for each block on the disk.
 *
 *	Each drive requires a separate allocation vector.
 *
 * revisions:
 *
 *	2005-03-07 rli: Added these comments.
 *
 ***/

unsigned char bios_drivea_alv[ ( 242 / 8 ) + 1 ];

/***
 *
 * bios_8inch_xlt
 *
 *	This is a sector translation table for the standard 8" SSSD
 *	format. Although we're using a RAM disk, we use the 8" SSSD
 *	format to allow us to load actual disk images, at least until
 *	we're running on an actual disk. Consequently, we need to
 *	use the standard sector skew.
 *
 *	All drives of a given type may share a single translation table.
 *
 * revisions:
 *
 *	2003-03-07 rli: Added these comments.
 *
 ***/

unsigned short int bios_8inch_xlt[ 26 ] = {
   0,  6, 12, 18, 24,  4, 10, 16,
  22,  2,  8, 14, 20,  1,  7, 13,
  19, 25,  5, 11, 17, 23,  3,  9,
  15, 21
};

/***
 *
 * bios_8inch_dph
 *
 *	This is the disk parameter header for drive A:, the 8" SSSD RAM
 *	disk. It contains pointers to the other data structures needed
 *	to run a drive; when the BDOS selects a drive, it receives a
 *	pointer to this structure, which it uses to locate the others.
 *
 *	Each drive needs its own disk parameter header.
 *
 * revisions:
 *
 *	2005-03-07 rli: Added these comments.
 *
 ***/

bios_dph_t bios_8inch_dph = {
  bios_8inch_xlt,	/* sector translation table */
  0, 0, 0,		/* scratch words */
  bios_dirbuf,		/* directory buffer */
  &bios_8inch_dpb,	/* disk parameter block */
  bios_drivea_csv,	/* checksum vector */
  bios_drivea_alv	/* allocation vector */
};

/***
 *
 * bios_currenttrack
 *
 *	This variable holds the current track number; in other words,
 *	the last track number that was set by the BDOS using the BIOS SET
 *	TRACK function.
 *
 * revisions:
 *
 *	2005-03-07 rli: Added these comments.
 *
 ***/

unsigned int bios_currenttrack;

/***
 *
 * bios_currentdrive
 *
 *	This variable holds the current drive number; in other words,
 *	the number of the most recently successfully selected disk
 *	drive.
 *
 * revisions:
 *
 *	2005-03-07 rli: Added these comments.
 *
 ***/

unsigned int bios_currentdrive;

/***
 *
 * bios_currentsector
 *
 *	This variable holds the currently selected sector number; that
 *	is, the last sector number handed to the BIOS SET SECTOR
 *	function.
 *
 * revisions:
 *
 *	2005-03-07 rli: added these comments.
 *
 ***/

unsigned int bios_currentsector;

/***
 *
 * bios_currentdmaaddress
 *
 *	This variable holds the current DMA address; that is, the
 *	last DMA address handed to the BIOS SET DMA ADDRESS function.
 *
 * revisions:
 *
 *	2005-03-07 rli: added these comments.
 *
 ***/

unsigned char *bios_currentdmaaddress;

/***
 *
 * bios_mrt
 *
 *	The memory region table describes the TPA to the system. We're
 *	going to allocate memory from zero to the bottom of the system
 *	image to the TPA, less a page we're reserving right below
 *	the system for stack space. The system image (declared in 
 *	boot.s) begins at start.
 *
 * revisions:
 *
 *	2005-03-07 rli: Added these comments.
 *
 ***/

extern void start();
bios_mrt_t bios_mrt = {
  1,	/* one region in the table */
  0,	/* start address */ 
  ( (unsigned long int)start ) - 512 /* size */
};

/***
 *
 * bios_iobyte
 *
 *	This variable contains the I/O byte, which is not currently
 *	implemented. Sure, we'll store it here and we'll read it back,
 *	but we aint using it to select any ports.
 *
 * revisions
 *
 *	2005-03-07 rli: added these comments.
 *
 ***/

unsigned short int bios_iobyte;

/***
 *
 * FUNCTION 0: INITIALIZATION
 *
 *	This routine is entered on cold boot and must initialize the
 *	BIOS.
 *
 *	At the moment, all our initialization is done elsewhere, so
 *	there's nothing to be done here.
 *
 * revisions:
 *
 *	2005-03-07 rli: Updated tese comments.
 *
 * formal parameters:
 *
 *	none.
 *
 * informal parameters:
 *
 *	none
 *
 * return value:
 *
 *	The default drive number and user number:
 *
 *
 *	 1098765432109876 54321098 76543210
 *	+----------------+--------+--------+
 *      |000000000000000 |        |        |
 *	+----------------+--------+--------+
 *                            ^       ^
 *	                      |       |
 *	                      |       +-- default drive number
 *	                      +-- initial user number
 *
 * side effects:
 *
 *	none.
 *
 ***/

unsigned long int bios_boot( void )
{
  return 0;
}

/***
 *
 * FUNCTION 1: WARM BOOT
 *
 *	This function is called whenever a program terminates. Some
 *	reinitialization of the hardware or software might occur.
 *
 *	Since we might be called from a user program, we don't know
 *	where the stack is. So re-initialize everything and start
 *	over. At some point, this will become inappropriate; someone
 *	will want an autostart command line executed on cold boot
 *	but not warm boot or whatnot.
 *
 * revisions:
 *
 *	2005-03-07 rli: rework to re-initialize the stack pointer.
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
 *	doesn't return.
 *
 * side effects:
 *
 *	The system is restarted.
 *
 ***/

void bios_wboot( void )
{
  extern void entry( void );
  entry();
}

/***
 *
 * FUNCTION 2: CONSOLE STATUS
 *
 *	This function returns the status of the currently assigned
 *	console device. It returns 0x00FF when a character is ready
 *	to be read or 0x0000 when no console characters are ready.
 *
 *	Currently, we support only line number 3 of the DZ; we don't
 *	use the I/O byte.
 *
 * revisions:
 *
 *	2005-03-07 rli: Added these comments.
 *
 * formal parameters:
 *
 *	none.
 *
 * informal parameters:
 *
 *	A MicroVAX 2000 DZ at address 200A.0000.
 *
 * return value:
 *
 *	- 0x0000: no characters are ready from the console.
 *
 *	- 0x00ff: A character may be read from the console.
 *
 * side effects:
 *
 *	none.
 *
 ***/

unsigned short int bios_const( void )
{

  /* We've disabled lines we're not interested in, so anything that
   * comes in should be from the console.
   */

  if( ( *(unsigned short int *)0x200A0000 ) & 0x80 ) 
    return 0xff;
  return 0;
}

/***
 *
 * FUNCTION 3: READ CONSOLE CHARACTER
 *
 *	This function reads and returns the next console character.
 *	If no console character is ready, it waits until a character
 *	is typed before returning.
 *
 *	Currently, we use only line 3 of the DZ; we ignore the I/O byte.
 *
 * revisions:
 *
 *	2005-03-07 rli: Added these comments.
 *
 * formal parameters:
 *
 *	none.
 *
 * informal parameters:
 *
 *	A MicroVAX 2000 DZ at address 200A.0000.
 *
 * return value:
 *
 *	- character read from the console.
 *
 * side effects:
 *
 *	none.
 *
 ***/

unsigned char bios_conin( void )
{
  while( !bios_const() );
  return ( *(unsigned short int *)0x200A0004 ) & 0x7f;
}

/***
 *
 * FUNCTION 4: WRITE CONSOLE CHARACTER
 *
 *	This function sends a character to the console output device.
 *
 *	Currently, we support only line 3 of the DZ; we're ignoring the
 *	I/O byte.
 *
 * revisions:
 *
 *	2005-03-07 rli: added these comments.
 *
 * formal parameters:
 *
 *	- victim: the character to be displayed.
 *
 * informal parameters:
 *
 *	A MicroVAX 2000 DZ at address 200A.0000.
 *
 * return value:
 *
 *	none.
 *
 * side effects:
 *
 *	Transmission is disabled for all lines except line 3.
 *
 ***/

void bios_conout( unsigned char victim )
{

  /* Ensure transmission is enabled for line 3 and only line 3. That
   * way, when the DZ is ready to send, it's ready to send to the
   * console.
   */

  *(unsigned short int *)0x200A0008 = 8;

  /* Wait for the line to be ready to print.
   */

  while( !( ( *(unsigned short int *)0x200A0000 ) & 0x8000 ) ) ;

  /* Send the character.
   */

  *(unsigned short int *)0x200A000C = victim;
}

/***
 *
 * FUNCTION 5: LIST CHARACTER OUTPUT
 *
 *	This character sends an ASCII character to the currently
 *	designated listing device. If your list device requires some
 *	communication protocol, it must be handled here.
 *
 * formal parameters:
 *
 *	- victim: the character to be sent to the printer.
 *
 * return value:
 *
 *	none.
 *
 ***/

void bios_list( unsigned char victim )
{
}

/***
 *
 * FUNCTION 6: AUXILIARY OUTPUT
 *
 *	This function sends an ASCII character to the currently assigned
 *	auxiliary output device.
 *
 * formal parameters:
 *
 *	- victim: the character to be sent to the punch.
 *
 * return value:
 *
 *	none.
 *
 ***/

void bios_punch( unsigned char victim )
{
}

/***
 *
 * FUNCTION 7: AUXILIARY INPUT
 *
 *	This function reads the next character from the currently
 *	assigned auxiliary input device. It reports an end-of-file
 *	condition by returning an ASCII CTRL-Z (0x1a).
 *
 * formal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	- The character read from the reader.
 *
 ***/

unsigned char bios_reader( void )
{
  return 0x1a;
}

/***
 *
 * FUNCTION 8: HOME
 *
 *	This function returns the disk head of the currently selected
 *	disk to the track 00 position. If your controller does not have
 *	a special feature for finding track 00, you can translate teh
 *	call to a SETTRK function with a parameter of 0.
 *
 * formal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	none.
 *
 ***/

void bios_home( void )
{
  bios_currenttrack = 0;
}

/***
 *
 * FUNCTION 9: SELECT DISK DRIVE
 *
 *	This function selects the specified disk drive for further
 *	operations. The parameter contains 0 for drive A, 1 for drive B,
 *	up to 15 for drive P.
 *
 *	On each disk select, this function returns the address of the
 *	selected drive's Disk Parmaeter Header.
 *
 *	If there is an attempt to select a nonexistent drive, this
 *	functions 0 as an error indicator. Although the function must
 *	return the header address on each call, it may be advisable to
 *	postpone the actual physical disk select operation until an I/O
 *	function (seek, read, or write) is performed. Disk select
 *	operations can occur withou; a subsequent disk operation. Thsu,
 *	doing a physical selecte ach time this funciton is called may be
 *	wasteful of time.
 *
 *	On entry to the Select Disk Drive function, if the least
 *	significant bit in the second parameter is zero, the disk is not
 *	currently logged in. If the disk drive is capable of handling
 *	varying media (such as single and double-sided disks, single- and
 *	double-density, and so on), the BIOS should check the type of
 *	media currnetly installed and set up the Disk Parameter Block
 *	accordingly at this time.
 *
 * formal parameters:
 *
 *	- drive: the number of the drive to be selected.
 *
 *	- logged: indicates whether the drive is currently logged in.
 *
 * return value:
 *
 *	- 0: The drive number is invalid.
 *
 *	- else: A pointer to the DPH for the drive.
 *
 ***/

bios_dph_t *bios_seldsk( unsigned char drive, unsigned char logged )
{
  switch( drive ) {
    case 0:
      bios_currentdrive = drive;
      return &bios_8inch_dph; 
    default:
      return 0;
  }
}

/***
 *
 * FUNCTION 10: SET TRACK NUMBER
 *
 *	This function specifies the disk track number for use in
 *	subsequent disk accesses. The track number remains valid until
 *	either another Function 10 or a Function 8 (Home) is performed.
 *
 *	You can choose to physically seek to the selected track at this
 *	time, or delay the physical seek until the next read or write
 *	actually occurs.
 *
 *	The track number can range from 0 to the maximum track number
 *	supported by the physical drive. However, the maximum track
 *	number is limited to 65535 by the fact that it is being passed
 *	as a 16-bit quantity. Standard floppy disks have tracks number
 *	from 0 to 76.
 *
 * formal parameters:
 *
 *	- track: The track number.
 *
 * return value:
 *
 *	none.
 *
 ***/

void bios_settrk( unsigned short int track )
{
  bios_currenttrack = track;
}

/***
 *
 * FUNCTION 11: SET SECTOR NUMBER
 *
 *	This function specifies the sector number for subsequent disk
 *	accesses. This number remains in effect until another function
 *	11 is performed.
 *
 *	Th function select actual (unskewed) sector numbers. If skewing
 *	is appropriate, it will have previously been done by a call to
 *	Function 16. You can send this information to the controller at
 *	this point or delay sector selection until  a read or write
 *	operation occurs.
 *
 * formal parmaeters:
 *
 *	- sector: the sector to be used for subsequent I/O.
 *
 * return value:
 *
 *	none.
 *
 ***/

void bios_setsec( unsigned short int sector )
{
  bios_currentsector = sector;
}

/***
 *
 * FUNCTION 12: SET DMA ADDRESS
 *
 *	This function specifies teh DMA address for subsequent read or
 *	write operations. Note that the controller need not actually
 *	support DMA (direct memory access). The BIOS will use the
 *	128-byte area starting at the selected DMA address for the
 *	memory buffer during the following read or write operations.
 *	This function can be called with either an even or an odd
 *	address for a DMA buffer.
 *
 * formal parameters:
 *
 *	- dmaaddress: pointer to the sector buffer.
 *
 * return value:
 *
 *	none.
 *
 ***/

void bios_setdma( void *dmaaddress )
{
  bios_currentdmaaddress = dmaaddress;
}

/***	bios_diskimage
 *
 *	This array contains the disk image.
 *
 ***/

extern unsigned char bios_diskimage[];

/***
 *
 * FUNCTION 13: READ SECTOR
 *
 *	After the drive has been selected, the track has been set, the
 *	sector has been set, and teh DMA address has been specified, the
 *	read function uses these parameters to read one sector and
 *	returns the error code.
 *
 *	Currently, CP/M-68K responds only to a zero or nonzero return
 *	code value. Thus, if the return value is zero, CP/M-68K assumes
 *	that the disk operation completed properly. If an error occurs,
 *	however, the BIOS should attempt at least ten retries to see if
 *	the error is recoverable.
 *
 * formal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	- 0: success.
 *
 *	- 1: failure.
 *
 ***/

unsigned short int bios_read( void )
{
  int i;
  int start;

  start = (( bios_currenttrack * 26 ) + bios_currentsector ) * 128; 
  for( i = 0; i < 128; i++ ) 
    bios_currentdmaaddress[ i ] = bios_diskimage[ start + i ];

  return 0;
}

/***
 *
 * FUNCTION 14: WRITE SECTOR
 *
 *	This function is used to write 128 bytes of data from the
 *	currently selected DMA buffer to the currently selected sector,
 *	track, and disk. The parameter indicates whether the write is an
 *	ordinary write operation or whether there are special
 *	considerations.
 *
 *	If the parameter is 0, this is an ordinary write operation. If
 *	it is 1, this is a write to a directory sector, and the write
 *	should be physically completed immediately. If the parmaters is
 *	2, this is a write to teh first sector of a newly allocated
 *	block of the disk.
 *
 * formal parameters:
 *
 *	- typecode: the type of the write.
 *
 * return value:
 *
 *	- 0: success.
 *
 *	- 1: error.
 *
 ***/

unsigned short int bios_write( unsigned short int typecode )
{
  int i;
  int start;

  start = (( bios_currenttrack * 26 ) + bios_currentsector ) * 128; 
  for( i = 0; i < 128; i++ ) 
    bios_diskimage[ start + i ] = bios_currentdmaaddress[ i ];

  return 0;
}

/***
 *
 * FUNCTION 15: RETURN LIST STATUS
 *
 *	This function returns the status of the list device, either 0
 *	when the list device is not ready to accept a character or 0xff
 *	when a character can be sent to the list device.
 *
 * formal parameters:
 *
 *	none
 *
 * return value:
 *
 *	- 0: printer is not ready to accept a character.
 *
 *	- 0xff: printer is ready to accept a character.
 *
 ***/

unsigned short int bios_listst( void )
{
  return 0xff;
}

/***
 *
 * FUNCTION 16: SECTOR TRANSLATE
 *
 *	This function performs logical-to-physical sector translation.
 *	The Sector Translate function receives a logical sector number.
 *	The logical sector number can range from 0 to the number of
 *	sectors per track - 1. Sector Translate also receives the
 *	address of teh translate table. The logical sector number is
 *	used as an index into the translate table. The resulting
 *	physical sector number is returned.
 *
 *	If the pointer to the translate table is null, implying that
 *	there is no translate table, the original sector number is
 *	returned. Note that other algorithms are possible; in
 *	particular, it is common to increment the logical sector number
 *	in order to convert the logical range of 0 to n-1 into the
 *	physical range of 1 to n. Sector Translate is always called by
 *	the BDOS, whether the translate table address in the Disk
 *	Parameter Header is zero or nonzero.
 *
 * formal parameters:
 *
 *	- sector: logical sector number.
 *
 *	- table: pointer to sector number translation table.
 *
 * return value:
 *
 *	physical sector number.
 *
 ***/

unsigned short int bios_sectran( unsigned short int sector, 
  unsigned short int *table )
{
  if( table == 0 ) return sector;
  return table[ sector ];
}

/***
 *
 * FUNCTION 17: There is NO function 17
 *
 ***/

/***
 *
 * FUNCTION 18: GET ADDRESS OF MEMORY REGION TABLE
 *
 *	This function returns the address of the Memory REgion Table
 *	(MRT). For compatibility with other CP/M system, CP/M-68K
 *	maintains a Memory Region Table. However, it contains only one
 *	region, the Transient Program Area (TPA). 
 *
 * formal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	A pointer to the memory region table.
 *
 ***/

bios_mrt_t *bios_getmrt( void )
{
  return &bios_mrt;
}

/***
 *
 * FUNCTION 19: GET I/O BYTE
 *
 *	This function returns the currnet value of the logical to
 *	physical input/output device byte (I/O byte). This 8-bit value
 *	associates physical devices with CP/M-68k's four logical
 *	devices.
 *
 *	NOTE:	Even though this is a byte value, we are using
 *		word references. The upper byte should be zero.
 *
 * formal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	a copy of the I/O byte.
 *
 ***/

unsigned short int bios_getiobyte( void )
{
  return bios_iobyte;
}

/***
 *
 * FUNCTION 20: SET I/O BYTE
 *
 *	This function sets the I/O byte to the specified value.
 *
 *	NOTE:	Even though this is a byte value, we are using word
 *		references. The upper byte should be zero.
 *
 * formal parameters:
 *
 *	- iobyte: The value to be stored in the I/O byte.
 *
 * return value:
 *
 *	none.
 *
 ***/

void bios_setiobyte( unsigned short int iobyte )
{
  bios_iobyte = iobyte;
}

/***
 *
 * FUNCTION 21: FLUSH BUFFERS
 *
 *	This function forces the contents of any disk buffers that have
 *	been modified to be written. That is, after this funciton has
 *	been performed, all disk writes have been physically completed.
 *	After the buffers are written, this function returns a zero.
 *	However, if the buffers cannot be written or an error occurs,
 *	the function returns 0xffff.
 *
 * formal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	- 0: success.
 *
 *	- 0xffff: failure.
 *
 ***/

unsigned short int bios_flush( void )
{
  return 0;
}

/***
 *
 * FUNCTION 22: SET EXCEPTION HANDLE ADDRESS
 *
 *	This function sets the specified exception vector such that it
 *	invokes the specified handler. The previous vector value is
 *	returned. Unlike the BDOS Set Exception Vector Function, this
 *	BIOS function sets any exception vector.
 *
 * formal parameters:
 *
 *	- vector: The number of the vector to be set.
 *
 *	- handler: A pointer to the routine that is to be invoked.
 *
 ***/

void *bios_setexc( unsigned short int vector, void *handler )
{
  return 0;
}
