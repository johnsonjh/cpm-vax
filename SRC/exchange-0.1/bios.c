/* Since this version of the BIOS interfaces with the host OS, we need
 * host includes.
 */

#include <stdio.h>
#include <stdlib.h>
#include <termios.h>


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

typedef struct bios_dph_s {
  unsigned short *xlt;			/* sector translation table address */
  unsigned short int scratch[ 3 ];	/* scratch words used by BIOS */
  unsigned char *dirbuf;		/* scratch sector for directory use */
  bios_dpb_t *dpb;			/* address of disk parameter block */
  unsigned char *csv;			/* checksum vector */
  unsigned char *alv;			/* allocation vector */
} bios_dph_t;

typedef struct bios_mrt_s {
  unsigned short int count;		/* number of regions in table */
  void *base;				/* base address of first region */
  unsigned long int length;		/* length of first region */
} bios_mrt_t;

unsigned short int bios_flush( void );


/***
 *
 * P112 3.5" diskette
 *
 *      dw      72              ;CP/M "sectors"/track
 *      db      4               ;Block shift
 *      db      15              ;Block mask
 *      db      0               ;Extent mask
 *      dw      715-1           ;Max. allocn. block no.
 *      dw      128-1           ;No. of directory entries -1
 *      db      11000000B       ;Bit-map for allocn. blocks
 *      db      00000000B       ;  used for directory
 *      dw      32              ;No. of bytes in dir. check buffer
 *      dw      1               ;No. of tracks before directory
 *
 ***/

bios_dpb_t bios_p112_dpb = {
  72,  /* 128-byte sectors per track */
   4,  /* block shift factor */
  15,  /* block mask */
   0,  /* extent mask */
   0,  /* reserved */
 714,  /* 2048-byte blcoks per disk - 1 */
 127,  /* number of directory entries - 1 */
   0,  /* reserved */
  32,  /* size of check vector */
   1   /* reserved tracks */
};

/***
 *
 * 8" SSSD standard diskette
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


 
unsigned char bios_dirbuf[ 128 ];

/***
 *
 * Check vector
 *
 *	The check vector contains one byte for each directory entry.
 *
 *	The directory may occupy up to 16 allocation blocks (at least on
 *	CP/M-80; I don't know what the limit is on CP/M-68K), because
 *	directory size is constrained by the fact that there are only
 *	16 bits available for the initial allocation vector. The
 *	largest allocation block is 16K. Since each sector contains
 *	four directory entries and there are 8 sectors in a K, a
 *	16K allocation block holds 16*8*4 = 512 directory directory
 *	entries; each directory block may therefore require up to
 *	512 bytes of check vector space. Since there can be up to 
 *	16 directory blocks, the wort case check vector occupies
 *	8K bytes.
 *
 *	NOTE:	This is assuming that CP/M-68K is, like CP/M-80,
 *		limited to reserving up to 16 allocation blocks for
 *		the directory!
 *
 ***/

unsigned char bios_drivea_csv[ 8192 ];

/***
 *
 * Allocation vector
 *
 *	Each allocation block requires one bit in the allocation vector.
 *	Since there are at most 64K allocation blocks, the allocation
 *	vector may occupy up to 8K bytes.
 *
 ***/

unsigned char bios_drivea_alv[ 8192 ];

/***
 *
 * P112 3.5" DPH
 *
 ***/

bios_dph_t bios_p112_dph = {
    0,  		/* sector translation table */
    0, 0, 0, 		/* scratch words */
    bios_dirbuf,	/* Directory buffer */
    &bios_p112_dpb,	/* disk parameter block */
    bios_drivea_csv,    /* checksum vector */
    bios_drivea_alv     /* allocation vector */
}; 

/***
 *
 * 8" sector translation vector
 *
 *	A standard 8" disk has 6:1 interleave with sector numbers
 *	starting at one. We want to keep the interleave, but we want
 *	sector numbers to start at zero; therefore, this is not quite
 *	the standard sector translation table.
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
 * 8" SSSD DPH
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

unsigned int bios_currenttrack;
unsigned int bios_currentdrive;
unsigned int bios_currentsector;
unsigned char *bios_currentdmaaddress;
bios_dph_t *bios_current_dph;

FILE *bios_diskimage;
FILE *bios_printer = 0;

/***
 *
 *	This is a pointer to the dph that will be returned when drive A
 *	is selected. Different disk formats may be supported by
 *	arranging for this to be properly initialized when the program
 *	starts.
 *
 *	By default, we use the standard 8" SSSD format.
 *
 ***/

bios_dph_t *bios_valid_dph = &bios_8inch_dph;

/***
 *
 * TPA
 *
 *	We need some sort of TPA to keep CP/M happy, even though we
 *	cannot (currently) load executables. Since I don't know what the
 *	minimum I can get away with, I'm just tossing out 64K.
 *
 ***/

unsigned char bios_tpa[ 65536 ];

/***
 *
 * MRT
 *
 *	The memory region table describes the TPA to the system.
 *
 ***/

bios_mrt_t bios_mrt = {
  1,
  bios_tpa,
  sizeof( bios_tpa )
};

unsigned short int bios_iobyte;

/***
 *
 * FUNCTION 0: INITIALIZATION
 *
 *	This routine is entered on cold boot and must initialize the
 *	BIOS.
 *
 * formal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	 1098765432109876 54321098 76543210
 *	+----------------+--------+--------+
 *      |                |        |        |
 *	+----------------+--------+--------+
 *               ^            ^       ^
 *	         |            |       |
 *	         |            |       +-- default drive number
 *	         |            +-- initial user number
 *               +-- MBZ
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
 *	When this function completes, it jumps directly to the entry
 *	point of the CCP, named _ccp. Node that _ccp must be declared
 *	as a global.
 *
 * formal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	doesn't return.
 *
 ***/

void bios_wboot( void )
{
  extern cpm_ccp( void );
  if( bios_printer != NULL ) fclose( bios_printer );
  cpm_ccp();
}

/***
 *
 * FUNCTION 2: CONSOLE STATUS
 *
 *	This functio nreturns the status of the currently assigned
 *	console device. It returns 0x00FF when a character is ready
 *	to be read or 0x0000 when no console characters are ready.
 *
 * formal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	- 0x0000: no characters are ready from the console.
 *
 *	- 0x00ff: A character may be read from the console.
 *
 ***/

unsigned short int bios_const( void )
{
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
 * formal parameters:
 *
 *	none.
 *
 * return value:
 *
 *	- character read from the console.
 *
 ***/

unsigned char bios_conin( void )
{
  return getchar();
}

/***
 *
 * FUNCTION 4: WRITE CONSOLE CHARACTER
 *
 *	This function sends a character to the console output device.
 *	The character is in ASCII You might want to include a delay or
 *	filler characters for a line-feed or carriage return, if your
 *	console device requires some time interval at the end of the
 *	line (such as a TI Silent 700 Terminal). You can also filter out
 *	control characters which have undesirable effects on the console
 *	device.
 *
 * formal parameters:
 *
 *	- victim: the character to be displayed.
 *
 * return value:
 *
 *	none.
 *
 ***/

void bios_conout( unsigned char victim )
{
  putchar( victim );
  fflush( stdout );
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
  if( bios_printer == NULL ) {
    bios_printer = fopen( "printer.txt", "w" );
    if( bios_printer == NULL ) {
      perror( "printer.txt" );
      exit( EXIT_FAILURE );
    }
  }
  fputc( victim, bios_printer );
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
      bios_current_dph = bios_valid_dph; break;
    default:
      bios_current_dph = 0; break;
  }
  return bios_current_dph;
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
  unsigned int offset;
  int temp;
  offset = ( bios_currenttrack * bios_current_dph->dpb->spt ) + 
    bios_currentsector;
  if( fseek( bios_diskimage, offset*128, SEEK_SET ) != 0 ) {
    perror( "seek" );
    return 1;
  }
  temp = fread( bios_currentdmaaddress, 1, 128, bios_diskimage ) ;
  if( temp != 128 ) {
    fprintf( stderr, "track: %d ", bios_currenttrack );
    fprintf( stderr, "sector: %d ", bios_currentsector );
    fprintf( stderr, "pos: %d ", offset*128 );
    fprintf( stderr, "read: %d ", temp );
    perror( "short read" );
    return 1;
  }
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
  unsigned int offset;
  offset = ( bios_currenttrack * bios_current_dph->dpb->spt ) + 
    bios_currentsector;
  if( fseek( bios_diskimage, offset*128, SEEK_SET ) != 0 ) {
    perror( "seek" );
    return 1;
  }
  if( fwrite( bios_currentdmaaddress, 1, 128, bios_diskimage ) != 128 ) {
    perror( "short write" );
    return 1;
  }
  if( typecode == 1 ) bios_flush();
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
  if( bios_current_dph->xlt == 0 ) return sector;
  return bios_current_dph->xlt[ sector ];
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
  fflush( bios_diskimage );
  return 0;
}

/***
 *
 * FUNCTION 22: SET EXCEPTION HANDLE ADDRESS
 *
 *	This function sets the specified exception vector such that it
 *	invokes the specified handler. The previous vector value is
 *	retruned. Unlike the BDOS Set Exception Vector Function, this
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

unsigned short int cpm_udiv( 
  signed long int dividend, 
  unsigned short int divisor,
  unsigned short int *remainder )
{
  *remainder = dividend % divisor;
  return dividend / divisor;
}

unsigned short int cpm_swap( unsigned short int victim )
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

unsigned char bios1( int function )
{
  return 0;
}

void bios2( int function, unsigned short int parm )
{
}

void bios3( int function, void *parm )
{
}

void *bios4( int function, unsigned short int parm1, unsigned short int parm2 )
{
  return (void *)0;
}

unsigned short int bios5( int function, unsigned short int parm1,
  unsigned short int parm2 )
{
  return 0;
}

signed char *bios6( int function )
{
  return (void *)0;
}

unsigned char cpm_submit = 0;
unsigned char cpm_morecmds = 0;
unsigned char cpm_autost = 0;
signed char cpm_usercmd[ 130 ];

unsigned short int cpm_bdos( unsigned short int func, 
  unsigned long int parm )
{
  return cpm__bdos( func, (unsigned short int)parm, (void *)parm );
}

unsigned short int cpm_pgmld( void *infop, void *dmaaddress )
{
  return 0;
}

unsigned char *cpm_traphndl( void )
{
  return 0;
}

void cpm_initexc( void *parm )
{
}

struct _filetyps
{
	signed char *typ;
	unsigned short int (*loader) ();
	signed char user_c;
	signed char user_0;
}
cpm_load_tbl[] = { "\0", 0, 0, 0 };

unsigned short int cpm_load68k( void )
{
  return 0;
}

struct termios bios_original_t;

void bios_exit( void )
{
  fclose( bios_diskimage );
  if( tcsetattr( fileno( stdin ), TCSANOW, &bios_original_t ) != 0 ) {
    perror( "tcsetattr" );
    exit( EXIT_FAILURE );
  }
  exit( EXIT_SUCCESS );
}

typedef struct bios_fcb_s {
  unsigned char drive;
  unsigned char name[ 8 ];
  unsigned char ext[ 3 ];
  unsigned char reserved[ 20 ];
  unsigned char cr;
} bios_fcb_t;

int bios_fcb2path( bios_fcb_t *fcb, char *path )
{
  int i;
  int pathindex = 0;

  for( i = 0; i < 8; i++ ) {
    if( ( fcb->name[ i ] & 0xff ) < '!' ) break;
    path[ pathindex++ ] = fcb->name[ i ];
  }
  path[ pathindex++ ] = '.';
  for( i = 0; i < 3; i++ ) {
    if( ( fcb->ext[ i ] & 0xff ) < '!' ) break;
    path[ pathindex++ ] = fcb->ext[ i ];
  }
  if( pathindex == 1 ) return 0;
  path[ pathindex ] = 0;
  return 1;
}

void bios_import( void )
{
  extern unsigned char cpm_dma[];
  extern unsigned short int cpm_fill_fcb(
    unsigned short int which_parm,
    bios_fcb_t *fcb );
  int i;
  bios_fcb_t fcb;
  int ambiguous;
  char path[ 13 ];
  FILE *exportfile;
  char buffer[ 128 ];

  ambiguous = cpm_fill_fcb( 1, &fcb );
  if( ambiguous ) {
    printf( "Wildcards not supported." );
    return;
  }

  if( !bios_fcb2path( &fcb, path ) ) {
    printf( "Cannot import ." );
    return;
  }

  cpm_bdos( 19, (unsigned long int)&fcb );

  if( cpm_bdos( 22, (unsigned long int)&fcb ) > 3 ) {
    printf( "Cannot make " );
    for( i = 0; i < 8; i++ )
      printf( "%c", fcb.name[ i ] & 0x7f );
    printf( "." );
    for( i = 0; i < 3; i++ )
      printf( "%c", fcb.ext[ i ] & 0x7f );
    return;
  }

  exportfile = fopen( path, "rb" );
  if( exportfile == NULL ) {
    perror( path );
    return;
  }

  printf( "import %s", path );
  printf( " -> " );
  for( i = 0; i < 8; i++ )
    printf( "%c", fcb.name[ i ] & 0x7f );
  printf( "." );
  for( i = 0; i < 3; i++ )
    printf( "%c", fcb.ext[ i ] & 0x7f );

  cpm_bdos( 26, (unsigned long int)&buffer );

  while( 1 ) {
    memset( buffer, 0x1a, 128 );
    if( fread( buffer, 1, 128, exportfile ) == 0 ) {
      break;
    }
    i = cpm_bdos( 21, (unsigned long int)&fcb );
    if( i == 1 ) {
      printf( " Directory full." );
      break;
    }
    if( i == 2 ) {
      printf( " Disk full." );
      break;
    }
    if( i != 0 ) {
      printf( " Unknown error %d", i );
      break;
    }
  }

  cpm_bdos( 16, (unsigned long int)&fcb );
  fclose( exportfile );

}

void bios_export( void )
{
  extern unsigned char cpm_dma[];
  extern unsigned short int cpm_fill_fcb(
    unsigned short int which_parm,
    bios_fcb_t *fcb );
  int i;
  bios_fcb_t fcb;
  int ambiguous;
  char path[ 13 ];
  FILE *exportfile;
  char buffer[ 128 ];

  ambiguous = cpm_fill_fcb( 1, &fcb );
  if( ambiguous ) {
    printf( "Wildcards not supported." );
    return;
  }

  if( !bios_fcb2path( &fcb, path ) ) {
    printf( "Cannot export ." );
    return;
  }

  if( cpm_bdos( 15, (unsigned long int)&fcb ) > 3 ) {
    printf( "No file." );
    return;
  }

  exportfile = fopen( path, "wb" );
  if( exportfile == NULL ) {
    perror( path );
    return;
  }

  printf( "export " );
  for( i = 0; i < 8; i++ )
    printf( "%c", fcb.name[ i ] & 0x7f );
  printf( "." );
  for( i = 0; i < 3; i++ )
    printf( "%c", fcb.ext[ i ] & 0x7f );
  printf( " -> " );
  printf( "%s", path );

  cpm_bdos( 26, (unsigned long int)&buffer );

  while( 1 ) {
    if( cpm_bdos( 20, (unsigned long int)&fcb ) != 0 ) {
      break;
    }
    if( fwrite( buffer, 1, 128, exportfile ) != 128 ) {
      perror( "short write" );
      break;
    }
  }

  cpm_bdos( 16, (unsigned long int)&fcb );
  fclose( exportfile );

}

void usage( void )
{
  fprintf( stderr, "usage: exchange " );
  fprintf( stderr, "[-p112] " );
  fprintf( stderr, "imagefile\n" );
  exit( EXIT_FAILURE );
}

int main( int ArgC, char **ArgV )
{
  unsigned short int diskuser;
  struct termios t;
  int nextarg = 1;

  while( 1 ) {
    if( nextarg == ArgC ) usage();
    if( strcmp( "-p112", ArgV[ nextarg ] ) == 0 ) {
      bios_valid_dph = &bios_p112_dph;
      nextarg++;
      continue;
    }
    break;
  }

  if( nextarg != ( ArgC - 1 ) ) usage();

  bios_diskimage = fopen( ArgV[ nextarg ], "rb+" );
  if( bios_diskimage == NULL ) {
    perror( ArgV[ 1 ] );
    exit( EXIT_FAILURE );
  }

  if( tcgetattr( fileno( stdin ), &t ) != 0 ) {
    perror( "tcgetattr" );
    exit( EXIT_FAILURE );
  }

  if( tcgetattr( fileno( stdin ), &bios_original_t ) != 0 ) {
    perror( "tcgetattr" );
    exit( EXIT_FAILURE );
  }

  cfmakeraw( &t );

  if( tcsetattr( fileno( stdin ), TCSANOW, &t ) != 0 ) {
    perror( "tcsetattr" );
    exit( EXIT_FAILURE );
  }

  diskuser = bios_boot();
  cpm_bdosinit();
  cpm_bdos( 32, diskuser >> 8 );
  cpm_bdos( 14, diskuser & 0xff );

  while( 1 ) cpm_ccp();

}
