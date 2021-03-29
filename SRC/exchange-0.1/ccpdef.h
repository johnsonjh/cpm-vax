/*--------------------------------------------------------------*\
 |	ccp_def.c	       DEFINES	                  v1.0  |
 |			       =======				|
 |								|
 |	CP/M 68k:  A CP/M derived operating system		|
 |								|
 |      File contents:						|
 |      -------------						|
 |			This file contains all of the #defines  |
 |			used by the console command processor.  |
 |								|
 |	created by   :  Tom Saulpaugh     Date:  7/13/82	|
 |      ----------						|
 |      last modified:  10/29/82				|
 |	-------------						|
 |								|
 |      (c) COPYRIGHT   Digital Research 1982			|
 |	all rights reserved					|
 |								|
\*--------------------------------------------------------------*/


	/*-------------------------------------------*\
	 |	     CP/M Transient Commands         |
	\*-------------------------------------------*/
						
#define		DIRCMD		0
#define		TYPECMD		1
#define		RENCMD		2
#define		ERACMD		3
#define		UCMD		4
#define	        CH_DISK 	5
#define		SUBCMD		6
#define		SUB_FILE    	7
#define		FILE    	8
#define		DIRSCMD		9
#define		SEARCH		10

#ifdef RLI
#define RLI_EXIT 11
#define RLI_IMPORT 12
#define RLI_EXPORT 13
#endif
		
	/*-------------------------------------------*\
	 |	        Modes and Flags              |
	\*-------------------------------------------*/

#define		ON	1
#define		OFF	0
#define		MATCH   0
#define		GOOD	1
#define		BAD	0
#define		FILL	1
#define		NOFILL  0
#define		VOID	/*no return value*/
#define		NO_FILE 98
#define		STOP    99
#define		USER_ZERO 0
#define		DISK_A	1
#define		SOURCEDRIVE	88
#define		DESTDRIVE	99
#define		BYTE	signed char
#define		REG	register
#define		WORD	short
#define		UWORD   unsigned short int /*** rli ***/
#define		LONG	long
#define		ULONG   unsigned long
#define		GET_MEM_REG 18
#define		ZERO	0
#define		NULL	'\0'
#define		TRUE	1
#define		FALSE	0
#define		NO_READ	255
#define		BLANK	' '
#define		BACKSLH '\\'
#define		EXLIMPT '!'
#define		CMASK   0177
#define		ONE	(long)49
#define		TAB	9
#define		Cr	13
#define		Lf      10
#define		CR	(long)13
#define		LF	(long)10
#define		EOF	26
#define		BLANKS  (long)32
#define		PERIOD  (long)46
#define		COLON   (long)58
#define		ARROW   (long)62

	/*-------------------------------------------*\
	 |	  Data Structure Size Constants      |
	\*-------------------------------------------*/

#define		CMD_LEN		128
#define		BIG_CMD_LEN	255
#define		MAX_ARGS	4
#define		ARG_LEN		30
#define		NO_OF_DRIVES    16
#define		NUMDELS		16
#define		FCB_LEN		36
#define		DMA_LEN	        128
#define		FILES_PER_LINE  5
#define		SCR_HEIGHT	23
#define		BIG_WIDTH	80
#define		SMALL_WIDTH     40

	/*-------------------------------------------*\
	 |             BDOS Function Calls	     |
 	\*-------------------------------------------*/

#define		WARMBOOT	0
#define		CONIN		1
#define		CONSOLE_OUTPUT	2
#define		READER_INPUT	3
#define		PUNCH_OUTPUT	4
#define		LIST_OUTPUT	5
#define		DIR_CONS_IO	6
#define		GET_IO_BYTE	7
#define		SET_IO_BYTE	8
#define		PRINT_STRING	9
#define		READ_CONS_BUF	10
#define		GET_CONS_STAT	11
#define		RET_VERSION_NO	12
#define		RESET_DISK_SYS	13
#define		SELECT_DISK	14
#define		OPEN_FILE	15
#define		CLOSE_FILE	16
#define		SEARCH_FIRST	17
#define		SEARCH_NEXT	18
#define		DELETE_FILE	19
#define		READ_SEQ	20
#define		WRITE_SEQ	21
#define		MAKE_FILE	22
#define		RENAME_FILE	23
#define		RET_LOGIN_VEC   24
#define		RET_CUR_DISK	25
#define		SET_DMA_ADDR	26
#define		GET_ADDR(ALLOC) 27
#define		WRITE_PROT_DISK 28
#define		GET_READO_VEC  29
#define		SET_FILE_ATTRIB 30
#define		GET_ADDR_D_PARM 31
#define		GET_USER_NO     32
#define		READ_RANDOM	33
#define		WRITE_RANDOM	34
#define		COMP_FILE_SIZE	35
#define		SET_RANDOM_REC	36
#define		RESET_DRIVE	37
#define		WRITE_RAN_ZERO  40
#define		BIOS_CALL	50
#define		LOAD_PROGRAM    59

	/*----------------------------------------------*\
	 |		      MACROS			|
	\*----------------------------------------------*/

#define		isalpha(c)	(islower(c) || isupper(c))
#define		islower(c)	('a' <= (c) && (c) <= 'z')
#define		isupper(c)	('A' <= (c) && (c) <= 'Z')
#define		tolower(c)	(isupper(c) ? ((c)+040):(c))
#define		toupper(c)	(islower(c) ? ((c)-040):(c))
#define		isdigit(c)	('0' <= (c) && (c) <= '9')
