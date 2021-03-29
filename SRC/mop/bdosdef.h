/********************************************************
*                                                       *
*               CP/M-68K header file                    *
*    Copyright (c) 1982 by Digital Research, Inc.       *
*    Structure definitions for BDOS globals             *
*       and BDOS data structures                        *
*                                                       *
*       Desecrated 6-Aug-83 (sw) for type-ahead         *
*	Again	  17-Mar-84 (sw) for chaining		*
*                                                       *
********************************************************/

/**************************************************************************
The BDOS data structures, especially those relating to global variables, 
are structured in a way that hopefully will enable this BDOS, in the future,
to easily become a re-entrant multi-tasking file system.  Consequently, 
the BDOS global variables are divided into two classes.  Those that are
truly global, even in the case of multiple tasks using the file system
concurrently, are simply declared as global variables in bdosmain.c.
Only a few "globals" are really global in this sense.

The majority of the "global" variables are actually state variables that
relate to the state of the task using the file system.  In CP/M-68K, these
are "global", since there's only one task, but in a multi-thread model they're
not.  This type of variables is put into a data structure, with the
intention that in the multi-task environment this structure will be based.

The following declarations take this philosophy into account, and define
a simple structure for the single thread environment while leaving the
possibilities open for the multi-thread environment.
****************************************************************************/

#define snglthrd TRUE
                        /* TRUE for single-thread environment
                        FALSE to create based structure for re-entrant model */
#if snglthrd
#define GBL gbls 
                                /* In single thread case, GBL just names
                                        the structure */
#define BSETUP  EXTERN struct stvars gbls;
                                /* and BSETUP defines the extern structure */
#endif

#if ! snglthrd
#define GBL (*statep)
                                /* If multi-task, state vars are based */
#define BSETUP  REG struct stvars *statep; \
          statep = &gbls;
                                /* set up pointer to state variables */
                        /* This is intended as an example to show the intent */
#endif


/* Note that there are a few critical regions in the file system that must 
   execute without interruption.  They pertain mostly to the manipulation of
   the allocation vector.  This isn't a problem in a single-thread model, but
   must be provided for in a multi-tasking file system.  Consequently, the
   primitives LOCK and UNLOCK are defined and used where necessary in the 
   file system.  For the single thread model, they are null routines       */

#define LOCK    /**/ 
#define UNLOCK  /**/
/* Be sure LOCK and UNLOCK are implemented to allow recursive calls to LOCK.
   That is, if a process that calls LOCK already owns the lock, let it proceed,
   but remember that only the outer-most call to UNLOCK really releases the
   file system.         */

 
#define VERSION 0x2022          /* Version number for CP/M-68K             */
#define robit 0                 /* read-only bit in file type field of fcb */
#define arbit 2                 /* archive bit in file type field of fcb   */
#define SECLEN 128              /* length of a CP/M sector                 */


/* File Control Block definition */
struct fcb
{
        UBYTE   drvcode;        /* 0 = default drive, 1..16 are drives A..P */
        UBYTE   fname[8];       /* File name (ASCII)                    */
        UBYTE   ftype[3];       /* File type (ASCII)                    */
        UBYTE   extent;         /* Extent number (bits 0..4 used)       */
        UBYTE   s1;             /* Reserved                             */
        UBYTE   s2;             /* Module field (bits 0..5), write flag (7) */
        UBYTE   rcdcnt;         /* Nmbr rcrds in last block, 0..128     */
        union
        {
          UBYTE small[16];      /* 16 block numbers of 1 byte           */
          WORD  big[8];         /* or 8 block numbers of 1 word         */
        }       dskmap;
        UBYTE   cur_rec;        /* current record field                 */
        UBYTE   ran0;           /* random record field (3 bytes)        */
        UBYTE   ran1;
        UBYTE   ran2;
};


/* Declaration of directory entry       */
struct dirent
{
        UBYTE   entry;          /* 0 - 15 for user numbers, E5 for empty */
                                /* the rest are reserved                */
        UBYTE   fname[8];       /* File name (ASCII)                    */
        UBYTE   ftype[3];       /* File type (ASCII)                    */
        UBYTE   extent;         /* Extent number (bits 0..4 used)       */
        UBYTE   s1;             /* Reserved                             */
        UBYTE   s2;             /* Module field (bits 0..5), write flag (7) */
        UBYTE   rcdcnt;         /* Nmbr rcrds in last block, 0..128     */
        union
        {
          UBYTE small[16];      /* 16 block numbers of 1 byte           */
          WORD  big[8];         /* or 8 block numbers of 1 word         */
        }       dskmap;
};


/* Declaration of disk parameter tables         */
struct dpb                      /* disk parameter table         */
{
        UWORD   spt;            /* sectors per track            */
        UBYTE   bsh;            /* block shift factor           */
        UBYTE   blm;            /* block mask                   */
        UBYTE   exm;            /* extent mask                  */
        UBYTE   dpbdum;         /* dummy byte for fill          */
        UWORD   dsm;            /* max disk size in blocks      */
        UWORD   drm;            /* max directory entries        */
        UWORD   dir_al;         /* initial allocation for dir   */
        UWORD   cks;            /* number dir sectors to checksum */
        UWORD   trk_off;        /* track offset                 */
};

struct  dph                     /* disk parameter header        */
{
        UBYTE   *xlt;           /* pointer to sector translate table    */
        UWORD   hiwater;        /* high water mark for this disk        */
        UWORD   dum1;           /* dummy (unused)                       */
        UWORD   dum2;
        UBYTE   *dbufp;         /* pointer to 128 byte directory buffer */
        struct dpb *dpbp;       /* pointer to disk parameter block      */
        UBYTE   *csv;           /* pointer to check vector              */
        UBYTE   *alv;           /* pointer to allocation vector         */
};


/* Declaration of structure containing "global" state variables */
#define TBUFSIZ 126             /*sw # typed-ahead characters              */
struct stvars
{
        UBYTE   kbchar;         /* keyboard type-ahead buffer count        */
        UBYTE   delim;          /* Delimiter for function 9                */
        BOOLEAN lstecho;        /* True if echoing console output to lst:  */
        BOOLEAN echodel;        /* Echo char when getting <del> ?          */
        UWORD   column;         /* CRT column number for expanding tabs    */
        UBYTE   curdsk;         /* Currently selected disk                 */
        UBYTE   dfltdsk;        /* Default disk (last selected by fcn 14)  */
        UBYTE   user;           /* Current user number                     */
        struct dph *dphp;       /* pointer to disk parm hdr for cur disk   */
        struct dirent *dirbufp; /* pointer for directory buff for process  */
                                /* stored here so that each process can    */
                                /* have a separate dirbuf.                 */
        struct dpb *parmp;      /* pointer to disk parameter block for cur */
                                /* disk. Stored here to save ref calc      */
        UWORD   srchpos;        /* position in directory for search next   */
        UBYTE   *dmaadr;        /* Disk dma address                        */
        struct fcb *srchp;      /* Pointer to search FCB for function 17   */
        UBYTE   *excvec[18];    /* Array of exception vectors              */
        UBYTE   *insptr;        /*sw Insertion pointer for typeahead       */
        UBYTE   *remptr;        /*sw Removal pointer for typeahead         */
        UBYTE   t_buff[TBUFSIZ]; /*sw Type-ahead buffer itself             */
};
				/*sw removed next line from structure	   */
        UBYTE   *chainp;        /* Used for chain to program call          */


/* Console buffer structure declaration */
struct  conbuf
{
        UBYTE   maxlen;         /* Maximum length from calling routine */
        UBYTE   retlen;         /* Length actually found by BDOS */
        UBYTE   cbuf[0];        /* Console data                  */
};
