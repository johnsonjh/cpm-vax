#ifdef RLI
#include "diverge.h"
#endif

/****************************************************************
*                                                               *
*               CP/M-68K BDOS Miscellaneous Module              *
*                                                               *
*       This module contains miscellaneous loose ends for       *
*       CP/M-68K.  Included are:                                *
*                                                               *
*               bdosinit()  - BDOS initialization routine       *
*                             called from CCP for system init   *
*               warmboot()  - BDOS warm boot exit routine       *
*               error()     - BDOS error printing routine       *
*               ro_err()    - BDOS read-only file error routine *
*               setexc()    - BDOS set exception vector         *
*               set_tpa()   - BDOS get/set TPA limits           *
*               serial # and copyright notice, machine readable *
*                                                               *
*                                                               *
*       Configured for Alcyon C on the VAX                      *
*                                                               *
*	Modified 2/5/84 sw for ^C disk reset.			*
*	Again    3/17/84   for chain hack			*
*								*
****************************************************************/

#include "bdosinc.h"            /* Standard I/O declarations */

#include "bdosdef.h"            /* Type and structure declarations for BDOS */

#include "biosdef.h"            /* BIOS definitions, needed for bios wboot */


/* serial # and copyright notice */

char *copyrt="CP/M-68K(tm), Version 1.2, Copyright (c) 1984, Digital Research";
char *serial="XXXX-0000-654321";



/*  Declare external functions */
EXTERN          conout();               /* Console Output function      */
EXTERN UBYTE    conin();                /* Console Input function       */
EXTERN          prt_line();             /* Print String function        */
EXTERN UWORD    _bdos();                /* BDOS main routine            */
EXTERN UBYTE    *traphndl();            /* assembly language trap handler */
EXTERN          initexc();              /* init the exception handler in  */
                                        /* exceptn.s                    */
EXTERN UWORD    dirscan();              /* Directory scanning routine   */
EXTERN BOOLEAN  set_attr();             /* Set File attributes function */
EXTERN UWORD    dir_rd();               /* Read directory sector routine */

/*  Declare external variables */
EXTERN  UWORD   log_dsk;                /* logged-on disk vector        */
EXTERN  UWORD   ro_dsk;                 /* read-only disk vector        */
EXTERN  UWORD   crit_dsk;               /* vector of critical disks     */
EXTERN  BYTE    *tpa_lt;                /* TPA lower limit (temporary)  */
EXTERN  BYTE    *tpa_lp;                /* TPA lower limit (permanent)  */
EXTERN  BYTE    *tpa_ht;                /* TPA upper limit (temporary)  */
EXTERN  BYTE    *tpa_hp;                /* TPA upper limit (permanent)  */
EXTERN  BOOLEAN submit;                 /* external variables from CCP  */
EXTERN  BOOLEAN morecmds;


#define trap2v 34                       /* trap 2 vector number */
#define ctrlc  3                        /* control-c            */


/********************************
*  bdos initialization routine  *
********************************/

bdosinit()
/* Initialize the File System */
{
    REG struct
    {
        WORD    nmbr;
        BYTE    *low;
        LONG    length;
    } *segp;
    BSETUP

    bsetvec(trap2v, &traphndl); /* set up trap vector */
    GBL.kbchar = 0;             /* initialize the "global" variables */
    GBL.insptr = GBL.remptr = &(GBL.t_buff[0]);
    GBL.delim  = '$';
    GBL.lstecho = FALSE;
    GBL.echodel = TRUE;
    chainp  = NULL;		/*sw Used to be GBL.chainp */
    _bdos(13);                  /* reset disk system function */
    segp = (void *)bgetseg();   /* get pointer to memory segment table */
    tpa_lt = tpa_lp = segp->low;
    tpa_ht = tpa_hp = tpa_lp + segp->length;
    initexc( &(GBL.excvec[0]) );
}


/************************
*  warmboot entry point *
************************/

warmboot(parm)
/* Warm Boot the system */
WORD parm;                      /* 1 to reset submit flag */
{
    BSETUP

    if(parm != 2)		/*sw Not ^C	*/
      log_dsk &= ~ro_dsk;       /* log off any disk marked read-only */
    else
      log_dsk &= (1 << GBL.curdsk);  /*sw Log off all but current drive, as */
				     /*   per manual.  (^C only) */

                        /* note that this code is specifically for a single-
                           thread system.  It won't work in a multi-task sys */
			/*sw The above is still very much true */
    ro_dsk = 0;
    crit_dsk = 0;
    if (parm)
        submit = morecmds = FALSE;
    GBL.curdsk = 0xff;                  /* set current disk to "unknown" */
    tpa_lt = tpa_lp;
    tpa_ht = tpa_hp;
    initexc( &(GBL.excvec[0]) );
    bwboot();
}


/*************************/
/*  disk error handlers  */
/*************************/

prt_err(p)
/*  print the error message  */

BYTE  *p;
{
    BSETUP

    prt_line(p);
    prt_line(" error on drive $");
    conout(GBL.curdsk + 'A');
}


abrt_err(p)
/*  print the error message and always abort */

BYTE  *p;
{
    prt_err(p);
    warmboot(1);
}

char *warning = "\r\nWARNING -- Do not attempt to change disks$";
ext_err(cont,p)
/*  print the error message, and allow for retry, abort, or ignore */

REG BOOLEAN cont;       /* Boolean for whether continuing is allowed */
BYTE  *p;               /* pointer to error message             */
{
    REG UBYTE  ch;

    prt_err(p);
    prt_line(warning);
    do
    {
        prt_line("\n\rDo you want to:  Abort (A),  Retry (R)$");
        if (cont) prt_line(", or Continue with bad data (C)$");
        prt_line("? $");
        ch = conin() & 0x5f;
        prt_line("\r\n$");

        switch ( ch )
        {
            case ctrlc: warmboot(1);
            case 'A':   warmboot(1);
            case 'C':   if (cont) return(1);
                        break;
            case 'R':   return(0);
        }
    }   while (TRUE);
}


/********************************/
/* Read-only File Error Routine */
/********************************/

ro_err(fcbp,dirindx)
/*  File R/O error  */

REG struct fcb *fcbp;
WORD            dirindx;
{
    REG BYTE *p;
    REG UWORD i;
    REG UBYTE  ch;

    p = (BYTE *)fcbp;
    prt_line("CP/M Disk file error: $");
    i = 8;
    do conout(*++p & 0x7f); while (--i);
    conout('.');
    i = 3;
    do conout(*++p & 0x7f); while (--i);
    prt_line(" is read-only.$");
    prt_line(warning);
    do
    {
 prt_line("\r\nDo you want to: Change it to read/write (C), or Abort (A)? $");
        ch = conin() & 0x5f;
        prt_line("\r\n$");

        switch ( ch )
        {
            case ctrlc: warmboot(1);
            case 'A':   warmboot(1);
            case 'C':   fcbp->ftype[robit] &= 0x7f;
                        dirscan(set_attr, fcbp, 2);
                        return(dir_rd(dirindx >> 2));
        }                       /* Reset the directory buffer !!!! */
    }   while (TRUE);
}


/************************
*  error entry point    *
************************/

error(errnum)
/* Print error message, do appropriate response */

UWORD errnum;                   /* error number */
{
    BSETUP

    prt_line("\r\nCP/M Disk $");
    switch (errnum)
    {
        case 0:  return( ext_err(TRUE,"read$") );
                 /* break; */

        case 1:  return( ext_err(TRUE,"write$") );
                 /* break; */

        case 2:  abrt_err("select$");
                 /* break; */

        case 3:  return( ext_err(FALSE,"select$") );
                 /* break; */

        case 4:  abrt_err("change$");
                 /* break; */

    }
}


/*****************************
*  set exception entry point *
*****************************/

struct setexc_struct {
  WORD vecnum;
  BYTE *newvec;
  BYTE *oldvec;
};

setexc(epbp)
/* Set Exception Vector */
REG struct setexc_struct *epbp;

{
    REG WORD i;
    BSETUP

    i = epbp->vecnum-2;
    if ( i==32 || i==33) return(-1);
    if ( (30 <= i) && (i <= 37) ) i -= 20;
    else if ( (i < 0) || (i > 9) ) return(255);
    epbp->oldvec = GBL.excvec[i];
    GBL.excvec[i] = epbp->newvec;
    return(0);
}


/*****************************
*  get/set TPA entry point   *
*****************************/

struct set_tpa_struct {
  UWORD parms;
  BYTE *low;
  BYTE *high;
};

set_tpa(p)
/* Get/Set TPA Limits */
REG struct set_tpa_struct *p;

#define set     1
#define sticky  2

{
    if (p->parms & set)
    {
        tpa_lt = p->low;
        tpa_ht = p->high;
        if (p->parms & sticky)
        {
            tpa_lp = tpa_lt;
            tpa_hp = tpa_ht;
        }
    }
    else
    {
        p->low = tpa_lt;
        p->high = tpa_ht;
    }
}
