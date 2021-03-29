#ifdef RLI
#include "diverge.h"
#endif

/****************************************************************
*                                                               *
*               CP/M-68K BDOS File I/O Module                   *
*                                                               *
*       This module contains all file handling BDOS functions   *
*       except for read and write for CP/M-68K.  Included are:  *
*                                                               *
*               seldsk()    - select disk                       *
*               openfile()  - open file                         *
*               close_fi()  - close file                        *
*               search()    - search for first/next file match  *
*               create()    - create file                       *
*               delete()    - delete file                       *
*               rename()    - rename file                       *
*               set_attr()  - set file attributes               *
*               getsize()   - get file size                     *
*               setran()    - set random record field           *
*               free_sp()   - get disk free space               *
*               move()      - general purpose byte mover        *
*                                                               *
*                                                               *
*       Compiled with Alcyon C on the VAX                       *
*                                                               *
*	Modified 2/5/84 sw Allow odd DMA on get free space	*
*								*
****************************************************************/

#include "bdosinc.h"            /* Standard I/O declarations */

#include "bdosdef.h"            /* Type and structure declarations for BDOS */

#include "pktio.h"              /* Packet I/O definitions */

/* declare external fucntions */
EXTERN UWORD    dirscan();      /* directory scanning routine   */
EXTERN UWORD    error();        /* disk error routine           */
EXTERN UWORD    ro_err();       /* read-only file error routine */
EXTERN UWORD    do_phio();      /* packet disk i/o handler      */
EXTERN          clraloc();      /* clear bit in allocation vector */
EXTERN          setaloc();      /* set bit in allocation vector */
EXTERN UWORD    swap();         /* assembly language byte swapper */
EXTERN UWORD    dir_wr();       /* directory write routine */
EXTERN          tmp_sel();      /* temporary select disk routine */
EXTERN UWORD    calcext();      /* calc max extent allocated for fcb */
EXTERN UWORD    udiv();         /* unsigned divide routine      */

/* declare external variables */
EXTERN UWORD    log_dsk;        /* logged-on disk vector        */
EXTERN UWORD    ro_dsk;         /* read-only disk vector        */
EXTERN UWORD    crit_dsk;       /* vector of disks in critical state    */


/************************************
*  This function passed to dirscan  *
*       from seldsk (below)         *
************************************/

BOOLEAN alloc(fcbp, dirp, dirindx)
/* Set up allocation vector for directory entry pointed to by dirp */

struct fcb      *fcbp;          /* not used in this function    */
REG struct dirent *dirp;        /* pointer to directory entry   */
WORD            dirindx;        /* index into directory for *dirp */
{
    REG WORD    i;              /* loop counter */
    BSETUP

    if ( UBWORD(dirp->entry) < 0x10 )   /* skip MP/M 2.x and CP/M 3.x XFCBs */
    {
        (GBL.dphp)->hiwater = dirindx;  /* set up high water mark for disk */
        i = 0;
        if ((GBL.parmp)->dsm < 256)
        {
            do setaloc( UBWORD(dirp->dskmap.small[i++]) );
                while (i <= 15);
        }
        else
        {
            do setaloc(swap(dirp->dskmap.big[i++]));
                while (i <= 7);
        }
    }
}


/************************
*  seldsk entry point   *
************************/

seldsk(dsknum)

REG UBYTE dsknum;               /* disk number to select */

{
    struct iopb selpkt;
    REG WORD    i;
    UWORD       j;
    REG UBYTE   logflag;
    BSETUP

    logflag = ~(log_dsk >> dsknum) & 1;
    if ((GBL.curdsk != dsknum) || logflag)
    {                           /* if not last used disk or not logged on */
        selpkt.iofcn = sel_info;
        GBL.curdsk = (selpkt.devnum = dsknum);
        if (UBWORD(dsknum) > 15) error(2);
        selpkt.ioflags = logflag ^ 1;
        do
        {
            do_phio(&selpkt);   /* actually do the disk select  */
            if ( (void *)(GBL.dphp = selpkt.infop) != NULL ) break;
        } while ( ! error(3) );

        GBL.dirbufp = (void *)((GBL.dphp)->dbufp);
                        /* set up GBL copies of dir_buf and dpb ptrs */
        GBL.parmp = (GBL.dphp)->dpbp;
    }
    if (logflag)
    {           /* if disk not previously logged on, do it now */
        LOCK    /* must lock the file system while messing with alloc vec */
        i = (GBL.parmp)->dsm;
        do clraloc(i); while (i--);     /* clear the allocation vector */
        i = udiv( (LONG)(((GBL.parmp)->drm) + 1), 
                  4 * (((GBL.parmp)->blm) + 1), &j);
                                        /* calculate nmbr of directory blks */
        if (j) i++;                     /* round up */
        do setaloc(--i); while (i);     /* alloc directory blocks */
        dirscan(alloc, NULL, 0x0e);     /* do directory scan & alloc blocks */
        log_dsk |= 1 << dsknum;         /* mark disk as logged in       */
    }
}


/*******************************
*  General purpose byte mover  *
*******************************/

move(p1, p2, i)

REG BYTE *p1;
REG BYTE *p2;
REG WORD  i;
{
    while (i--)
        *p2++ = *p1++;
}


/*************************************
*  General purpose filename matcher  *
*************************************/

BOOLEAN match(p1, p2, chk_ext)

REG UBYTE *p1;
REG UBYTE *p2;
BOOLEAN  chk_ext;
{
    REG WORD    i;
    REG UBYTE temp;
    BSETUP

    i = 12;
    do
    {
        temp = (*p1 ^ '?');
        if ( ((*p1++ ^ *p2++) & 0x7f) && temp )
            return(FALSE);
        i -= 1;
    } while (i);
    if (chk_ext)
    {
        if ( (*p1 != '?') && ((*p1 ^ *p2) & ~((GBL.parmp)->exm)) )
            return(FALSE);
        p1 += 2;
        p2 += 2;
        if ((*p1 ^ *p2) & 0x3f) return(FALSE);
    }
    return(TRUE);
}


/************************
*  openfile entry point *
************************/

BOOLEAN openfile(fcbp, dirp, dirindx)

REG struct fcb *fcbp;           /* pointer to fcb for file to open */
struct dirent  *dirp;           /* pointer to directory entry   */
WORD    dirindx;

{
    REG UBYTE fcb_ext;          /* extent field from fcb        */
    REG BOOLEAN rtn;
    BSETUP

    if ( rtn = match(fcbp, dirp, TRUE) )
    {
        fcb_ext = fcbp->extent;  /* save extent number from user's fcb */
        move(dirp, fcbp, sizeof *dirp);
                                /* copy dir entry into user's fcb  */
        fcbp->extent = fcb_ext;
        fcbp->s2 |= 0x80;        /* set hi bit of S2 (write flag)       */
        crit_dsk |= 1 << (GBL.curdsk);
    }
   return(rtn);
}


/*************************/
/* flush buffers routine */
/*************************/

UWORD flushit()
{
    REG UWORD   rtn;            /* return code from flush buffers call */
    struct iopb flushpkt;       /* I/O packet for flush buffers call */

    flushpkt.iofcn = flush;
    while ( rtn = do_phio(&flushpkt) )
        if ( error(1) ) break;
    return(rtn);
}


/*********************************
* file close routine for dirscan *
*********************************/

BOOLEAN close(fcbp, dirp, dirindx)

REG struct fcb *fcbp;           /* pointer to fcb */
REG struct dirent *dirp;        /* pointer to directory entry */
WORD    dirindx;                /* index into directory */

{
    REG WORD  i;
    REG UBYTE *fp;
    REG UBYTE *dp;
    REG UWORD fcb_ext;
    REG UWORD dir_ext;
    BSETUP

    if ( match(fcbp, dirp, TRUE) )
    {                   /* Note that FCB merging is done here as a final
                           confirmation that disks haven't been swapped */
        LOCK
        fp = &(fcbp->dskmap.small[0]);
        dp = &(dirp->dskmap.small[0]);
        if ((GBL.parmp)->dsm < 256)
        {               /* Small disk map merge routine  */
            i = 16;
            do
            {
                if (*dp)
                {
                    if (*fp)
                    {
                        if (*dp != *fp) goto badmerge;
                    }
                    else *fp = *dp;
                }
                else *dp = *fp;
                fp += 1;
                dp += 1;
                i -= 1;
            } while (i);
        }
        else
        {               /* Large disk map merge routine */
            i = 8;
            do
            {
                if (*(UWORD *)dp)
                {
                    if (*(UWORD *)fp)
                    {
                        if (*(UWORD *)dp != *(UWORD *)fp) goto badmerge;
                    }
                    else *(UWORD *)fp = *(UWORD *)dp;
                }
                else *(UWORD *)dp = *(UWORD *)fp;
                (UWORD *)fp += 1;
                (UWORD *)dp += 1;
                i -= 1;
            } while (i);
        }
        /* Disk map merging complete */
        fcb_ext = calcext(fcbp);        /* calc max extent for fcb */
        dir_ext = (UWORD)(dirp->extent) & 0x1f;
        if ( (fcb_ext > dir_ext) || 
            ((fcb_ext == dir_ext) && 
                (UBWORD(fcbp->rcdcnt) > UBWORD(dirp->rcdcnt))) )
                        /* if fcb points to larger file than dirp */
        {
            dirp->rcdcnt = fcbp->rcdcnt;        /* set up rc, ext from fcb */
            dirp->extent = (BYTE)fcb_ext;
        }
        dirp->s1 = fcbp->s1;
        if ( (dirp->ftype[robit]) & 0x80) ro_err(fcbp,dirindx);
                                                /* read-only file error */
        dirp->ftype[arbit] &= 0x7f;             /* clear archive bit        */
        dir_wr(dirindx >> 2);
        UNLOCK
        return(TRUE);

badmerge:
        UNLOCK
        ro_dsk |= (1 << GBL.curdsk);
        return(FALSE);
    }
    else return(FALSE);
}


/************************
*  close_fi entry point *
************************/

UWORD close_fi(fcbp)

struct fcb *fcbp;               /* pointer to fcb for file to close */
{
    flushit();                          /* first, flush the buffers     */
    if ((fcbp->s2) & 0x80) return(0);   /* if file write flag not on,
                                           don't need to do physical close */
    return( dirscan(close, fcbp, 0));   /* call dirscan with close function */
}


/************************
*  search entry point   *
************************/

/* First two functions for dirscan */

BOOLEAN alltrue(p1, p2, i)
UBYTE   *p1;
UBYTE   *p2;
WORD    i;
{
    return(TRUE);
}
 
BOOLEAN matchit(p1, p2, i)
UBYTE   *p1;
UBYTE   *p2;
WORD    i;
{
    return(match(p1, p2, TRUE));
}


/* search entry point */ 
UWORD search(fcbp, dsparm, p)

REG struct fcb *fcbp;           /* pointer to fcb for file to search */
REG UWORD dsparm;               /* parameter to pass through to dirscan */
UBYTE   *p;                     /* pointer to pass through to tmp_sel   */

{
    REG UWORD   rtn;            /* return value */
    BSETUP

    if (fcbp->drvcode == '?')
    {
        seldsk(GBL.dfltdsk);
        rtn = dirscan(alltrue, fcbp, dsparm);
    }
    else
    {
        tmp_sel(p);             /* temporarily select disk */
        if (fcbp->extent != '?') fcbp->extent = 0;
        fcbp->s2 = 0;
        rtn = dirscan(matchit, fcbp, dsparm);
    }
    move( GBL.dirbufp, GBL.dmaadr, SECLEN);
    return(rtn);
}


/************************
*  create entry point   *
************************/

BOOLEAN create(fcbp, dirp, dirindx)

REG struct fcb *fcbp;           /* pointer to fcb for file to create */
REG struct dirent *dirp;        /* pointer to directory entry   */
REG WORD dirindx;               /* index into directory         */

{
    REG BYTE *p;
    REG WORD i;
    REG BOOLEAN rtn;
    BSETUP

    if ( rtn = ((dirp->entry) == (UBYTE)0xe5) )
    {
        p = &(fcbp->rcdcnt);
        i = 17;
        do
        {                       /* clear fcb rcdcnt and disk map */
            *p++ = 0;
            i -= 1;
        } while (i);
        move(fcbp, dirp, sizeof *dirp); /* move the fcb to the directory */
        dir_wr(dirindx >> 2);           /* write the directory sector */
        if ( dirindx > (GBL.dphp)->hiwater )
            (GBL.dphp)->hiwater = dirindx;
        crit_dsk |= 1 << (GBL.curdsk);
    }
    return(rtn);
}


/************************
*  delete entry point   *
************************/

BOOLEAN delete(fcbp, dirp, dirindx)

REG struct fcb *fcbp;           /* pointer to fcb for file to delete */
REG struct dirent *dirp;        /* pointer to directory entry   */
REG WORD dirindx;               /* index into directory         */

{
    REG WORD i;
    REG BOOLEAN rtn;
    BSETUP

    if ( rtn = match(fcbp, dirp, FALSE) )
    {
        if ( (dirp->ftype[robit]) & 0x80 ) ro_err(fcbp,dirindx);
                                /* check for read-only file */
        dirp->entry = 0xe5;
        LOCK
        dir_wr(dirindx >> 2);
        /* Now free up the space in the allocation vector */
        if ((GBL.parmp)->dsm < 256)
        {
            i = 16;
            do clraloc(UBWORD(dirp->dskmap.small[--i]));
                while (i);
        }
        else
        {
            i = 8;
            do clraloc(swap(dirp->dskmap.big[--i]));
                while (i);
        }
        UNLOCK
    }
    return(rtn);
}


/************************
*  rename entry point   *
************************/

BOOLEAN rename(fcbp, dirp, dirindx)

REG struct fcb *fcbp;           /* pointer to fcb for file to delete */
REG struct dirent *dirp;        /* pointer to directory entry   */
REG WORD dirindx;               /* index into directory         */

{
    REG UWORD i;
    REG BYTE *p;                /* general purpose pointers */
    REG BYTE *q;
    REG BOOLEAN rtn;
    BSETUP

    if ( rtn =  match(fcbp, dirp, FALSE) )
    {
        if ( (dirp->ftype[robit]) & 0x80 ) ro_err(fcbp,dirindx);
                                /* check for read-only file */
        p = &(fcbp->dskmap.small[1]);
        q = &(dirp->fname[0]);
        i = 11;
        do
        {
            *q++ = *p++ & 0x7f;
            i -= 1;
        } while (i);
        dir_wr(dirindx >> 2);
    }
    return(rtn);
}


/************************
*  set_attr entry point *
************************/

BOOLEAN set_attr(fcbp, dirp, dirindx)

REG struct fcb *fcbp;           /* pointer to fcb for file to delete */
REG struct dirent *dirp;        /* pointer to directory entry   */
REG WORD dirindx;               /* index into directory         */

{
    REG BOOLEAN rtn;
    BSETUP

    if ( rtn = match(fcbp, dirp, FALSE) )
    {
        move(&fcbp->fname[0], &dirp->fname[0], 11);
        dir_wr(dirindx >> 2);
    }
    return(rtn);
}


/****************************
*  utility routine used by  *
*  setran and getsize       *
****************************/

LONG extsize(fcbp)
/* Return size of extent pointed to by fcbp */
REG struct fcb *fcbp;

{
    return( ((LONG)(fcbp->extent & 0x1f) << 7)
                | ((LONG)(fcbp->s2 & 0x3f) << 12) );
}


/************************
*  setran entry point   *
************************/

setran(fcbp)

REG struct fcb *fcbp;           /* pointer to fcb for file to set ran rec */

{
    union {
       struct
       {
        BYTE b3;
        BYTE b2;
        BYTE b1;
        BYTE b0;
      } bytes;
      LONG random;
    } random;

    random.random = (LONG)UBWORD(fcbp->cur_rec) + extsize(fcbp);
                                /* compute random record field  */
    fcbp->ran0 = random.bytes.b2;
    fcbp->ran1 = random.bytes.b1;
    fcbp->ran2 = random.bytes.b0;
}


/**********************************/
/* fsize is a funtion for dirscan */
/* passed from getsize            */
/**********************************/

BOOLEAN fsize(fcbp, dirp, dirindx)

REG struct fcb *fcbp;           /* pointer to fcb for file to delete */
REG struct dirent *dirp;        /* pointer to directory entry   */
WORD dirindx;                   /* index into directory         */

{
    REG BOOLEAN rtn;
    union {
      struct
      {
        BYTE b3;
        BYTE b2;
        BYTE b1;
        BYTE b0;
      } bytes;
      LONG longs;
    } temp;

    if ( rtn = match(fcbp, dirp, FALSE) )
    {
        temp.longs = (LONG)UBWORD(dirp->rcdcnt) + extsize(dirp);
                                /* compute file size    */
        fcbp->ran0 = temp.bytes.b2;
        fcbp->ran1 = temp.bytes.b1;
        fcbp->ran2 = temp.bytes.b0;
    }
    return(rtn);
}

/************************
*  getsize entry point  *
************************/

getsize(fcbp)
/* get file size        */
REG struct fcb *fcbp;           /* pointer to fcb to get file size for */

{
    REG WORD dsparm;
    union {
      struct
      {
        BYTE b3;
        BYTE b2;
        BYTE b1;
        BYTE b0;
      } bytes;
      LONG longs;
    } maxrcd, temp;

    maxrcd.longs = 0;
    dsparm = 0;
    temp.longs = 0;
    while ( dirscan(fsize, fcbp, dsparm) < 255 )
    {                           /* loop until no more matches */
        temp.bytes.b2 = fcbp->ran0;
        temp.bytes.b1 = fcbp->ran1;
        temp.bytes.b0 = fcbp->ran2;
        if (temp.longs > maxrcd.longs ) maxrcd.longs = temp.longs;
        dsparm = 1;
    }
    fcbp->ran0 = maxrcd.bytes.b2;
    fcbp->ran1 = maxrcd.bytes.b1;
    fcbp->ran2 = maxrcd.bytes.b0;
}


/************************
*  free_sp entry point  *
************************/

free_sp(dsknum)

UBYTE dsknum;           /* disk number to get free space of */
{
    REG LONG records;
    REG UWORD   *alvec;
    REG UWORD   bitmask;
    REG UWORD   alvword;
    REG WORD    i;
	LONG	temp;		/*sw For DMA Odd problem	*/
    BSETUP

    seldsk(dsknum);             /* select the disk */
    records = (LONG)0;          /* initialize the variables */
    alvec = (void *)(GBL.dphp)->alv;
    bitmask = 0;
    for (i = 0; i <= (GBL.parmp)->dsm; i++)     /* for loop to compute */
    {
        if ( ! bitmask)
        {
            bitmask = 0x8000;
            alvword = ~(*alvec++);
        }
        if ( alvword & bitmask)
            records += (LONG)( ((GBL.parmp)->blm) + 1 );
        bitmask >>= 1;
    }
    temp = records;			 /*sw Put in memory		 */
    move(&temp,GBL.dmaadr,sizeof(LONG)); /*sw Move to user's DMA	 */
}
