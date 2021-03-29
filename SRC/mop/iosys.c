#ifdef RLI
#include "diverge.h"
#endif

/****************************************************************
*                                                               *
*               CP/M-68K BDOS Disk I/O System Module            *
*                                                               *
*       This module translates from the packet oriented I/O     *
*       passed from the other BDOS modules into BIOS calls.     *
*                                                               *
*       It includes only one external entry point:
*               do_phio()   - do physical i/o                   *
*                                                               *
*                                                               *
*       Configured for Alcyon C on the VAX                      *
*                                                               *
****************************************************************/

#include "bdosinc.h"            /* Standard I/O declarations */

#include "bdosdef.h"            /* Type and structure declarations for BDOS */

#include "pktio.h"              /* Packet I/O definitions */

#include "biosdef.h"            /* Declarations for BIOS entry points */

EXTERN  udiv();                 /* Assembly language unsigned divide routine */
                                /* in bdosif.s.  It's used because Alcyon C  */
                                /* can't do / or % without an external */

/************************
*  do_phio entry point  *
************************/

UWORD do_phio(iop)

REG struct iopb *iop;           /* iop is a pointer to a i/o parameter block */

{
    MLOCAL UBYTE last_dsk;      /* static variable to tell which disk
                                     was last used, to avoid disk selects */
    REG struct dph *hdrp;         /* pointer to disk parameter header   */
    REG struct dpb *dparmp;       /* pointer to disk parameter block    */
    REG UWORD   rtn;              /* return parameter                   */
    UWORD       iosect;           /* sector number returned from divide rtn */

    LOCK                /* lock the disk system while doing physical i/o */

    rtn = 0;
    switch (iop->iofcn)
    {
        case sel_info:  
                last_dsk = iop->devnum;
                iop->infop = (void *)bseldsk(last_dsk, iop->ioflags);
                break;

        case read:
        case write:
                if (last_dsk != iop->devnum)
                    bseldsk((last_dsk = iop->devnum), 0);
                    /* guaranteed disk is logged on, because temp_sel in
                        BDOSMAIN does it        */
                hdrp = iop->infop;
                dparmp = hdrp->dpbp;

                bsettrk( udiv( iop->devadr, dparmp->spt, &iosect )
                         + dparmp->trk_off );
                bsetsec( bsectrn( iosect, hdrp->xlt ) );
                bsetdma(iop->xferadr);
                if ((iop->iofcn) == read) rtn = bread();
                else rtn = bwrite(iop->ioflags);
                break;

        case flush:
                rtn = bflush();
    }

    UNLOCK
    return(rtn);
}
