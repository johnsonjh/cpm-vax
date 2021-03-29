#ifdef RLI
#include "diverge.h"
#endif

/********************************************************
*                                                       *
*       CP/M-68K BDOS Character I/O Routines            *
*                                                       *
*       This module does BDOS functions 1 thru 11       *
*                                                       *
*       It contains the following functions which       *
*       are called from the BDOS main routine:          *
*               constat();                              *
*               conin();                                *
*               tabout();                               *
*               rawconio();                             *
*               prt_line();                             *
*               readline();                             *
*                                                       *
*       Copyright (c) 1982 Digital Research, Inc.       *
*                                                       *
*	Modified 2/5/84 sw Allow typeahead		*
*			   ^C warmboot modifications    *
*	Again	 3/17/84 sw Chain hack			*
*                                                       *
********************************************************/

#include "bdosinc.h"

#include "bdosdef.h"

#include "biosdef.h"


#define   ctrlc  0x03
#define   ctrle  0x05
#define   ctrlp  0x10
#define   ctrlq  0x11
#define   ctrlr  0x12
#define   ctrls  0x13
#define   ctrlu  0x15
#define   ctrlx  0x18

#define   cr      0x0d
#define   lf      0x0a
#define   tab     0x09
#define   rub     0x7f
#define   bs      0x08
#define   space   0x20

  
EXTERN  warmboot();             /* External function definition */


/******************/
/* console status */
/******************/

BOOLEAN constat()
{
    BSETUP

    return( GBL.kbchar ? TRUE : bconstat() );
}

/********************/
/* check for ctrl/s */
/* used internally  */
/********************/
conbrk()
{
    REG UBYTE ch;
    REG BOOLEAN stop;
    BSETUP

    stop = FALSE;
    if ( bconstat() ) do
    {
        if ( (ch = bconin()) == ctrlc ) warmboot(2);	/*sw from (1) */
        if ( ch == ctrls ) stop = TRUE;
        else if (ch == ctrlq) stop = FALSE;
        else if (ch == ctrlp) GBL.lstecho = !GBL.lstecho;
					/*sw What Follows is new...	    */
        else                            /* Insert character in ring buffer  */
        {                               /*                                  */
          if(GBL.kbchar < TBUFSIZ)      /* Room?                            */
          {                             /************************************/
            *GBL.insptr++ = ch;         /* Yes, insert the character in buff*/
             GBL.kbchar++;              /* Up count                         */
          }                             /************************************/
        }                               /* Note if no room, character is    */
                                        /* Ignomiously discarded (!)        */
					/*sw End of new stuff		    */
                                        /************************************/
    } while (stop);
}


/******************/
/* console output */
/* used internally*/
/******************/

conout(ch)
REG UBYTE ch;
{
    BSETUP

    conbrk();                   /* check for control-s break */
    bconout(ch);                /* output character to console */
    if (GBL.lstecho) blstout(ch);       /* if ctrl-p on, echo to list dev */
    if ((UWORD)ch >= (UWORD)' ') 
	GBL.column++; 		/* keep track of screen column */
    else if (ch == cr) GBL.column = 0;
    else if (ch == bs) GBL.column--;
}


/*************************************/
/* console output with tab expansion */
/*************************************/

tabout(ch)
REG UBYTE ch;           /* character to output to console       */
{
    BSETUP

    if (ch == tab) do
        conout(' ');
    while (GBL.column & 7);
    else conout(ch);
}

/*******************************/
/* console output with tab and */
/* control character expansion */
/*******************************/

cookdout(ch)
REG UBYTE ch;           /* character to output to console       */
{
    if (ch == tab) tabout(ch);  /* if tab, expand it    */
    else
    {
        if ( (UWORD)ch < (UWORD)' ' )
        {
            conout( '^' );
            ch |= 0x40;
        }
    conout(ch);                 /* output the character */
    }
}


/*****************/
/* console input */
/*****************/

UBYTE getch()           /* Get char from buffer or bios */
                        /* For internal use only        */
{
    REG UBYTE temp;
    BSETUP

    if(GBL.kbchar)
    {
        temp = *GBL.remptr++;           /* Fetch the character    */
        GBL.kbchar--;                   /* Decrement the count    */
        if(!GBL.kbchar)                 /* Gone to zero?          */
                GBL.remptr = GBL.insptr = &(GBL.t_buff[0]);
        return(temp);
    }
    return( bconin() );                 /* else get char from bios */
}
    
UBYTE conin()           /* BDOS console input function */
{
    REG UBYTE ch;
    BSETUP

    conout( ch = getch() );
    if (ch == ctrlp) GBL.lstecho = !GBL.lstecho;
    return(ch);
}

/******************
* raw console i/o *
******************/

UBYTE rawconio(parm)    /* BDOS raw console I/O function */

REG UWORD parm;
{
    BSETUP

    if (parm == 0xff) return(getch());
    else if (parm == 0xfe) return(constat());
    else bconout(parm & 0xff);
}


/****************************************************/
/* print line up to delimiter($) with tab expansion */
/****************************************************/

prt_line(p)
REG UBYTE *p;
{
    BSETUP

    while( *p != GBL.delim ) tabout( *p++ );
}


/**********************************************/
/* read line with editing and bounds checking */
/**********************************************/

/* Two subroutines first */

newline(startcol)
REG UWORD startcol;
{
    BSETUP

    conout(cr);                 /* go to new line */
    conout(lf);
    while(startcol)
    {
        conout(' ');
        startcol -= 1;          /* start output at starting column */
    }
}


backsp(bufp, col)
/* backspace one character position     */
REG struct conbuf *bufp;        /* pointer to console buffer    */
REG WORD col;                   /* starting console column      */
{
    REG UBYTE   ch;             /* current character            */
    REG WORD    i;
    REG UBYTE   *p;             /* character pointer            */
    BSETUP

    if (bufp->retlen) --(bufp->retlen);
                                /* if buffer non-empty, decrease it by 1 */
    i = UBWORD(bufp->retlen);   /* get new character count      */
    p = &(bufp->cbuf[0]);       /* point to character buffer    */
    while (i--)                 /* calculate column position    */
    {                           /*  across entire char buffer   */
        ch = *p++;              /* get next char                */
        if ( ch == tab )
        {
            col += 8;
            col &= ~7;          /* for tab, go to multiple of 8 */
        }
        else if ( (UWORD)ch < (UWORD)' ' ) col += 2;
                                /* control chars put out 2 printable chars */
        else col += 1;
    }
    while (GBL.column > col)
    {
        conout(bs);             /* backspace until we get to proper column */
        conout(' ');
        conout(bs);
    }
}


readline(p)                     /* BDOS function 10 */
REG struct conbuf *p;
        
{
    REG UBYTE ch;
    REG UWORD i;
    REG UWORD j;
    REG UBYTE *q;
    UWORD stcol;

    BSETUP

    stcol = GBL.column;         /* set up starting column */

#ifdef	NFG			/*sw This didn't work for SUBMIT files...*/
    if (GBL.chainp != NULL)     /* chain to program code  */
    {
        i = UBWORD(*(GBL.chainp++));
        j = UBWORD(p->maxlen);
        if (j < i) i = j;               /* don't overflow console buffer! */
        p->retlen = (UBYTE)i;
        q = p->cbuf;
        while (i)
        {
            cookdout( *q++ = *(GBL.chainp++) );
            i -= 1;
        }
        GBL.chainp = NULL;
        return;
    }
#endif				/*sw NFG chain code		*/

    p->retlen = 0;              /* start out with empty buffer */
    while ( UBWORD(p->retlen) < UBWORD(p->maxlen) )
    {                           /* main loop for read console buffer */

        if ( ((ch=getch()) == ctrlc) && !(p->retlen) ) 
        {
            cookdout(ctrlc);
            warmboot(2);	/*sw From warmboot(1)	*/
        }

        else if ( (ch == cr) || (ch == lf) )
        {                               /* if cr or lf, exit */
            conout(cr);
            break;
        }

        else if (ch == bs) backsp(p, stcol);    /* backspace */

        else if (ch == rub)                     /* delete character */
        {
            if (GBL.echodel)
            {
                if (p->retlen)
                {
                    i = UBWORD(--(p->retlen));
                    conout( p->cbuf[i] );
                }
            }
            else backsp(p, stcol);
        }

        else if (ch == ctrlp) GBL.lstecho = !GBL.lstecho;
                                                /* control-p */
        else if (ch == ctrlx)                   /* control-x */
            do backsp(p,stcol); while (p->retlen);

        else if (ch == ctrle) newline(stcol);   /* control-e */

        else if (ch == ctrlu)                   /* control-u */
        {
            conout('#'); 
            newline(stcol);
            p->retlen = 0;
        }

        else if (ch == ctrlr)                   /* control-r */
        {
            conout('#');
            newline(stcol);
            for (i=0; i < UBWORD(p->retlen); i++)
                    cookdout( p->cbuf[i] );
        }

        else                                    /* normal character */
            cookdout( p->cbuf[UBWORD((p->retlen)++)] = ch );
    }
}
