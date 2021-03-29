#include "basepage.h"

void entry( VAX_BasePage_t *BasePage );
void argh( VAX_BasePage_t *BasePage )
{
  entry( BasePage );
}

void entry( VAX_BasePage_t *BasePage )
{
  BasePage->Bdos( 9, (unsigned long int)"Hello world!$" );
}
