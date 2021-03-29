#include <stdio.h>

int main( void )
{
  int x;

  printf( "\t.text\n\t.globl image\nimage:\n" );
  x = getchar();
  printf( "\t.byte 0x%x\n", x );

loop:
  x = getchar();
  if( x == EOF ) goto EndFound;
  printf( "\t.byte 0x%x\n", x );
  goto loop;

EndFound:
  printf( "\t.end\n" );
  return 0;
}
