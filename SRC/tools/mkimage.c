#include <stdio.h>

int main( void )
{
  int x;

  printf( "unsigned char bios_diskimage[] = {\n" );
  x = getchar();
  printf( " 0x%x", x );

loop:
  x = getchar();
  if( x == EOF ) goto EndFound;
  printf( ",\n 0x%x", x );
  goto loop;

EndFound:
  printf( "\n};\n" );
  return 0;
}


