#include <stdio.h>

int main( void )
{
  int i;
  int j;
  for( i = 0; i < 64; i++ )
    for( j = 0; j < 512; j++ )
      putchar( i );
}
