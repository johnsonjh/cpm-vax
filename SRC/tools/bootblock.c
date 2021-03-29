/* program to generate a boot block to describe the specified files. The
 * boot block assumes the files will be concatenated, loaded from block
 * 1, and entered at the first byte of the image.
 */

#include <stdio.h>
#include <sys/stat.h>
#include <stdlib.h>

unsigned char BootBlock[ 512 ] = {

  /* Boot block header.
   *
   * This information is used to locate the image descriptor within
   * the boot block.
   */

  0x00, 0x00,		/* don't care */ /* 0 */
  0x04,			/* word offset into boot block of descriptor */
  0x01,			/* must be one */

  0x00, 0x00,		/* MS word of LBN of start of image */ /* 4 */
  0x01, 0x00,		/* LS word of LBN of start of image */

  /* Image descriptor.
   *
   * This information describes the image.
   */

  0x18,			/* instruction set. 0x18 = VAX */ /* 8 */
  0x00,			/* expected controller. 0x00 = unknown */
  0x00,			/* type of filesystem on medium. */
  0xff - 0x18,		/* one's complement of some of preceding three */

  0x00,			/* must be zero */ /* 12 */
  0x81,			/* 0x80 = two sides + 0x01 = version of standard */
  0x00,	0x00,		/* don't care */

  0x00, 0x00, 0x00, 0x00, /* Size, in blocks, of image. */ /* 16 */
  0x00, 0x00, 0x00, 0x00, /* Offset from default load address */ /* 20 */
  0x00, 0x00, 0x00, 0x00, /* image entry point */ /* 24 */
  0x00, 0x00, 0x00, 0x00  /* Sum of previous three longwords */ /* 28 */
};

int main( int ArgC, char **ArgV )
{
  int NextArg;
  struct stat StatBuffer;
  unsigned long long int TotalSize = 0;

  if( ArgC < 2 ) {
    fprintf( stderr, "usage: bootblock binary-file ...\n" );
    exit( EXIT_FAILURE );
  }

  for( NextArg = 1; NextArg < ArgC; NextArg++ ) {
    if( stat( ArgV[ NextArg ], &StatBuffer ) != 0 ) {
      perror( ArgV[ NextArg ] );
      exit( EXIT_FAILURE );
    }
    TotalSize += (unsigned long long int)StatBuffer.st_size;
  }

  fprintf( stderr, "total size: %lld (0x%llx)\n",
    TotalSize, TotalSize );

  /* Fill in the image size.
   */

  BootBlock[ 20 ] = TotalSize;
  BootBlock[ 21 ] = TotalSize >> 8;
  BootBlock[ 22 ] = TotalSize >> 16;
  BootBlock[ 23 ] = TotalSize >> 24;

  /* And the sum of the last three longwords.
   */

  BootBlock[ 28 ] = TotalSize;
  BootBlock[ 29 ] = TotalSize >> 8;
  BootBlock[ 30 ] = TotalSize >> 16;
  BootBlock[ 31 ] = TotalSize >> 24;

  /* Dump out the boot block.
   */

  for( NextArg = 0; NextArg < sizeof( BootBlock ); NextArg++ ) {
    putchar( BootBlock[ NextArg ] );
  }

}
