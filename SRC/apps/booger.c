unsigned short int bdos( unsigned short int func, unsigned long int parm);

void Hello( void )
{
  bdos( 9, "hello world!" );
}
