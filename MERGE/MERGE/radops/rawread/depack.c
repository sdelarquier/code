/*This routine decompresses the RADOPS 16-bit compressed data and converts
  it back into 32-bit integers.  The calling sequence is:

  depack( short int *inptr, long int *outpt, short int *len)

where inptr points to an array of compressed 16-bit values,
outpt points to an array of uncompressed 32-bit values,
and len points to an integer giving the number of values to be converted.

*/
/*
$Log: depack.c,v $
 * Revision 1.4  1994/03/02  17:46:28  baker
 * removed all references to "endiahn"n", which is now done in decommutate.
 *
 * Revision 1.3  1994/02/28  21:13:31  baker
 * swappping bytes is now done in decommutate rather than depack.
 *
 * Revision 1.2  1994/02/07  18:10:20  baker
 * added initial byte swapping so that this will work on big endian
 * machines.
 *
 * Revision 1.1  1993/10/08  20:53:45  baker
 * Initial revision
 *
*/

char depack_rev[] = {"$Revision: 1.4 $"};

#define SHIFT_MASK 0xf
#define SIGN_BIT 0x8000

void depack(unsigned short int *inptr, long int *outpt, short int *len)
{
  int sc;   /* shift count */
  int sign;
  long int mantissa;
  unsigned short int sval;
  int i;

/* OK, now do the decompression for each value */

  for (i=0; i<= *len; ++i) {
    sval = *inptr;
    sc = sval & SHIFT_MASK;  /* get the shift count */
    sign = sval & SIGN_BIT;  /* get the sign bit */
    mantissa = (unsigned long) (sval & ~(SHIFT_MASK | SIGN_BIT)); /* get mantissa */

    if (sc == 0) {
      mantissa = mantissa << 1;  /* left shift once if shift count is 0 */
    }
    else {
      mantissa = 0x8000 | mantissa;  /* add the unseen high order bit */
      mantissa = mantissa << sc;     /* shift left the required number of times */
    }
    /* if sign is set, make the value negative */
    if (sign != 0) mantissa = -mantissa;  
    *outpt = mantissa;
    ++outpt;
    ++inptr;
  }
  return;
}

      
