/*
$Log: cnv_sec_mdhms.c,v $
 * Revision 1.4  1995/04/21  21:24:49  baker
 * Nick Mattin discovered a bug with this routine.  After March 1 of
 * non-leap years, it would be off by one day.
 *
 * The changes for this version correct that bug, by completely
 * replacing the algorithm for finding the month with an alternative
 * algorithm.  The new algorithm is the same as the one that is
 * used in the IDL version of this routine.
 *
 * Additional changes cleaned up some of the C code.
 *
 * Revision 1.3  1995/01/30  15:51:50  bristow
 * *** empty log message ***
 *
 * Revision 1.2  1995/01/25  19:43:20  bristow
 * Some changes to make Kile happy
 *
*/

#include <math.h>
 
cnv_sec_mdhms(iyr,mo,dy,hr,min,sec,seconds)
long *seconds;
short *iyr,*mo,*dy,*hr,*min,*sec;
{
const long spm=60,mph=60,hpd=24;
long dmin,dhr,ddy;
short days,hours,tdys;
const myear=4;
short modys[12]= {31,28,31,30,31,30,31,31,30,31,30,31};
short register i;

/* check to see if this is a leap year */
if( *iyr % myear  == 0 ) modys[1]=29;

*sec=(short)(*seconds % spm );

dmin = (*seconds - (long)*sec)/60;
*min=(short) (dmin % mph);

dhr = (dmin-(long)*min)/60;
*hr=(short) (dhr % hpd);

ddy = (dhr-(long)*hr)/24;

i = 0;
while (ddy > 0 && i < 12) 
{
    ddy = ddy - modys[i];
    ++i;
}

if (i > 0) 
{
    if (ddy == 0) 
    {
	*mo = i+1;
	*dy = 1;
    }
    else 
    {
	*mo = i;
	*dy = ddy + modys[i-1] + 1;
    }
}
else {
    *mo = 1;
    *dy = ddy + 1;
}   
}

