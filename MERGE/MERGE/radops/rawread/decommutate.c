/*	This routine is used to pull out the compressed lag-0 power, ACF,
	and XCF data from the raw data file and fill in the raw data structure
	(raddat) that is then used by proc_rec. */
/*
$Log: decommutate.c,v $
 * Revision 1.2  1994/03/02  17:45:00  baker
 * removed unused variable declaration.
 *
 * Revision 1.1  1993/10/08  20:23:17  baker
 * Initial revision
 *
*/

#include <stdio.h>
#include <stdlib.h>

#include "radops.h"

extern void depack();

char decommutate_rev[] = {"$Revision: 1.2 $"};

void decommutate(idata, inlen, raddat, raw_version)
	short int *idata;
	short int inlen;
	struct rawdata *raddat;
	double raw_version;

{
short int dlen, i, i_old;
short int *inptr;
static short int recnum=0;

if ((raw_version < 2.10) && (raddat->PARMS.MPLGS != 17))
	{
	printf("Rawwrite Version %4.2f is invalid for this data\n",raw_version);
	exit(0);
	}

inptr = idata;
++recnum;

/*	First decompress the lag-0 power data */

dlen = raddat->PARMS.NRANG;
depack(inptr, &(raddat->pwr0[0]), &dlen);
inptr += dlen;
inlen -= dlen;

/* now decompress the ACF and XCF data */
/* just to be safe, first clear the data block */

memset(&(raddat->acfd[0][0][0]), 0, (size_t)(4*sizeof(raddat->acfd[0][0][0])*
				LAG_TAB_LEN*MAX_RANGE));

dlen = 2*raddat->PARMS.MPLGS;
i_old = -1;

i = -1;
while (inlen > 0)
	{
	if ((raw_version > 0) && (raw_version < 2.0)) ++i;
	else
		{
		i = *inptr -1;		/* get the range number */
		if (i < 0 || i >= raddat->PARMS.NRANG) {
		   printf("Invalid Range Number: %d\n",i);
		   printf("Time = %d %d %d %d %d %d\n",
		      raddat->PARMS.YEAR, raddat->PARMS.MONTH,
		      raddat->PARMS.DAY, raddat->PARMS.HOUR,
		      raddat->PARMS.MINUT, raddat->PARMS.SEC);
		   printf("Record number = %d\n",recnum);
		   exit(-1);
		   }
		++inptr;
		--inlen;
		}

	if (i > i_old)		/* determine if acf or xcf */
		{	/* range # increasing, so must still be ACFs */
		depack(inptr, &(raddat->acfd[i][0][0]), &dlen);
		i_old = i;
		}
	else
		{	/* range # went down - therefore its an XCF */
		depack(inptr, &(raddat->xcfd[i][0][0]), &dlen);
		i_old = MAX_RANGE;
		}
	inptr += dlen;
	inlen -= dlen;
	}
return;
}
