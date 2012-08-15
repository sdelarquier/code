/*	This routine determines which samples are bad because of transmitter
	pulses	*/
  
#include "radops.h"
#include "fitacf.h"

char badlags_rev[] = {"$Revision$"};

static short int nbad, badsmp[50];
/* ---------------------------------------------------------------------*/

void badlags(raddat)
	struct rawdata *raddat;

{
short int i, k, sample;
long ts, t1, t2;

i = -1;
ts = (long) raddat->PARMS.LAGFR;
sample = 0;
k = 0;
t2 = 0L;
while (i < (raddat->PARMS.MPPUL - 1))
   {
   /* first, skip over any pulses that occur before the first sample */

   while ((ts > t2) && (i < (raddat->PARMS.MPPUL - 1)))
      {
      i++;
      t1 = (long) (raddat->PULSE_PATTERN[i]) * (long) (raddat->PARMS.MPINC)
            - raddat->PARMS.TXPL/2;
      t2 = t1 + 3*raddat->PARMS.TXPL/2 + 100; /* adjust for rx-on delay */
      }   
   /*   we now have a pulse that occurs after the current sample.  Start
      incrementing the sample number until we find a sample that lies
      within the pulse */
   while (ts < t1)
      {
      sample++;
      ts = ts + raddat->PARMS.SMSEP;
      }
   
   /*   ok, we now have a sample which occurs after the pulse starts.
      check to see if it occurs before the pulse ends, and if so, mark
      it as a bad sample */
   while ((ts >= t1) && (ts <= t2))
      {
      badsmp[k] = sample;
      k++;
      sample++;
      ts = ts + raddat->PARMS.SMSEP;
      }
   }
nbad = k;   /* total number of bad samples */

/* set up table for checking range interference */
r_overlap(raddat);
return;
}

/*   This routine uses the table set up by badlags to locate which lags
   are bad for a specified range */
long int ckrng(range, badlag, raddat)
   short int range;
   short int  *badlag;
   struct rawdata *raddat;
{
short int sam1, sam2, i, j;

/*  First, check and make sure badlags has been called to initialize the
	bad sample array -  if it hasn't been called then do it now -
	NOTE:  this has potentially bad consequences if the pulse pattern
	or any of the other parameters (mpinc, lagfr, smsep) have changed.
	Programs that use CKRNG should call BADLAGS after each read operation
	to ensure that the bad sample table is correct */

if (nbad == 0) badlags(raddat);

/*  OK, now compare the samples used for each lag of this ACF with the
	bad sample table */

if (raddat->PARMS.SMSEP == 0) 
{
    return;
}

for (i=0; i<raddat->PARMS.MPLGS; i++)
   {
   badlag[i] = 0;
   sam1 = raddat->LAG_TABLE[0][i]*(raddat->PARMS.MPINC/raddat->PARMS.SMSEP)
         + range - 1;
   sam2 = raddat->LAG_TABLE[1][i]*(raddat->PARMS.MPINC/raddat->PARMS.SMSEP)
         + range - 1;
   for (j=0; j<nbad; j++)
      {
      if ((sam1 == badsmp[j]) || (sam2 == badsmp[j]))
         badlag[i] = 1;
      if (sam2 < badsmp[j]) break;
      }
   }
if (raddat->PARMS.MPLGS == 17) badlag[13] = 1;

/* check additional lag */
lag_overlap(range,badlag,raddat);

return;
}
