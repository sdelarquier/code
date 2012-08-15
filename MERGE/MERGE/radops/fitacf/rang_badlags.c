/* this file computes the additional bad lags due to range interference
*/

/*
$Log$
*/

#include <stdio.h>
#include <math.h>
#include "radops.h"

#define FALSE 0
#define TRUE  1
#define MIN_PWR_RATIO	.3

static short int range_overlap[PULSE_PAT_LEN][PULSE_PAT_LEN];

/* r_overlap sets up the table r_overlap which keeps track of the
   ranges which might cause interference
*/ 

char rang_badlags_rev[] = {"$Revision$"};

long int r_overlap(raw_data)
struct rawdata *raw_data;
{
   int ck_pulse;
   int pulse;
   int tau;
   int mppul;
   int diff_pulse;

   /* define constants */
   tau = raw_data->PARMS.MPINC / raw_data->PARMS.SMSEP;
   mppul = raw_data->PARMS.MPPUL;

   for(ck_pulse = 0; ck_pulse < mppul; ++ck_pulse) {
      for(pulse = 0; pulse < mppul; ++pulse) {
         diff_pulse = raw_data->PULSE_PATTERN[ck_pulse] - 
                      raw_data->PULSE_PATTERN[pulse];
         range_overlap[ck_pulse][pulse] = diff_pulse * tau;
      }
   }

   return;

} /* r_overlap() */


/* lag_overlap marks the badlag array for bad lags
*/

long int lag_overlap(range,badlag,raw_data)
short int range;
short int badlag[];
struct rawdata *raw_data;
{
     
   int ck_pulse;
   int pulse;
   int lag;
   int mppul;
   short int ck_range;
   int nrang;
   long min_pwr;
   long pwr_ratio;
   int bad_pulse[PULSE_PAT_LEN];  /* true if there is a bad pulse */
   int i;
   double nave;
   
   --range;  /* compensate for the index which starts from 0 instead of 1 */

   mppul = raw_data->PARMS.MPPUL;
   nrang = raw_data->PARMS.NRANG;
   nave = (float) (raw_data->PARMS.NAVE);

   for(pulse = 0; pulse < mppul; ++pulse)
      bad_pulse[pulse] = FALSE;

   for(ck_pulse = 0;  ck_pulse < mppul; ++ck_pulse) {
      for(pulse = 0; pulse < mppul; ++pulse) {
         ck_range = range_overlap[ck_pulse][pulse] + range;
         if((pulse != ck_pulse) && (0 <= ck_range) && (ck_range < nrang)) {
            pwr_ratio = (long) (nave * MIN_PWR_RATIO);
            min_pwr =  pwr_ratio * raw_data->pwr0[range];
            if(min_pwr < raw_data->pwr0[ck_range])
               bad_pulse[ck_pulse] = TRUE;
         }
      } /* for(pulse */
   } /* for(ck_pulse */            


   /* mark the bad lag */
   for(pulse = 0 ; pulse < mppul; ++pulse) {
      if(bad_pulse[pulse] == TRUE) {
         for(i=0; i < 2 ; ++i) {
            for(lag = 0 ; lag < raw_data->PARMS.MPLGS ; ++lag) {
               if(raw_data->LAG_TABLE[i][lag] == 
                                     raw_data->PULSE_PATTERN[pulse])
                  badlag[lag] = 1;  /* 1 for bad lag */
            }
         }
      }
   } /* for pulse */

   return;

} /* lag_overlap() */

