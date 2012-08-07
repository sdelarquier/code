/*	This file provides two routines that are used when reading the
	old DG raw data files.  

	dg_pulse_pat fills in the PULSE_PATTERN table
	dg_lag_table fills in the LAG_TABLE

*/

int dg_pulse_ver = 1;

#include "radops.h"

void dg_pulse_pat(np)
	struct rawdata *np;
{
np->PULSE_PATTERN[0] = 0;
np->PULSE_PATTERN[1] = 2;
np->PULSE_PATTERN[2] = 3;
np->PULSE_PATTERN[3] = 7;
np->PULSE_PATTERN[4] = 13;
np->PULSE_PATTERN[5] = 15;
np->PULSE_PATTERN[6] = 16;
return;
}

void dg_lag_table(np)
	struct rawdata *np;
{
int i;
static short int dg_lags[2][17] = 
	{{7, 2, 0, 0, 3, 2, 7, 0, 7, 7, 3, 2, 3, 2, 2, 0, 0},
	 {7, 3, 2, 3, 7, 7, 13,7,15,16,13,13,15,15,16,15,16}};

for (i=0; i<17; ++i)
	{
	np->LAG_TABLE[0][i] = dg_lags[0][i];
	np->LAG_TABLE[1][i] = dg_lags[1][i];
	}
return;
}

