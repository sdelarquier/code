/*	This routine is used to convert the old DG parameter data to the
	new RADOPS 386 format */
#include <math.h>
#include "radops.h"

char oldparms_newparms_ver[] = {"$Revision: 1.1 $"};

oldparms_newparms(op, np)
	short int op[];
	struct radops_parms *np;

{
np->NPARM = op[0];
np->ST_ID = (op[11] & 0xFF00) >> 8;
np->YEAR = op[1];
np->MONTH = op[2];
np->DAY = op[3];
np->HOUR = op[4];
np->MINUT = op[5];
np->SEC = op[6];
np->TXPOW = op[11] & 0x00FF;
np->NAVE = op[8];
np->ATTEN = op[21];
np->LAGFR = op[17];
np->SMSEP = op[18];
np->ERCOD = op[25];
np->NOISE = (long) op[26];
np->INTT = op[7];
np->TXPL = op[12];
np->MPINC = op[13];
np->MPPUL = op[14];
np->MPLGS = op[15];
np->NRANG = op[16];
np->FRANG = ((np->LAGFR)*3)/20;
np->RSEP = ((np->SMSEP)*3)/20;
np->BMNUM = op[19];
np->XCF = op[28];
np->TFREQ = op[9]*1000L + op[10];
np->MXPWR = ((long) op[20]) * 65536;
np->LVMAX = op[24];

/* adjust the noise level when we have attenuation applied.  This is required
	because the DG radops always saves the noise at 0 attenuation, while
	RADOPS 386 saves the noise as it would be observed with the 
	attenuation that was used during the integration */

if ((np->ATTEN != 0) && (np->NOISE > 0))
	np->NOISE = (long)(((double) np->NOISE)/(pow(10.0,(double)np->ATTEN)));

/*  For data that predates the Halley radar, we need to define the station
	id properly [assume it is Goose Bay] */

if (((np->YEAR % 1900) < 88) && (np->ST_ID != 1) && (np->ST_ID != 2)) 
	np->ST_ID = 1;	
return;
}

