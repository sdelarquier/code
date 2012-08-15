/*	This function is used to read the raw data from an old Data General
	raw data file */

/*
$Log: read_raw_dg.c,v $
 * Revision 1.5  1994/03/02  21:05:06  baker
 * fixed bug in handling the junk data that appears on disk files containing
 * compressed DG data.
 *
 * Revision 1.4  1994/03/02  17:38:52  baker
 * added function prototypes.
 *
 * Revision 1.3  1994/02/28  21:27:16  baker
 * the previous version neglected to include "endian.h" resulting in
 * incorrect definitions of swab_word, etc.
 *
 * Revision 1.2  1994/02/07  17:58:26  baker
 * Changed calls to 'swab' to 'sd_swab'.  This avoids a conflict with
 * the name swab in IDL.
 *
 * Revision 1.1  1993/09/29  20:53:44  baker
 * Initial revision
 *
*/

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include "radops.h"
#include "rawfile.h"
#include "endian.h"

extern void oldparms_newparms(short int [], struct radops_parms *);
extern void dg_pulse_pat(struct rawdata *);
extern void dg_lag_table(struct rawdata *);


char read_raw_dg_rev[] = {"$Revision: 1.5 $"};

size_t read_raw_dg(indata, raddat, raw_file)
	short int *indata;
	struct rawdata *raddat;
	RAW_FILE *raw_file;
{
FILE *fp;
short int pbuf[30];
int i, j;
short int slist[50];
short int old_acf[2][17];
short int *ibuf;
short int nbytes;
size_t iostat;
short int etst;

struct { short int junk, c_flag, num_bytes;} cmphead;

fp = raw_file->fp;

if (raw_file->dg_flag == DG_COMPRESSED) {
	iostat = fread(&cmphead,sizeof(cmphead),1,fp);
	if (iostat != 1) {
	   printf("Error reading compressed file header\n");
	   return EOF;
	   }
	iostat = fread(&(cmphead.junk),sizeof(cmphead.junk),1,fp);
	if (iostat != 1) {
	   printf("Error reading first word of data record following cmp header\n");
	   return EOF;
	   }
	if (endian(&etst) != 0)
	   sd_swab(&cmphead.junk, &cmphead.junk, sizeof(cmphead));
	
	
/*	if (cmphead.junk != 3) printf("junk word = %hx\n",cmphead.junk); */

	}

if ((iostat = fread(pbuf, sizeof(short int), 30, fp)) != 30)
	{
	printf("Error Reading parameter list in DG file\n");
	return EOF;
	}
nbytes = 30;

/* if this is a little endian machine, we have to swap bytes */

if (endian(&etst) != 0) sd_swab(pbuf, pbuf, nbytes);

oldparms_newparms(pbuf,&(raddat->PARMS));
if (raddat->PARMS.NPARM != 70) 
	{
	printf("Got lost reading a DG data file\n");
	/*  try to find a starting point */
	while (raddat->PARMS.NPARM != 70) {
	  if ((iostat = fread(pbuf, sizeof(short int), 1, fp)) != 1) {
	    printf("End of file?\n");
	    exit(0);
	    }
	  nbytes = 2;
	  if (endian(&etst) != 0) sd_swab(pbuf, pbuf, nbytes);
	  if (pbuf[0] == 70) {
	    printf("Found a value of 70.  Maybe at beginning of record\n");
	    if ((iostat = fread(pbuf + 1, sizeof(short int), 29, fp)) != 29) {
	      printf("Couldn't continue reading\n");
	      exit(0);
	      }
 	    else {
	      nbytes = 29;
	      if (endian(&etst) != 0) sd_swab(pbuf+1, pbuf+1, nbytes);
	      oldparms_newparms(pbuf, &(raddat->PARMS));
	      if (raddat->PARMS.MPLGS != 17) {
	        printf("Nope, MPLGS isn't right\n");
	        raddat->PARMS.NPARM = 0;
	        }
	      }
            }
	  }
	}

if ((iostat = fread(&(raddat->COMBF[0]), sizeof(char), 80, fp)) != 80)
	{
	printf("Error reading comment buffer in DG file\n");
	return EOF;
	}


dg_pulse_pat(raddat);	/*  store the original pulse pattern in raddat */
dg_lag_table(raddat);	/* store the original lag table */

/*  If this is a compressed file and this record contains only the parameters
	then pad things out with sufficient 0s */

if ((raw_file->dg_flag == DG_COMPRESSED) && (raddat->COMBF[78] != 0)) {
	memset(indata, 0, sizeof(short int)*50);
			/* pad the lag-0 data with 0 */
	memset(slist, 0, sizeof(short int)*50);
	cmphead.c_flag = 1;
	}
	
else if ((iostat = fread(indata, sizeof(short int), raddat->PARMS.NRANG, fp))
			 <= 0)
	{
	printf("Error reading lag0 powers in DG file\n");
	return EOF;
	}
else cmphead.c_flag = 0;

/* if this is a little endian machine we have to swap bytes */
if (endian(&etst) != 0) sd_swab(indata, indata, (raddat->PARMS.NRANG));

iostat = raddat->PARMS.NRANG;
if (!cmphead.c_flag)
   iostat = fread(slist, sizeof(short int), raddat->PARMS.NRANG, fp);
if (iostat <= 0)
	{
	printf("Error reading selection list in DG file\n");
	return EOF;
	}
if (endian(&etst) != 0) sd_swab(slist, slist, (raddat->PARMS.NRANG));

ibuf = indata + raddat->PARMS.NRANG;

i = 0;
while ((i < 50) && (slist[i] > 0))
	{
	*ibuf++ = slist[i];
	if (!cmphead.c_flag) 
	   iostat = fread(old_acf, sizeof(short int),2*raddat->PARMS.MPLGS,fp);
	else {
	   memset(old_acf,0,2*raddat->PARMS.MPLGS);  /* PAD missing data */
	   iostat = 2*raddat->PARMS.MPLGS;
	   }
	if(iostat <= 0)
		{
		printf("Error reading ACF data from DG file\n");
		return EOF;
		}
	nbytes = 2*raddat->PARMS.MPLGS;
	if (endian(&etst) != 0) sd_swab(old_acf, old_acf, nbytes);
	for (j=0; j<raddat->PARMS.MPLGS; ++j)
		{
		*ibuf++ = old_acf[0][j];
		*ibuf++ = old_acf[1][j];
		}
	++i;
	}
i = 0;
while ((i < 50) && (slist[i] > 0) && (raddat->PARMS.XCF))
	{
	*ibuf++ = slist[i];
	if (!cmphead.c_flag) 
	   iostat = fread(old_acf, sizeof(short int),2*raddat->PARMS.MPLGS,fp);
	else {
	   memset(old_acf,0,2*raddat->PARMS.MPLGS);  /* PAD missing data */
	   iostat = 2*raddat->PARMS.MPLGS;
	   }
	if (iostat <= 0)
		{
		printf("Error reading ACF data from DG file\n");
		return EOF;
		}
	nbytes = 2*raddat->PARMS.MPLGS;
	if (endian(&etst) != 0) sd_swab(old_acf, old_acf, nbytes);
	for (j=0; j<raddat->PARMS.MPLGS; ++j)
		{
		*ibuf++ = old_acf[0][j];
		*ibuf++ = old_acf[1][j];
		}
	++i;
	}
return ibuf - indata;
}
