/*	this routine uses the raw data index file to locate the requested
	time in a raw data file */

/*
$Log: find_raw_rec.c,v $
 * Revision 1.6  1994/03/07  19:59:55  baker
 * *** empty log message ***
 *
 * Revision 1.5  1994/03/07  18:22:27  baker
 * If there is no raw index file, then in the previous version, you could not
 * find a record that was earlier than the current position in the file.
 * In this version, if the file is currently positioned at a point that is
 * later than the requested time, the file is rewound and the search for
 * the time begins at the beginning of the file.
 *
 * Revision 1.4  1994/03/02  17:53:33  baker
 * removed unused debug code.
 *
 * Revision 1.3  1994/02/28  21:22:57  baker
 * Calls to swab_word and swab_dword must use pointers not values (as
 * was done in the original version).
 *
 * Revision 1.2  1994/02/07  18:19:13  baker
 * fixed up comments,  no change to code.
 *
 * Revision 1.1  1993/09/28  21:18:09  baker
 * Initial revision
 *
*/

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include "radops.h"
#include "rawfile.h"
#include "endian.h"

#define INDEX_REC_LEN 2

char find_raw_rec_rev[] = {"$Revision: 1.6 $"};

extern long int cnv_mdhms_sec();

long find_raw_rec(req_time, time_found, raw_file)
	long req_time, *time_found;
	RAW_FILE *raw_file;

{
long inxrec[INDEX_REC_LEN];
long ltemp;
long t1, t2, t, r1, r2, r;
int p1, p2, p, n1, n2, n;
int status;
short int old_hour;
struct rawdata *raddat;
short int etst;

int i;

 
/*  First check to see if a raw index file exists.  If not, then simply
	read thru the data file until you find the right time */

if (raw_file->inx > 0) {

   if ((p1 = lseek(raw_file->inx, 0, SEEK_SET)) < 0)
	 perror("Can't rewind index file\n");

   if ((status = read(raw_file->inx, inxrec, 
		INDEX_REC_LEN*sizeof(inxrec[0]))) <= 0)
	{
	printf("Error reading first index record\n");
	exit(0);
	}
   else if (endian(&etst) == 0) {
      for (i=0; i<INDEX_REC_LEN; ++i) {
	ltemp = inxrec[i];
	swab_dword(&inxrec[i], &ltemp);
	}
      }

   t1 = inxrec[0];
   r1 = inxrec[1];

   p2 = lseek(raw_file->inx, -8, SEEK_END);
   if (read(raw_file->inx, inxrec, INDEX_REC_LEN*sizeof(inxrec[0])) <= 0)
	{
	printf("Error reading last index record.  p2=%d\n",p2);
	exit(0);
	}
   else if (endian(&etst) == 0) {
      for (i=0; i<INDEX_REC_LEN; ++i) {
	ltemp = inxrec[i];
	swab_dword(&inxrec[i], &ltemp);
	}
      }

   t2= inxrec[0];
   r2 = inxrec[1];

   if (t1 >= req_time)
	{
	*time_found = t1;
	return r1;
	}
   else if (t2 <= req_time)
	{
	*time_found = t2;
	return r2;
	}

   /*  now enter the main loop */

   while (1)
	{
	n1 = p1/8;
	n2 = p2/8;
	n = n1 + (int) (((double) req_time - 
			 (double) t1)*((double)n2 - (double)n1))
		/((double)t2-(double)t1);
	p = n * INDEX_REC_LEN * sizeof(inxrec[0]);
	
	if (n1 == n)
		{
		*time_found = t1;
		 return r1;
		}
	else if (n2 == n)
		{
		*time_found = t2;
		return r2;
		}

	p = lseek(raw_file->inx, p, 0);
	if (read(raw_file->inx, inxrec, INDEX_REC_LEN*sizeof(inxrec[0])) <= 0)
		{
		printf("Error reading index file.  p = %d\n",p);
		exit(0);
		}
	else if (endian(&etst) == 0) {
	      for (i=0; i<INDEX_REC_LEN; ++i) {
		ltemp = inxrec[i];
		swab_dword(&inxrec[i], &ltemp);
		}
	      }

	t = inxrec[0];
	r = inxrec[1];

	if (t < req_time)
		{
		t1 = t;
		p1 = p;
		r1 = r;
		}
	else if (t == req_time)
		{
		*time_found = req_time;
		return r;
		}
	else
		{
		t2 = t;
		p2 = p;
		r2 = r;
		}
	}
   }	/* end of operations when an index file exists */
else {

	/*  We come here if there is no index file */

   if (raw_file == 0) {
	printf("No raw data file is open!\n");
	return 0L;
	}

   raddat = calloc(1, sizeof(struct rawdata));
   status = raw_read(raw_file, 0, raddat);
   
   t = cnv_mdhms_sec(&(raddat->PARMS.YEAR), &(raddat->PARMS.MONTH),
        	&(raddat->PARMS.DAY), &(raddat->PARMS.HOUR), 
		&(raddat->PARMS.MINUT), &(raddat->PARMS.SEC));

   if (t > req_time) {
       status = fseek(raw_file->fp, 0, 0);
       if (status != 0) 
       {
	   printf("Unable to rewind raw data file\n");
	   return 0L;
       }
       t = 0;
       raw_file->raw_offset = 0;
       raw_file->old_recno = 0;
   }
   
   while (t < req_time) {
	status = raw_read(raw_file, 0, raddat);

	t = cnv_mdhms_sec(&(raddat->PARMS.YEAR), &(raddat->PARMS.MONTH),
        	&(raddat->PARMS.DAY), &(raddat->PARMS.HOUR), 
		&(raddat->PARMS.MINUT), &(raddat->PARMS.SEC));
	if (t < 0) return 0L;
	if (raddat->PARMS.HOUR != old_hour) {
	   printf("hour: %d\n",raddat->PARMS.HOUR);
	   old_hour = raddat->PARMS.HOUR;
	   }
	free(raddat);
	}
   *time_found = t;
   return raw_file->raw_offset;
   }
}
