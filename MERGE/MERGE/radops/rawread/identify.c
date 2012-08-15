/*	This routine reads the first data record of the raw data file
	and returns the starting time and the station identifier */
/*
$Log: identify.c,v $
 * Revision 1.2  1995/03/22  19:37:20  baker
 * switched to using the standard header file sd_radar_name.h to
 * provide the information about the radar name and id number.
 *
 * Revision 1.1  1995/03/22  18:12:22  baker
 * Initial revision
 *
*/

#include <stdio.h>
#include "radops.h"
#include "fitacf.h"
#include "sd_radar_name.h"
#include "rawfile.h"
#include <malloc.h>

extern size_t raw_read();

char identify_rev[] = {"$Revision: 1.2 $"};

void identify(raw_file, yr, mo, day, hr, minut, sec, st_id, st_name, st_code)
	RAW_FILE *raw_file;
	short int *yr, *mo, *day, *hr, *minut, *sec, *st_id;
	char *st_name, *st_code;

{
struct rawdata *raddat;
long iostat;
raddat = (struct rawdata *) calloc(1, sizeof(struct rawdata));

if ((iostat = raw_read(raw_file, 0, raddat)) == EOF)
	{
	printf("Unable to read raw data file\n");
	exit(0);
	}

*yr = raddat->PARMS.YEAR;
*mo = raddat->PARMS.MONTH;
*day = raddat->PARMS.DAY;
*hr = raddat->PARMS.HOUR;
*minut = raddat->PARMS.MINUT;
*sec = raddat->PARMS.SEC;
*st_id = raddat->PARMS.ST_ID;

strcpy(st_name,sd_radar_name[*st_id].name);
*st_code = sd_radar_name[*st_id].l_code;

raw_read(raw_file, -1, raddat);	/* rewind the raw data file */
free(raddat);
return;
}	





