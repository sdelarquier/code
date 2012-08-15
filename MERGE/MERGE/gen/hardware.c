/*	This is routine determines the appropriate hardware parameters for 
	a particular radar at a particular time.  It reads the file pointed
	to by the environment variable, SD_HARDWARE.  It scans through the
	file searching for the entry appropriate for the selected radar
	and the selected time.  It returns the values go the structure
	passed to it in the argument list.
	
	The calling sequence is:
	
	int hardware_setup( year, yr_sec, station, hd)
	
	where year is the year in 4 digit form (e.g. 1993),
	yr_sec is the time in seconds from the beginning of the year,
	station is the radar's station id number,
	hd is a pointer to a structure of type SD_HARDWARE, which
		is defined in radops.h

	the return value of the function is 0 if a hardware setup 
	was found successfully.
	
*/

/*
$Log:	hardware.c,v $
 * Rev 2.4 20020111 DA; changed SD_HARDWARE to SD_HARDWARE_DAT
   to remove a conflict with rst
 * Revision 2.3  94/08/03  18:04:51  18:04:51  baker (Kile Baker S1G)
 * This is the initial revision on the UNIX systems.
 * 
 * Revision 2.3  1993/07/28  21:24:24  kile
 * added receiver rise time to the data contained in the hardware data file
 *
 * Revision 2.2  1993/07/28  16:36:43  kile
 * the routine now closes the hardware data file before it returns
 * to the calling routine.
 *
 * Revision 2.1  1993/07/28  16:24:37  kile
 * initial revision
 *
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include "radops.h"

char hardware_rev[] = {"$Revision: 2.3 $"};

int hardware_setup( int year, int yr_sec, int station, struct SD_HARDWARE *hd)

{
int ssyr;
long ctime;
FILE *fp;
int st_indx;
char inbuf[256], *line;


if (year < 1950) year = year + 1900;

fp = fopen(getenv("SD_HARDWARE_DAT"), "r");
if (fp == 0) {
	ssyr = errno;
	perror("Unable to open Hardware Definition File\n");
	return ssyr;
	}

ssyr = 0;
ctime = 0L;
st_indx = 0;

while ((ssyr < year) || (st_indx != station) || 
	((ssyr == year) && (ctime < yr_sec))) {
	
	line = fgets( inbuf, sizeof(inbuf), fp);
	if (line == NULL) {
		perror("I/O error or EOF in Hardware Def. file\n");
		fclose(fp);
		return (EIO);
		}
	if (line[0] == '#') continue;	/* ignore comment lines */
	
	sscanf(line, "%d%d%ld%lf%lf%lf%lf%lf%lf%lf%lf%lf%lf%lf%lf%lf",
		&st_indx, &ssyr, &ctime, &hd->geo_lat, &hd->geo_long,
		&hd->altitude, &hd->boresite, &hd->beam_sep,
		&hd->vdir, &hd->atten, &hd->tdiff, &hd->phidiff,
		&hd->interfer_pos[0], &hd->interfer_pos[1], &hd->interfer_pos[2],
		&hd->rec_rise);
	}
fclose(fp);
return 0;
}

