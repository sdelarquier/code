
/*	This routine is an easy to use generalized routine for opening
	FIT data files.  If a full path is specified it will try to
	open the file on that path ONLY.  If no path is specified, then
	it will get the environment variable SD_FITROPEN_PATH and 
	use the contents of that variable to form a sequence of paths.
	The routine will attempt to open the files on each path in
	the sequence until it is either successful (in which case
	it returns the pointer to the FIT_FILE structure) or it
	reaches the end of the paths, inwhich case it returns a
	NULL for the pointer */


/*  The calling sequence is:
	FIT_FILE *fitfile, *fitropen();  
	fitfile = fitropen(fname);

	The FIT_FILE data type is defined by a typedef statement in 'fitfile.h'
*/

/*
$Log: fitropen.c,v $
Revision 1.11  1999/07/17 00:00:00  Sessai
fix for the velocity sign error that is present in Syowa East data
between December 27, 1997 and July 16, 1999.

Revision 1.10  1997/04/16 15:11:55  baker
Added support for the routine read_fit386_syowa, which corrects
for the velocity sign error that is present in Syowa data
up until April 24, 1996.

Revision 1.9  1996/03/08 16:04:31  baker
modified this routine to allow the read_fit
routines to work with files that span a year buonoundary.
NOTE:  The header file "fitfile.h" has also been
modified.

 * Revision 1.8  1996/03/07  14:13:48  bristow
 * DELETED TYHE HE %^*&$& BELL
 *
 * Revision 1.7  1995/09/13  15:24:02  baker
 * The previous version of fitropen would cause a problem
 * if you attempted to read the file without first doing
 * a find_fit_rec.  This version corrects that error.
 *
 * Revision 1.6  1995/08/31  17:29:33  baker
 * modified the messages indicating the SuperDARN time period
 * to include the value of CP for common mode and special
 * mode data (discretionary mode already printed out cp).
 *
 * Revision 1.5  1995/08/31  15:58:05  baker
 * This version checks the first data record and tries to
 * identify the type of SuperDARN time (Common, Special,
 * Discretionary).  It outputs a message to the screen
 * indicating the type of time it identified.
 *
 * One additional checange:   All printf statments have
 * been changed to fprintf(stderr, statements.  This
 * allows users to redirect the output of a program
 * without getting these informational messages written
 * to their output file.
 *
 * Revision 1.4  1994/09/06  11:22:19  baker
 * eliminated paths
 *
 * Revision 1.2  93/09/07  15:28:39  15:28:39  baker (Kile Baker S1G)
 * We now check to see if the environment veriable SD_FITROPEN_PATH is
 * defined, before trying to copy it.
 * 
 * Revision 1.1  1993/08/23  13:59:31  baker
 * Initial revision
 *
*/

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

#include "fitfile.h"
#include "radops.h"
#include "endian.h"

char fitropen_rev[] = {"$Revision: 1.11 $"};

#ifndef _MAX_PATH
#define _MAX_PATH 127
#define _MAX_NODE 24
#define _MAX_DIR 127
#define _MAX_FNAME 127
#define _MAX_EXT 127
#endif

#ifndef EOK
#define EOK 0
#endif

#define MAX_COMMON_CP 1000
#define SECS_IN_YEAR 31536000L
#define SECS_IN_DAY 86400L
#define SYOWA 12
#define SYOWAEAST 13

extern int read_fit_dg();
extern int read_fit386_v110();
extern int read_fit386_v130();
extern int read_fit386_syowa();

FIT_FILE *fitropen(char filename[])
{
char fullpath[_MAX_PATH];
char *s, *t;

FIT_FILE *fit_file;

struct PATH_STRUCT {
	char node[_MAX_NODE];
	char dir[_MAX_DIR];
	char fname[_MAX_FNAME];
	char ext[_MAX_EXT];
	} filepath;


union FIT_REC fitdat;
union INX_REC inx_rec;
short int temp, etst;

int iostat, i;
long int ltemp, inx_stime, inx_etime;
struct FIT_DATA *fdata;
off_t fit_start;

char fullname[_MAX_PATH], path_list[256], *cpath;
char path_delimiter[] = ":";	/* this should be a semicolon for VMS  systems */

/* The first step is to decompose the string fname into a path structure */

iostat = _splitpath(filename, filepath.node, filepath.dir, 
			filepath.fname, filepath.ext);
if (iostat == -1) {
	errno = ENOENT;
	perror("fitropen: invalid file name");
	return NULL;
	}

/* now, if the directory is given, copy it to the path_list string, 
	otherwise use getenv to form the path list */

if ((strlen(filepath.dir) == 0) && (getenv("SD_FITROPEN_PATH") != NULL))
        strcpy(path_list, getenv("SD_FITROPEN_PATH"));

else strcpy(path_list, filepath.dir);

/* initialize the string for strtok */

cpath = strtok(path_list, path_delimiter);

/* initialize the FIT_FILE structure */

fit_file = calloc(1, sizeof(FIT_FILE));

/* now try to open the files as long as there is something left in
	the path */

while (cpath != NULL && fit_file->fitdes <= 0 && fit_file->inxdes <= 0) {
	strcpy(filepath.dir, cpath);
	strcpy(filepath.ext, "fit");
	_makepath(fullname, filepath.node, filepath.dir, filepath.fname,
			filepath.ext);
	fit_file->fitdes = open(fullname, O_RDONLY);
	strcpy(fit_file->fname, fullname);
	strcpy(filepath.ext, "inx");
	_makepath(fullpath, filepath.node, filepath.dir, filepath.fname,
			filepath.ext);
	fit_file->inxdes = open(fullpath, O_RDONLY);
	
	/* get next token in the path list */
	cpath = strtok(NULL, path_delimiter);
	}

/*  We have either successfully opend the files or exhausted all the
	paths available.  If we've failed, return a NULL.  If OK, then
	read the first record of the FIT file in order to figure out
	what type of file it is. */

if (fit_file->fitdes <= 0 || fit_file->inxdes <= 0) return NULL;

/* OK, let's find out what type of file it is */

/*	read the first record of the fit file.  Assume it has the structure
	of a FIT_HEADER.  If it is a FIT_HEADER record it will contain
	the name of the creating program, a version number and the date
	and time the file was created.  If it doesn't have this information
	then we have to assume that it is an old Data General file */

iostat = read(fit_file->fitdes, &fitdat, sizeof(union FIT_REC));

if (iostat != sizeof(union FIT_REC)) {
	perror("Error reading header record of FIT file");
	return NULL;
	}

/* check to see if this is a big endian machine */

if (endian(&etst) == 0) {
	temp = fitdat.hdr.fit_len;
	swab_word(&fitdat.hdr.fit_len, &temp);
	temp = fitdat.hdr.inx_len;
	swab_word(&fitdat.hdr.inx_len, &temp);
	}

/* scan for the string "fitacf" if you find it, then scan for the 
	type (QNX, VAX, etc) and the version number.  If "fitacf" is
	not present assume data general file */

s = strstr(fitdat.hdr.info, "fitacf");
if (s == NULL) {
	/* this must be an old DG file */
	strcpy(fit_file->source, "DG");
	fit_file->fit_read = read_fit_dg;
	fit_file->fit_recl = FIT_RECL;
	fit_file->inx_recl = INX_RECL;
	fprintf(stderr, "DG type FIT file identified.\n%s\n",fit_file->fname);
	}
else {
	fprintf(stderr, "RADOPS 386 type FIT file identified\n%s\n",fitdat.hdr.info);
	fit_file->fit_recl = fitdat.hdr.fit_len;
	fit_file->inx_recl = fitdat.hdr.inx_len;

	s = strstr(s, "version");
	if (s == NULL) {
		fprintf(stderr, "Unable to locate version No.: Assume1.3\n");
		strcpy(fit_file->version, "1.30");
		}
	else {
		s = s + strlen("version ");
		t = strchr(s, '(');
		strncpy(fit_file->version, s, (int) (t - s - 1));
		}


	s = strchr(fitdat.hdr.info, '(');	/* locate the source name */
	t = strchr(s, ')');	
	strncpy(fit_file->source, s+1, (int) (t-s-1));
	}

/* OK, we've got a standard fitacf file.  Now determine what subroutine
	is the proper one for reading it.

	if the version number is less than 1.0 it is invalid - output
	an error message and return NULL.

	if the creator is QNX and the version number is 1.1 then use
	read_fit386_v110 otherwise use read_fit386_v130
*/

if (strcmp(fit_file->source, "DG") == 0) strcpy(fit_file->version, "");
else if ((strcmp(fit_file->source, "QNX") == 0) &&
	(strncmp(fit_file->version, "1.3", strlen("1.3")) < 0)) {
	
	fit_file->fit_read = read_fit386_v110;
	}
else fit_file->fit_read = read_fit386_v130;

 
/*	OK, now read the index file and initialize the start time and stop
	time, the starting offset of the data in the index file and its ending
	offset, and set the current time to the start time, and the fit_offset
	to the first data record of the fit file */

iostat = read (fit_file->inxdes, &inx_rec, sizeof(inx_rec));
if (iostat != sizeof(inx_rec)) {
	perror("Unable to read first record of index file\n");
	return NULL;
	}

if (endian(&etst) == 0) 
  for (i=0; i < 4; ++i) {
    ltemp = inx_rec.iv[i];
    swab_dword(&(inx_rec.iv[i]), &ltemp);
  }

fit_file->stime = inx_stime = inx_rec.inx_hdr.stime;
fit_file->etime = inx_etime = inx_rec.inx_hdr.etime;

fit_file->ctime = inx_rec.inx_hdr.stime;
fit_file->inx_srec = sizeof(inx_rec)*(inx_rec.inx_hdr.srec - 1);
fit_file->inx_erec = sizeof(inx_rec)*(inx_rec.inx_hdr.erec - 1);

lseek (fit_file->inxdes, fit_file->inx_srec, SEEK_SET);

iostat = read (fit_file->inxdes, &inx_rec, sizeof(inx_rec));
if (iostat != sizeof(inx_rec)) {
	perror("fitropen: Unable to read first data record of index file\n");
	return NULL;
	}

if (endian(&etst) == 0)
  for (i=0; i < 4; ++i) {
    ltemp = inx_rec.iv[i];
    swab_dword(&(inx_rec.iv[i]), &ltemp);
  }

fit_file->fit_offset = lseek(fit_file->fitdes, 
		fit_file->fit_recl * (inx_rec.index.recno - 1), SEEK_SET);
fit_file->inx_offset = lseek(fit_file->inxdes, fit_file->inx_srec, SEEK_SET);

/* check the result of lseek to make sure the seek worked */
if ((fit_file->fit_offset != fit_file->fit_recl*(inx_rec.index.recno - 1)) ||
    (fit_file->inx_offset != fit_file->inx_srec)) 
{
    perror("fitropen: line: __LINE__.  lseek failed");
    fprintf(stderr,"fit_offset = %d, desired value = %d\n",
	    fit_file->fit_offset, fit_file->fit_recl*(inx_rec.index.recno -1));
    fprintf(stderr,"inx_offset = %d, desired value = %d\n",
	    fit_file->inx_offset, fit_file->inx_srec);
    return NULL;
}

fit_start = fit_file->fit_offset;

/* Now we have to read the first data record in the file to see if
this data is common time, special time or discretionary time */

fdata = (struct FIT_DATA *) calloc(1, sizeof(struct FIT_DATA));
if (fdata == 0) 
{
    perror ("fitropen: Unable to allocate memory for FIT_DATA");
    exit(-1);
}
iostat = read_fit(fit_file, fdata);
if (iostat != EOK) 
{
    perror("fitropen: Unable to read fit file");
    exit (-1);
}

if (fdata->p.CP == 0) 
    fprintf(stderr, "SuperDARN CP is undefined\n");
else if (fdata->p.CP >0 && fdata->p.CP < MAX_COMMON_CP) 
    fprintf(stderr, "SuperDARN Common Time data. CP = %hd\n",fdata->p.CP);
else if (fdata->p.CP < 0)
    fprintf(stderr, "\nWARNING:  \nThis data file contains Discretionary Time Data.  CP = %hd\n\n",fdata->p.CP);
else 
    fprintf(stderr, "\nSuperDARN SPECIAL Time data. CP = %hd\n",fdata->p.CP);

/* OK, we have now informed the user about the type of SuperDARN time
that this data file contains.  Now we have to reset all the values in
the fit_file structure and rewind the file */

fit_file->stime = inx_stime;
fit_file->etime = inx_etime;
fit_file->ctime = inx_stime;

/* now check the end time to see if it is less than the start
   time.  If it is, we assume that the file crosses a year
   boundary. */

if (fit_file->etime < fit_file->stime) 
{
    fprintf(stderr,"stime=%d,etime=%d\n",fit_file->stime,
	    fit_file->etime);
    fprintf(stderr,"Warning:  end time < start time.\n"
	"Assumming file spans year boundary\n");

    if ((fdata->p.YEAR % 4) == 0) 
	fit_file->inx_yr_offset = SECS_IN_YEAR + SECS_IN_DAY;
    else fit_file->inx_yr_offset = SECS_IN_YEAR;
}


fit_file->fit_offset = lseek(fit_file->fitdes, fit_start, SEEK_SET);
fit_file->inx_offset = lseek(fit_file->inxdes, fit_file->inx_srec, SEEK_SET);

/* check the result of lseek to make sure the seek worked */
if ((fit_file->fit_offset != fit_file->fit_recl*(inx_rec.index.recno - 1)) ||
    (fit_file->inx_offset != fit_file->inx_srec) ) 
{
    perror("fitropen: line: __LINE__.  lseek failed");
    fprintf(stderr,"fit_offset = %d, desired value = %d\n",
	    fit_file->fit_offset, fit_start);
    fprintf(stderr,"inx_offset = %d, desired value = %d\n",
	    fit_file->inx_offset, fit_file->inx_srec);
    return NULL;
}

/* the last step is to see if this is actually a Syowa radar file.
   If it is, then we have to check the date and time to see if
   this data is from the period when the velocity on the Syowa data 
   was reversed in sign.

   The period of sign reversed data is any time prior to 11 UT, April 24, 1996

   */

if (fdata->p.ST_ID == SYOWA) 
{
  
  if ((fdata->p.YEAR < 1996) || 
      ((fdata->p.YEAR == 1996) && (inx_stime < 9889200))) {
    
    fit_file->fit_read = read_fit386_syowa;
	/* added by Sessai just for checking... */
	printf("Special read_fit386_syowa (Vel Reverse!!!) selected...\n");
  }
	/* added by Sessai just for checking... */
  else printf("normal read_fit386_v130 (No Vel Reversal..) selected...\n");
}


/* one more step! is to see if this is actually a Syowa East radar file.
   If it is, then we have to check the date and time to see if
   this data is from the period when the velocity on the Syowa East data 
   was reversed in sign.

   The period of sign reversed data is
	from 08:17:00 UT, December 27, 1997
		(1997/12/27 (day 360) 08:17:00 UT (year=1997, ysec=31133820))
	to   11:18:28 UT, July     16, 1999
		(1999/07/16 (day 196) 11:18:28 UT (year=1999, ysec=16975108))
   */

if (fdata->p.ST_ID == SYOWAEAST) 
{
  
  if ((fdata->p.YEAR == 1998) || 
      ((fdata->p.YEAR == 1997) && (inx_stime > 31133820)) ||
      ((fdata->p.YEAR == 1999) && (inx_stime < 16975108))) {
    
    fit_file->fit_read = read_fit386_syowa;
	/* added by Sessai just for checking... */
	printf("Special read_fit386_syowa (Vel Reverse!!!) selected...\n");
  }
	/* added by Sessai just for checking... */
  else printf("normal read_fit386_v130 (No Vel Reversal..) selected...\n");
}



free(fdata);


/* OK, we're all done.  Return the pointer to the fit_file structure */

return fit_file;
}


