/* SD_BEGIN> */
/*	This routine is an easy to use generalized routine for opening
	raw (DAT) radar data files.  If a full path is specified it will try to
	open the file on that path ONLY.  If no path is specified, then
	it will get the environment variable SD_RAWROPEN_PATH and 
	use the contents of that variable to form a sequence of paths.
	The routine will attempt to open the files on each path in
	the sequence until it is either successful (in which case
	it returns the pointer to the RAW_FILE structure) or it
	reaches the end of the paths, in which case it returns a
	NULL for the pointer */


/*  The calling sequence is:
	RAW_FILE *rawfile, *rawropen();  
	rawfile = rawropen(fname);

	The RAW_FILE data type is defined by a typedef statement in 'rawfile.h'
*/
/*
$Log: rawropen.c,v $
 * Revision 1.6  1995/08/31  17:26:14  baker
 * This version implements 2 changes:
 * 1)  The SuperDARN data type (common, special , or discretionary)
 * is identified.
 *
 * 2)  All printf statements have been changed to frprintf(stderr,
 * so that redicretso that redirection of stdout will not affect the error messages
 *
 * Revision 1.5  1994/03/02  21:06:28  baker
 * In the previous version, compressed Data General files could result
 * in a n attempt to read a negative number of bytes.  If this would
 * be the case, we now don't attempt to read anything if the length
 * would be negative.
 *
 * Revision 1.4  1994/03/02  16:54:59  baker
 * chadeleted unused variables.  Deleted definitions of path information
 * and replaced that with a new include file, "sd_path.h"
 *
 * Revision 1.3  1994/02/28  21:26:26  baker
 * calls to swab_word and swab_dword must use pointers not values.
 *
 * Revision 1.2  1994/02/07  16:28:34  baker
 * Changed from VMS delimiter to POSIX delimiter
 *
 * Revision 1.1  1993/09/29  20:42:35  baker
 * Initial revision
 *
*/
/* <SD_END */
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "radops.h"
#include "rawfile.h"
#include "endian.h"
#include "sd_path.h"

#define MAX_COMMON_CP 1000

extern size_t raw_read();

char rawropen_rev[] = {"$Revision: 1.6 $"};


RAW_FILE *rawropen(char filename[])
{
char fullpath[_MAX_PATH];

RAW_FILE *raw_file;

struct PATH_STRUCT filepath;


short int temp, etst;
long int ltemp;

int iostat;
char fullname[_MAX_PATH], path_list[256], *cpath;
char path_delimiter[] = ":";	/* this should be a semicolon for VMS systems */
char extension[_MAX_EXT];

short int recln;
long int recno;
char header[1024];
int headlen;

struct {short int junk, c_flag, num_bytes;} cmphead;

struct rawdata *rdata;
int offset;

/* The first step is to decompose the string fname into a path structure */

iostat = _splitpath(filename, filepath.node, filepath.dir, 
			filepath.fname, filepath.ext);
if (iostat == -1) {
	errno = ENOENT;
	perror("rawropen: invalid file name");
	return NULL;
	}

/* now, if the directory is given, copy it to the path_list string, 
	otherwise use getenv to form the path list */

if (strlen(filepath.dir) == 0) strcpy(path_list,
					getenv("SD_RAWROPEN_PATH"));
else strcpy(path_list, filepath.dir);

/* initialize the string for strtok */

cpath = strtok(path_list, path_delimiter);

/* initialize the RAW_FILE structure */

raw_file = calloc(1, sizeof(RAW_FILE));

if (strlen(filepath.ext) == 0) strcpy(extension, "dat");
else strcpy(extension, filepath.ext);

/* now try to open the files as long as there is something left in
	the path */

while (cpath != NULL && raw_file->fp == NULL) {
	strcpy(filepath.dir, cpath);
	strcpy(filepath.ext, extension);
	_makepath(fullname, filepath.node, filepath.dir, filepath.fname,
			filepath.ext);
	raw_file->fp = fopen(fullname, "r");
	strcpy(filepath.ext, "rin");
	_makepath(fullpath, filepath.node, filepath.dir, filepath.fname,
			filepath.ext);
	raw_file->inx = open(fullpath, O_RDONLY, 0);

	/* get next token in the path list */
	cpath = strtok(NULL, path_delimiter);
	}

/*  We have either successfully opend the files or exhausted all the
	paths available.  If we've failed, return a NULL.  If OK, then
	read the first record of the DAT file in order to figure out
	what type of file it is. */

if (raw_file->fp == NULL) return NULL;
if (raw_file->inx <= 0) fprintf(stderr,"Warning:  no raw index file\n");

/* OK, let's find out what type of file it is */

/*	read the first record of the DAT file.  
	If it is a HEADER record it will contain the record length,
	the name of the creating program, a version number and the date
	and time the file was created.  If it doesn't have this information
	then we have to assume that it is an old Data General file */

iostat = fread(&recln, sizeof(recln), 1, raw_file->fp);

if (iostat != 1) {
	perror("Error reading header record of DAT file");
	return NULL;
	}

/* check to see if this is a big endian machine */

if (endian(&etst) == 0) {
	temp = recln;
	swab_word(&recln, &temp);
	}

if (recln > 1024 || recln < 0) recln = 1024; /* probably a DG file */

iostat = fread(&recno, sizeof(recno), 1, raw_file->fp);

if (iostat != 1) {
	perror("rawropen: Error reading header record number");
	return NULL;
	}
if (endian(&etst) == 0) {
	ltemp = recno;
	swab_dword(&recno, &ltemp);
	}

headlen = recln - sizeof(recln) - sizeof(recno);

if (headlen > 0) 
    iostat = fread(header, sizeof(header[0]), headlen, raw_file->fp);
else strcpy(header, "DG");

if (iostat == 0) {
	fprintf(stderr,"rawropen: bad read while reading header string\n");
	return NULL;
	}

sscanf(header, "rawwrite version%lf", &(raw_file->raw_version));

if (raw_file->raw_version == 0) {
	fprintf(stderr,"DG type raw data file identified\n");
	raw_file->dg_flag = DG_NORMAL;
	if (fseek(raw_file->fp, 0, SEEK_SET) == EOF) { /*rewind the file */
		fprintf(stderr,"rawropen: unable to rewind file\n");
		return NULL;
		}
	/* now check to see if it is a normal DG file or a compressed file */
	
	if ((iostat = fread(&cmphead, sizeof(cmphead), 1, raw_file->fp)) != 1) {
		fprintf(stderr,"rawropen: Failed to read 1st record of a DG type file\n");
		return NULL;
		}
	if (fseek(raw_file->fp, 0, SEEK_SET) == EOF) {
		fprintf(stderr,"rawropen: Unable to rewind file after reading cmphead\n");
		return NULL;
		}
	if ((cmphead.c_flag < 70) && (cmphead.c_flag >=0)) {
		fprintf(stderr,"Compressed DG format.  c_flag = %d, num-bytes=%d\n",
			cmphead.c_flag, cmphead.num_bytes);
		raw_file->dg_flag = DG_COMPRESSED;
		}

	/* OK, set the initial offsets */
	
	raw_file->raw_offset = 0;
	raw_file->old_recno = 0;
	}

else	/* it must be a RADOPS 386 file */
	{
	fprintf(stderr,"Raw Data File Header:\n%s\n",header);
	raw_file->dg_flag = 0;
	raw_file->raw_offset = ftell(raw_file->fp);
	raw_file->old_recno = recno;
	}

/*  Now we have to identify the type of SuperDARN time (common, special
or discretionary) that is contained in this file. */

rdata = (struct rawdata *) calloc(1, sizeof(struct rawdata));
if (rdata == 0) 
{
    perror("rawropen: Unable to allocate memory for rawdata\n");
    return NULL;
}

offset = 0;
iostat = raw_read(raw_file, offset, rdata);
if (iostat == EOF) 
{
    perror("rawropen: Unable to read first data record of rawdata file\n");
    fprintf(stderr,"iostat = %d %d\n",iostat);
    return NULL;
}

if (rdata->PARMS.CP == 0) fprintf(stderr,"SuperDARN time is not identified\n");
else if (rdata->PARMS.CP >= 100 && rdata->PARMS.CP < MAX_COMMON_CP) 
    fprintf(stderr, "SuperDARN Common Time data:  CP = %hd\n",rdata->PARMS.CP);
else if (rdata->PARMS.CP < 0) 
    fprintf(stderr,"\nWARNING\a: SuperDARN DISCRETIONARY data.  CP = %hd\n\n",rdata->PARMS.CP);
else fprintf(stderr,"SuperDARN SPECIAL data.  CP = %hd\n",rdata->PARMS.CP);

iostat = raw_read(raw_file, -1, rdata);
if (iostat == EOF) 
{
    perror ("rawropen: Unable to rewind raw data file\n");
    return NULL;
}

free(rdata);

return raw_file;
}
