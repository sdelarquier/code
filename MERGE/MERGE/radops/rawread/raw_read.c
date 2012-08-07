/*	This routine reads the raw data file.
	The parameters, pulse table, lag table and comment buffer
	are stored in the rawdata structure in the appropriate places.
	The lag-0 powers and the ACFs are stored in a global buffer
	called indata.  The routine "decommutate" is then used to pull
	these compressed values out of the input buffer and put them
	into the rawdata structure

	calling sequence:

	iostat = raw_read(raw_data_file_pointer, offset, raddat);

		where raw_data_file_pointer is a pointer to a file opened with
		the "raw_open" function.

		offset is a long int containing either -1 (rewind file), 
		0 (indicating that
		the routine should read the next sequential record of data) or
		a byte offset within the raw data file.  This allows the
		calling program to read a random data record.  The calling
		routine should first use the function "find_raw_rec" to locate
		the start of the record desired.
	
		The pointer, raddat, must be a pointer to a structure
		of type "rawdata"
  */	

/*
$Log: raw_read.c,v $
 * Revision 1.4  1994/03/02  17:23:15  baker
 * saadded function prototypes.
 *
 * Revision 1.3  1994/02/28  21:25:22  baker
 * calls to swab_word and swab_dword were being made using the values
 * rather than pointers.  Pointers must be used.
 *
 * Revision 1.2  1994/02/07  17:54:24  baker
 * changed all calls to swab to new calls to sd_swab.  This avoids a
 * problem with the name swab in IDL.
 *
 * Revision 1.1  1993/09/28  21:50:42  baker
 * Initial revision
 *
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "radops.h"
#include "rawfile.h"
#include "endian.h"

extern size_t read_raw_dg(short int *, struct rawdata *, RAW_FILE *);
extern void swap_rawparms(struct radops_parms *, struct radops_parms *);
extern void decommutate(short int *, short int, struct rawdata *, double);


char raw_read_rev[] = {"$Revision: 1.4 $"};

/* ----------------------------------------------------------------------*/

size_t raw_read(rawfile, offset, raddat)
	RAW_FILE *rawfile;
	long offset;
	struct rawdata *raddat;

{
long recno;
short int recln;
char tag[9];
size_t iostat;
short int pbuf[30];
/* increased buffer length from 8000 to accommodate long pulse sequenced; 20021015 DA */
short int indata[12000];
short int etst, temp;
long int ltemp;
struct radops_parms ptemp;

/* if offset is negative, rewind the file */
if (offset < 0)
	{
	rawfile->old_recno = 0;
	fseek(rawfile->fp, 0, 0);
	rawfile->raw_offset = 0;
	return 0;
	}


/*	first read the record length of the next record */
READ_RECORD:
if (offset != 0)
	{
	rawfile->raw_offset = offset;
	if ((iostat = fseek(rawfile->fp, offset, 0)) !=0)
		{
		printf("Improper FSEEK (%d) to %d in raw data file\n",
				iostat,offset);
		return EOF;
		}
	}

/*  If the current offset in the file is 0 and the file is a RADOPS 386
	type file, then we have to skip over the header record */

if ((rawfile->raw_offset == 0) && (rawfile->dg_flag == 0)) {
  if ((iostat = fread(&recln, sizeof(recln), 1, rawfile->fp)) != 1)
	{
	printf("bad read while reading record length\n");
	return EOF;
	}
  else if (endian(&etst) == 0) {
	temp = recln;
	swab_word(&recln, &temp);
	}

/*	now read the relative record number  */

  if ((iostat = fread(&recno, sizeof(recno), 1, rawfile->fp)) != 1)
	{
	printf("bad read while reading record number.\n");
	return iostat;
	}
  else if (endian(&etst) == 0) {
	ltemp = recno;
	swab_dword(&recno, &ltemp);
	}

/* now read the header */

  if ((iostat = fread(&indata, 1, recln - sizeof(recln)-sizeof(recno),
			rawfile->fp)) == 0) {
	printf("bad read while reading header string\n");
	return 0;
	}
  }

/*  OK - now read the next data record */


/*  check the dg_flag to see if we are reading an old style data file */

if (rawfile->dg_flag)
	{
	iostat = read_raw_dg(indata, raddat, rawfile);
	++(rawfile->old_recno);
	rawfile->raw_offset = ftell(rawfile->fp);
	decommutate(indata, iostat, raddat, rawfile->raw_version);
	return iostat;
	} 

if ((iostat = fread(&recln, sizeof(recln), 1, rawfile->fp)) != 1)
	{
	printf("bad read while reading record length\n");
	return EOF;
	}
else if (endian(&etst) == 0) {
	temp = recln;
	swab_word(&recln, &temp);
	}

/*	now read the record number  */

if ((iostat = fread(&recno, sizeof(recno), 1, rawfile->fp)) != 1)
	{
	printf("bad read while reading record number.\n");
	return iostat;
	}
else if (endian(&etst) == 0) {
	ltemp = recno;
	swab_dword(&recno, &ltemp);
	}

if (offset != 0)
	rawfile->old_recno = recno - 1;

if (recno != rawfile->old_recno+1)
	{
	printf("New record number != old rec+1: %d  %d\n",recno, 
		rawfile->old_recno);
	return EOF;
	}

/*	now read the tag and make sure it says "rawwrite" */

if ((iostat = fread(tag, sizeof(tag[0]), 8, rawfile->fp)) != 8)
	{
	printf("RAW_READ: bad read while reading tag.\n");
	return iostat;
	}

tag[8] = 0;

if (strcmp(tag,"rawwrite") != 0)
	{
	printf("RAW_READ: Invalid tag = %s\n",tag);
	/*  check and see if this is a DG type  raw data file */
	fseek(rawfile->fp, 0, 0);
	fread(pbuf, sizeof(short int), 30, rawfile->fp);
	sd_swab(pbuf, pbuf, 60);
	if (*pbuf == 70)
		{
		rawfile->dg_flag = 1;
		goto READ_RECORD;
		}
	else
		return EOF;
	}

/*	Now read the parameter block, the pulse table, lag table, and 
	comment buffer */

if ((iostat = fread(&(raddat->PARMS), sizeof(raddat->PARMS), 1, rawfile->fp))
	!= 1)
	{
	printf("bad read while reading parameter block.\n");
	return EOF;
	}
if (endian(&etst) == 0) {
	memcpy(&ptemp, &(raddat->PARMS), sizeof(ptemp));
	swap_rawparms(&(raddat->PARMS), &ptemp);
	}

/* now read the pulse pattern and the lag_table */

if ((iostat = fread(&(raddat->PULSE_PATTERN[0]),
		sizeof(raddat->PULSE_PATTERN[0]),
		raddat->PARMS.MPPUL, rawfile->fp)) != raddat->PARMS.MPPUL)
	{
	printf("bad read while reading pulse pattern.\n");
	return EOF;
	}

if (endian(&etst) == 0) {
	sd_swab(&(raddat->PULSE_PATTERN), &(raddat->PULSE_PATTERN),
		2*PULSE_PAT_LEN);
	}

if ((iostat = fread(&(raddat->LAG_TABLE[0][0]), sizeof(raddat->LAG_TABLE[0][0]),
		raddat->PARMS.MPLGS, rawfile->fp)) == 0)
	{
	printf("bad read while reading lag table[0].\n");
	return EOF;
	}

if (endian(&etst) == 0) {
	sd_swab(&(raddat->LAG_TABLE[0][0]), &(raddat->LAG_TABLE[0][0]), 
		sizeof(raddat->LAG_TABLE[0])*LAG_TAB_LEN);
	}

if ((iostat = fread(&(raddat->LAG_TABLE[1][0]), sizeof(raddat->LAG_TABLE[0][0]),
		raddat->PARMS.MPLGS, rawfile->fp)) == 0)
	{
	printf("bad read while reading lag table[1].\n");
	return EOF;
	}


if (endian(&etst) == 0) {
	sd_swab(&(raddat->LAG_TABLE[1][0]), &(raddat->LAG_TABLE[1][0]), 
		sizeof(raddat->LAG_TABLE[0])*LAG_TAB_LEN);
	}

/* now read in the comment buffer */

if ((iostat = fread(&(raddat->COMBF[0]), sizeof(raddat->COMBF[0]), 
		COMBF_SIZE, rawfile->fp)) != COMBF_SIZE)
	{
	printf("bad read while reading comment buffer.\n");
	return EOF;
	}

/*	OK, we've read all the preliminaries, so now get the compressed data */

recln = recln - sizeof(recln) - sizeof(recno) - 8*sizeof(tag[0]) -
	sizeof(struct radops_parms) - 
	raddat->PARMS.MPPUL*sizeof(raddat->PULSE_PATTERN[0]) -
	2*raddat->PARMS.MPLGS*sizeof(raddat->LAG_TABLE[0][0]) -
	COMBF_SIZE*sizeof(raddat->COMBF[0]);

if (recln <= 0)
	{
	printf("Error in record length.  recln = %d\n");
	return EOF;
	}

if ((iostat = fread(indata, sizeof(*indata), recln/sizeof(*indata), 
		rawfile->fp)) != recln/sizeof(*indata))	
	{
	printf("Record not long enough. Number of items = %d  %d\n",
		recln/sizeof(*indata), iostat);
	return EOF;
	}
rawfile->raw_offset = ftell(rawfile->fp);

rawfile->old_recno = recno;

if (endian(&etst) == 0) sd_swab(indata, indata, recln);

decommutate(indata, iostat, raddat, rawfile->raw_version);
return iostat;
}

