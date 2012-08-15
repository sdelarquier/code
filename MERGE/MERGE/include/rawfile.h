/*   This header defines the RAW_FILE pointer structure */

#ifndef RAWFILE_H_INCLUDED

#include <stdio.h>

#define DG_NORMAL 1	/* Normal Data Gnereal raw data file */
#define DG_COMPRESSED 3 /* Compressed Data General raw data file */

typedef struct RAW_FILE_INFO {
	double raw_version;	/* version number for RADOPS 386 file */
	long int raw_offset;	/* current byte offset in the file */
	long int old_recno;	/* record number of the last record */
	FILE *fp;		/* ANSI file pointer for raw data file */
	int inx;		/* UNIX file id number for raw index file */
	short int dg_flag;	/* data general type flag */
	} RAW_FILE;

#define RAWFILE_H_INCLUDED

#endif
