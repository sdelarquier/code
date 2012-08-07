/*	This routine simply closes the indicated files */
/*
$Log: raw_close.c,v $
 * Revision 1.3  1994/03/02  17:41:56  baker
 * added include of <fcntl.h> to make sure 'close' was properly defined.
 * /
 *
 * Revision 1.2  1993/09/29  21:11:51  baker
 * fixed the #include statements to be compatible with UNIX rather than VMS
 *
 * Revision 1.1  1993/09/28  21:21:12  baker
 * Initial revision
 *
*/

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include "radops.h"
#include "rawfile.h"

char raw_close_rev[] = {"$Revision: 1.3 $"};

int raw_close(raw_file)
	RAW_FILE *raw_file;
{
int status;

status = fclose(raw_file->fp);

if (raw_file->inx > 0) status = status || close(raw_file->inx);
free(raw_file);
return status;
}
