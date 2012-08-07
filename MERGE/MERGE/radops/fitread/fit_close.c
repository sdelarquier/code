/*	This routine simply closes the indicated files */
/*
$Log:	fit_close.c,v $
 * Revision 1.2  94/09/06  10:26:39  10:26:39  baker (Kile Baker S1G)
 * removed path portion of includes.
 * 
 * Revision 1.1  1993/08/20  20:48:51  baker
 * Initial revision
 *
*/

#include <stdio.h>
#include <stdlib.h>
#include "fitfile.h"

char fit_close_rev[] = {"$Revision: 1.2 $"};


int fit_close(fit_file)
	FIT_FILE *fit_file;
{
int status;

status = close(fit_file->fitdes);
status = close(fit_file->inxdes);

free(fit_file);
return status;
}
