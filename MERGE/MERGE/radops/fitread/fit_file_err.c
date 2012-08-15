/*  This routine provides a standard error reporting mechanism for
    errors which are encountered while trying to read a FIT file

    If the subroutine read_fit returns a non-zero value, call this
    routine as follows:

    fit_file_err ( fit_file_ptr, value_returned_from_read_fit);

*/
/*
$Log:	fit_file_err.c,v $
 * Revision 1.2  95/01/17  11:29:11  11:29:11  baker (Kile Baker S1G)
 * added default action to print out unrecognized error code.
 * 
 * Revision 1.1  95/01/17  11:10:03  11:10:03  baker (Kile Baker S1G)
 * Initial revision
 * 
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "fitfile.h"
#include "fit_errno.h"

void fit_file_err(fp, ecode)
  FIT_FILE *fp;
  int ecode;
{
    if (fp == 0) 
    {
	fprintf(stderr, "FIT_FILE_ERR - file pointer is NULL\n");
	errno = EBADF;
	exit(errno);
    }
    
    if (ecode == EOK) return;
    
    fprintf (stderr, "FIT_FILE_ERR - ");
    
    switch (ecode) 
    {
    case RDFIT_INV_PARM_TIME :
	fprintf(stderr, "Invalid time in parameter block\n");
	break;
    case RDFIT_INV_PARM_RECORD :
	fprintf(stderr, "Parameter record has invalid structure\n");
	break;
    case RDFIT_INV_RECORD :
	fprintf(stderr, "data record is invalid\n");
	break;
    case RDFIT_INV_TIME :
	fprintf(stderr, "time tag in data record inconsistent\n");
	fprintf(stderr, "with time tag in parameter record\n");
	break;
    case RDFIT_INV_RANGE :
	fprintf(stderr, "invalid range gate number in data record\n");
	break;
    case RDFIT_INV_INX :
	fprintf(stderr, "INX record has invalid structure\n");
	break;
    case F_FIT_REC_LT :
	fprintf(stderr, "Request time < start time of file\n");
	break;
    case F_FIT_REC_INEXACT :
	fprintf(stderr, "Time found is not exactly equal to requested time\n");
	break;
    case F_FIT_REC_GT :
	fprintf(stderr, "Request time > end time of file\n");
	break;
    default:
	fprintf(stderr, "UNRECOGNIZED ERROR CODE %d from READ_FIT\n",ecode);
	fprintf(stderr, "Please notify K.B. Baker of this anomaly\n");
	fprintf(stderr, "(Kile_Baker@jhuapl.edu)\n");
    }
    
    if (ecode == F_FIT_REC_LT || ecode == F_FIT_REC_INEXACT ||
	ecode == F_FIT_REC_GT) 
	return;
    
    fprintf(stderr, "FIT_FILE_INFO:\n");
    fprintf(stderr, "  fit descriptor = %d.  inx descriptor = %d\n",
	    fp->fitdes, fp->inxdes);
    fprintf(stderr, "  file name and path: %s\n",fp->fname);
    fprintf(stderr, "  file created by %s\n",fp->source);
    fprintf(stderr, "  software: %s\n",fp->version);
    fprintf(stderr, "  offset in fitfile = %ld, offset in inxfile = %ld\n",
	    fp->fit_offset, fp->inx_offset);
    fprintf(stderr, "  first inx rec = %ld, last inx rec = %ld\n",
	    fp->inx_srec, fp->inx_erec);
    fprintf(stderr, "  time of last good record = %ld\n",fp->ctime);
    fprintf(stderr, "  first time in file = %ld, end time = %ld\n\n",
	    fp->stime, fp->etime);
    
    return;
}


