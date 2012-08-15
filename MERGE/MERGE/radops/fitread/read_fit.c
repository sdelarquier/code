/* this routine reads all the fit files 
   the file has to opened by fitropen routine

/* 
   read_fit reads a logical record from a FIT file
   it determines which routine to call for each kind of FIT file

   it has 2 arguments:
   fp = pointer to FIT_FILE structure = pointer to the data base which
        contains the offset to index and fit files, time etc. 
   fd = pointer to the FIT_DATA structure = pointer to the data

   it returns EOK  : if there is no error
   and        error code : otherwise
*/

/*
$Log:	read_fit.c,v $
 * Revision 1.5  94/09/06  10:30:15  10:30:15  baker (Kile Baker S1G)
 * removed path portion of includes
 * 
 * Revision 1.4  1993/11/24  20:11:30  baker
 * changed to clear the input array before reading in the new data.
 *
 * Revision 1.3  1993/09/28  14:49:50  baker
 * The structure FIT_FILE contains the element "ctime" which is the value
 * of the time of the last record successfully read from the file.  This value
 * was not being updated when doing sequential reads.  This has been corrected
 * in this version of read_fit.
 *
 * Revision 1.2  1993/09/07  15:37:48  baker
 * We now check the FIT_FILE pointer passed to the routine to make sure
 * it isn't a NULL.
 *
 * Revision 1.1  1993/08/20  20:34:34  baker
 * Initial revision
 *
*/

#include <stdio.h> 
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "fitfile.h"
#include "fit_errno.h"

char read_fit_rev[] = {"$Revision: 1.5 $"};

int read_fit( FIT_FILE *fp, struct FIT_DATA *fd )
{
   int (*read_fun) ();
   int iostat;

   if (fp == NULL) {
     errno = EBADF;
     perror("read_fit: ");
     return EBADF;
   }

   /*  clear the data structure */
   memset(fd, 0, sizeof(struct FIT_DATA));

   /* get the read function */
   read_fun = fp->fit_read;

   /* call the function */
   iostat = read_fun(fp,fd);
   if (iostat == EOK) fp->ctime = fd->rec_time;

   return(iostat);

} /* read_fit() */
    
