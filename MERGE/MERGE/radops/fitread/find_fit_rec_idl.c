#include <stdio.h>
/*
$Log:	find_fit_rec_idl.c,v $
 * Revision 1.2  94/09/06  10:26:08  10:26:08  baker (Kile Baker S1G)
 * removed path portion of includes.
 * 
 * Revision 1.1  1994/03/16  17:45:55  baker
 * Initial revision
 *
*/

#include "fit_errno.h"
#include "fitfile.h"
#include "endian.h"

long find_fit_rec_idl(argc,argv)
     int argc;
     void *argv[];
{

 FIT_FILE *fp;
 long int time_req;
 long int *time_found;
 long int find_fit_rec();
 
 fp = *(FIT_FILE **)argv[0];
 time_req = *(int *)argv[1];
 time_found = (long int *)argv[2];

 return find_fit_rec( fp, time_req, time_found);

}
