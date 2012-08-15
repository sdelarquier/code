/* This routine provides the interface for IDL to the fortran routine
   median_filter
*/
/*
$Log:	median_filter_idl.c,v $
 * Revision 1.2  94/08/04  10:04:43  10:04:43  baker (Kile Baker S1G)
 * Removed explicit include path.
 * 
*/

#include <stdio.h>

#include "fit_errno.h"
#include "fitfile.h"
#include "endian.h"

/* modified to work with median_filter v 1.5 */
/* DA 04-JUN-1997 */

long *median_filter_idl(argc,argv)
     int argc;
     void *argv[];
{
  char *kbad;
  float *vel;
  char *gscat;
  short *ifilt;
  short *dim_1;
  short *dim_2;

  kbad   = (char *)argv[0];
  vel    = (float *)argv[1];
  gscat	 = (char *)argv[2];
  ifilt  = (short *)argv[3];
  dim_1  = (short *)argv[4];
  dim_2  = (short *)argv[5];

  median_filter( kbad, vel, gscat, ifilt, dim_1, dim_2);

  return 0;
}

