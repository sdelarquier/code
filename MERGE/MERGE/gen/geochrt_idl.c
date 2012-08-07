/* File: /home/darn/gen/geochrt_idl.c */
/* Last Modification: 13-MAR-2002 */

/* This routine provides the IDL interface to the fortran routine geochrt.*/

/*
 * Revision 1.6 13-MAR-2002 Dieter Andre'
 * Time dependent rbpos
 * Use frang and rsep
 * Revision 1.5 24-Feb-2000 Dieter Andre'
 * Took out parameter irt, because it ie NOT used by geochrt.f
 *
 * Revision 1.4  94/08/08  13:53:00  13:53:00  miker (Michael Ruohoniemi S1G)
 * There was in inconsistency in the definition of the function and the 
 * value it actually returned.  The returned value is the station ID as 
 * a 32-bit integer.  The function declared that it was returning a pointer
 * to the value rather than the value itself.
 * 
 * Revision 1.3  94/08/05  21:42:06  21:42:06  bristow (Bill Bristow S1G)
 * Changed name to geochrt_idl
 * 
 * Revision 1.2  1994/08/04  10:01:58  baker
 * Added the RCS Log feature to the comments.  Also removed explicit
 * directory paths for the include files.
 *
*/

#include <stdio.h>

#include "fit_errno.h"
#include "fitfile.h"
#include "endian.h"

long geochrt_idl( int argc, void *argv[])
{
  short *st_id;
  short *year;
  long  *yr_sec;
  float *frang;
  float *rsep;
  float *height;
  short *yr;
  float *lat;
  float *lon;
  float *rho;
  float *elv;
  float *azc;

  st_id  = (short *)argv[0];
  year   = (short *)argv[1];
  yr_sec = (long  *)argv[2];
  frang  = (float *)argv[3];
  rsep  = (float *)argv[4];
  height = (float *)argv[5];
  lat    = (float *)argv[6];
  lon    = (float *)argv[7];
  rho    = (float *)argv[8];
  elv    = (float *)argv[9];
  azc    = (float *)argv[10];

  geochrt( st_id,year,yr_sec, frang, rsep,height, lat,lon,rho,elv,azc);

  return (long) *st_id;
}









