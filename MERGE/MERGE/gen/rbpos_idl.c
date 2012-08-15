/* File: /home/darn/gen/rbpos_idl.c */
/* Last Modification: 13-MAR-2002   */
/* Modified by: Dieter Andre        */
/**/
/* IDL interface to rbpos_gen       */
/* Revision 2.0; 20020313 Dieter Andre */
/* Changed call to frang and rsep */

#include <stdio.h>

#include "fit_errno.h"
#include "fitfile.h"
#include "endian.h"

long rbpos_idl( int argc, void *argv[])
{
  short *range;
  short *bmnum;
  float *frang;
  float *rsep;
  short *st_id;
  short *year;
  long  *yr_sec;
  short *mgflag;
  short *cflag;
  short *gsflag;
  short *err;
  float *height;
  float *pos;
  char  cflg;
  char  gsflg;
  
  range  = (short *)argv[0];
  bmnum  = (short *)argv[1];
  frang  = (float *)argv[2];
  rsep  = (float *)argv[3];
  height = (float *)argv[4];
  pos    = (float *)argv[5];
  st_id  = (short *)argv[6];
  year   = (short *)argv[7];
  yr_sec = (long  *)argv[8];
  mgflag = (short *)argv[9];
  cflag  = (short *)argv[10];
  gsflag  = (short *)argv[11];
  err    = (short *)argv[12];
  cflg   = (char) *cflag;
  gsflg  = (char) *gsflag;

  rbpos_gen(range,bmnum, frang, rsep,height,pos,st_id,year,yr_sec,mgflag,&cflg,&gsflg,err);

  return (long) err;
}





