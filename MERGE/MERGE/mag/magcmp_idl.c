#include <stdio.h>

#include "fit_errno.h"
#include "fitfile.h"
#include "endian.h"

long *magcmp_idl(argc,argv)
     int argc;
     void *argv[];
{
  short *ifrst;
  long  *err;
  float *tm4;
  float frho4;
  float *flat4;
  float *flon4;
  float *bx4;
  float *by4;
  float *bz4;
  float *b4;
  float *heit;

  fflush(stdout);
  tm4    = (float *)argv[0];
  heit   = (float *)argv[1];
  flat4  = (float *)argv[2];
  flon4  = (float *)argv[3];
  bx4    = (float *)argv[4];
  by4    = (float *)argv[5];
  bz4    = (float *)argv[6];
  b4     = (float *)argv[7];
  ifrst  = (short *)argv[8];

  frho4 = 6370. + *heit;
  err = 0;
  magcmp(tm4,&frho4,flat4,flon4,bx4,by4,bz4,b4,ifrst);

  return err;
}
