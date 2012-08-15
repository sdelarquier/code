
#include <stdio.h>

#include "rawfile.h"
#include "endian.h"

extern int raw_close();

int raw_close_idl(argc,argv)
     int argc;
     void *argv[];
{
  RAW_FILE *fp;

  fp = *(RAW_FILE **)argv[0];
  
  return raw_close(fp);
}


