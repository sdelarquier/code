
#include <stdio.h>

#include "rawfile.h"
#include "endian.h"

extern void raw_file_info();

long raw_file_info_idl(argc,argv)
     int argc;
     void *argv[];
{
  RAW_FILE *fp;

  fp = *(RAW_FILE **)argv[0];
  
  raw_file_info(fp);
  return 0L; 
}


