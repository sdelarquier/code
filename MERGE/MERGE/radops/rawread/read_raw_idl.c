#include <stdio.h>

#include "rawfile.h"
#include "endian.h"

extern void badlags();

long read_raw_idl(argc,argv)
     int argc;
     void *argv[];
{
 RAW_FILE *fp;
 int status;
 long offset;
 
 struct RAW_DATA *fd;

 fp = *(RAW_FILE **)argv[0];
 fd = (struct RAW_DATA *) argv[1];
 offset = *(long *) argv[2];

 status = raw_read( fp, offset,  fd );
 if (status == EOF) return status;
 
 badlags(fd);
 
 return status;
}

