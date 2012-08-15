#include <stdio.h>
/*
$Log:	fit_close_idl.c,v $
 * Revision 1.1  94/09/06  10:27:20  10:27:20  baker (Kile Baker S1G)
 * Initial revision
 * 
*/

#include "fitfile.h"
#include "endian.h"

FIT_FILE *fit_close_idl(argc,argv)
     int argc;
     void *argv[];
{
  FIT_FILE *fp;

  fp = *(FIT_FILE **)argv[0];
  
  return fit_close(fp);
}


