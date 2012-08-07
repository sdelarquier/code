#include <stdio.h>
/*  This routine provides the interface between the standard C routines
for reading FIT files and IDL */
/*
$Log:	read_fit_idl.c,v $
 * Revision 1.3  94/09/06  10:32:04  10:32:04  baker (Kile Baker S1G)
 * removed path portion of includes
 * 
 * Revision 1.2  1994/03/16  17:50:35  baker
 * eliminated an unnecessary line of code.
 *
 * Revision 1.1  1994/03/16  17:46:41  baker
 * Initial revision
 *
*/

#include "fitfile.h"
#include "endian.h"

long read_fit_idl(argc,argv)
     int argc;
     void *argv[];
{
 FIT_FILE *fp;
 struct FIT_DATA fdh;
 int status;
 char *fd;

 fp = *(FIT_FILE **)argv[0];
 fd = argv[1];

 status = read_fit( fp, fd );

 return status;
}
