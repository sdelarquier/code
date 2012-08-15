#include <stdio.h>
/*
$Log:	fitropen_idl.c,v $
 * Revision 1.2  94/09/06  10:29:15  10:29:15  baker (Kile Baker S1G)
 * removed path portion of includes
 * 
 * Revision 1.1  1994/03/16  17:44:50  baker
 * Initial revision
 *
*/

#include "fitfile.h"
#include "endian.h"

#include "/usr/local/rsi/idl/external/export.h"

#include <string.h>

FIT_FILE *fitropen_idl( int argc,void *argv[])
{
  FIT_FILE *fitfile, *fitropen();
  IDL_STRING strptr;
  char *filename;

  strptr = *(IDL_STRING *)argv[0];
  filename = strptr.s;

printf("%s\n", filename);

  return fitropen(filename);
}



