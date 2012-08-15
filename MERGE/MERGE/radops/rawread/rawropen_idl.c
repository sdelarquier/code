/* This routine provides the interface between the standard "rawropen" 
   subroutine, and the IDL call external for rawropen.
*/

#include <stdio.h>
#include "rawfile.h"

#include "/usr/local/rsi/idl/external/export.h"

RAW_FILE *rawropen_idl( int argc, void *argv[])
{
   RAW_FILE *rawfile, *rawropen();
   IDL_STRING strptr;
   char *filename;
   
   strptr = *(IDL_STRING *)argv[0];
   filename = strptr.s;
   rawfile = rawropen(filename);
   
   return rawfile;
}





    

