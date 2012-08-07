/*  This subroutine prints out the contents of the raw_file information
    structure
*/

/*
$Log: raw_file_info.c,v $
 * Revision 1.1  1994/03/07  20:04:12  baker
 * Initial revision
 *
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "rawfile.h"

char raw_file_info_rev[] = {"$Revision: 1.1 $"};

void raw_file_info(RAW_FILE * rawfile) 
{
   printf("RAW_FILE pointer has value %x\n",rawfile);
   if (rawfile == 0) return;

   printf("DAT file pointer = %x\n",rawfile->fp);
   
   printf("RIN file ID = %d\n",rawfile->inx);
   
   printf("DG flag = %d  = ",rawfile->dg_flag);
   
   switch (rawfile->dg_flag) 
   {
   case 0:
       printf("RADOPS 386\n");
       break;
   case DG_NORMAL:
       printf("DG NORMAL\n");
       break;
   case DG_COMPRESSED:
       printf("DG COMPRESSED\n");
       break;
   default:
       printf("INVALID flag\n");
   }

   if (rawfile->dg_flag == 0) printf("Version = %4.2f\n",rawfile->raw_version);
   
   printf("current offset = %d\nlast record number = %d\n",
	  rawfile->raw_offset, rawfile->old_recno);

   return;
}

   
