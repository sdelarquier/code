/* $Id: mlt_idl.c,v 1.2 1996/03/20 22:55:57 baker Exp $ */
/* This routine provides the IDL interface to 
   the magnetic local time conversion routines */
/*
$Log: mlt_idl.c,v $
 * Revision 1.2  1996/03/20  22:55:57  baker
 * Changed the declaration of the arguments to
 * use the types "int16" and "int32" instead of
 * short and long.  This is to avoid conflicts with
 * machines that have different word sizes.  The
 * header file "SD_types.h" is now required.
 *
 * Revision 1.1  1996/03/14  15:59:35  baker
 * Initial revision
 *
*/

#include <stdio.h>
#include "SD_types.h"

float mlt_idl(argc,argv)
     int argc;
     void *argv[];
{
  int16 *year;
  int32  *t;
  float mlt();
  float *mlong,mslong;

  if (argc < 3) return -1;
  
  year  = (int16 *) argv[0];
  t     = (int32 * ) argv[1];
  mlong = (float *) argv[2];

  return mlt(year,t,mlong,&mslong);
}

