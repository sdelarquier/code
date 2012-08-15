/* File: radar_pos_idl.c */
/* Last Modification: 27-FEB-2002 */
/* Author: Dieter Andre */

#include <stdio.h>

#include "radops.h"

long radar_pos_idl( int argc, void *argv[])
{
  short int *st_id;
  short int *year;
  long int *yr_sec;
  float *pos;
  char  cflg;
  
  st_id  = (short int *) argv[0];
  year  = (short int *)  argv[1];
  yr_sec  = (long int *) argv[2];
  pos    = (float *)     argv[3];

  radar_pos( st_id, year, yr_sec, pos);

  return (long) 1;
}


