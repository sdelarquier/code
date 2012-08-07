/* radar_pos.c */
/* converted from Fortran */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include "radops.h"


void radar_pos( short int* st_id, short int* year, int* yr_sec, float pos[5])

{
  extern int hardware_setup( int year, int yr_sec, int station, struct SD_HARDWARE *hd);

  struct SD_HARDWARE hd;
  int istat;

  istat=  hardware_setup( *year, *yr_sec, *st_id, &hd);

  if (istat !=0)
    {
      printf( "%d\n", istat);
      exit;
    }
  pos[ 0]= hd.geo_lat;
  pos[ 1]= hd.geo_long;
  pos[ 2]= hd.boresite;
  pos[ 3]= hd.beam_sep;
  pos[ 4]= hd.rec_rise;
  return;
 } /* radar_pos */





