#include <stdio.h>

void block_cnv_sec_mdhms_idl(argc,argv)
     int argc;
     void *argv[];
{
  short *yr, mo, dy, hr, mn, sc; 
  short i, *count, *mdhms;
  long seconds, *tar;

  tar   = (long *)  argv[0];
  count = (short *) argv[1];
  yr    = (short *) argv[2];
  mdhms = (short *) argv[3];


  for( i=0; i<*count; i++){
    seconds = *(tar+i);
    cnv_sec_mdhms(yr,&mo,&dy,&hr,&mn,&sc,&seconds);

    *(mdhms+5*i)   = mo;
    *(mdhms+5*i+1) = dy;
    *(mdhms+5*i+2) = hr;
    *(mdhms+5*i+3) = mn;
    *(mdhms+5*i+4) = sc;
    
  }
}
