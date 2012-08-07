/* SD_BEGIN> */
/*  This routine reads a block of data from a FIT file into arrays that
    are passed to IDL for plotting.  Three parameters are read in, defined by
    variables in argv[3],argv[4], and argv[5]

$Log: son_of_scan_read.c,v $
 *
 * Son_of_scan_read Revision 1.14leic 1997/12/17  milan
 * Pass index of first beam to scan_read (start_beam) to which
 * *icount is first initialised.  This allows a file
 * to be appended to already existing arrays.  For a new file,
 * set start_beam=0.
 *
 * Scan_read Revision 1.13leic 1997/12/17  milan
 * Ha ha - the tyranny of the posg.dat and posm.dat files is
 * at and end.  The revolutionary council have decreed that
 * the offending section of the code be summarily executed.
 *
 * Scan_read Revision 1.12leic 1997/05/22  milan
 * Bodge City - new pulse coding means that sampling period (SMSEP)
 * is not necessarily equal to the gate length (RSEP), even though
 * most of the software uses SMSEP to calculate gate positions.
 * Hence fix the software so that RSEP/0.15 (convert km to us) is
 * loaded into SMSEP array for output.  Tut tut.
 *
 * Scan_read Revision 1.11leic 1997/04/09  milan
 * Print out integration time in run-time output
 *
 * Scan_read Revision 1.10leic 1996/10/14  milan
 * Increment *icount before quitting routine to give correct count
 * of number of beams read (not just index of last array position
 * filled).
 *
 * Scan_read Revision 1.9leic 1996/5/21  milan
 * Only data between st and et are loaded into arrays, allowing
 * a subset of a fit file to be read.
 *
 * Scan_read Revision 1.8leic 1996/1/16  milan
 * Changed beam_read to load scan_id and scan_mark arrays.
 * Also generates an error and aborts if file loaded is larger
 * than arrays.
 *
 * Beam_read Revision 1.7leic 1996/1/5  milan
 * Made output prettier and lost redundant code
 *
 * Beam_read Revision 1.6leic 1995/11/20  milan
 * Revised so cross-correlation parameters are tested against x-quality flag
 *
 * Beam_read Revision 1.5leic 1995/06/03  milan
 * Changed block_read to read data into arrays in terms of beams, not scans
 * and returns times in seconds from the start of the year, not day
 *
 * Block_read Revision 1.4  1995/03/06  19:33:55  bristow
 * Changed call to rbpos so that the first call uses index 1.
 *
 * Block_read Revision 1.3  1995/01/17  14:37:50  bristow
 * Revised so the time was begining of scan.
 *
 * Block_read Revision 1.2  1995/01/06  14:02:35  bristow
 * Initial revision					*/
/* <SD_END */

#include <stdio.h>

#include "fitfile.h"
#include "endian.h"
#include "/usr/local/rsi/idl/external/export.h"

#define N_BMS 16

#define  PWR_LAG0 796
#define  PWR_L 548
#define  PWR_L_ERR 972
#define  PWR_S 555
#define  PWR_S_ERR 979
#define  VEL 327
#define  VEL_ERR 751
#define  WIDTH_L 747
#define  WIDTH_L_ERR 1171
#define  WIDTH_S 754
#define  WIDTH_S_ERR 1178
#define  STND_DEV_L 1058
#define  STND_DEV_S 1065
#define  STND_DEV_PHI 1271
#define  X_PWR_L 763
#define  X_PWR_L_ERR 1187
#define  X_PWR_S 770
#define  X_PWR_S_ERR 1194
#define  X_VEL 542
#define  X_VEL_ERR 966
#define  X_WIDTH_L 962
#define  X_WIDTH_L_ERR 1386
#define  X_WIDTH_S 969
#define  X_WIDTH_S_ERR 1393
#define  PHI0 369
#define  PHI0_ERR 793
#define  ELEV 428
#define  ELEV_LOW 861
#define  ELEV_HIGH 939
#define  NUM_LAGS 854


long son_of_scan_read(argc,argv)
     int argc;
     void *argv[];
{
  float posc[2][MAX_RANGE+1][N_BMS+1];
  float pos[8],height;
  float *a1,*a2,*a3;
  long st,et,ct,start_beam,max_beams,*time,jndx,*icount;
  short lagfrl,smsepl,inttl,icnt,count,*lagfr;
  short *noise_lev,*noise_lag0,*noise_vel,*atten;
  short *dir,*smsep,*tfreq,*grnscat,mgflag,bmnum;
  short yr,mo,dy,hr,mn,sc,*st_id,scan_idl,*scan_id,*scan_mark,err,scan;
  int n_same;
  short i,ibm,igate,items,j;
  int status;
  int pval1,pval2,pval3;
  IDL_STRING strptr1,strptr2,strptr3;
  FIT_FILE *ifileptr;
  struct FIT_DATA dstr;
/*  FILE *u1,*u2; */
  
  st        = *(long *) argv[0];
  et        = *(long *) argv[1];
  ifileptr  = *(FIT_FILE **) argv[2];
  start_beam= *(long *) argv[3];
  max_beams = *(long *) argv[4];
  strptr1   = *(IDL_STRING *) argv[5];
  strptr2   = *(IDL_STRING *) argv[6];
  strptr3   = *(IDL_STRING *) argv[7];
  a1        = argv[8];
  a2        = argv[9];
  a3        = argv[10];
  time      = argv[11];
  dir       = argv[12];
  lagfr     = argv[13];
  smsep     = argv[14];
  tfreq     = argv[15];
  grnscat   = argv[16];
  st_id     = (short *) argv[17];
  icount    = argv[18];
  noise_lev = argv[19];
  noise_lag0= argv[20];
  noise_vel = argv[21];
  atten     = argv[22];
  scan_id   = argv[23];
  scan_mark = argv[24];

  height = 400.;

  pval1 = 0;
  pval2 = 0;
  pval3 = 0;
  for( i=0; i<strlen(strptr1.s); i++ )
    pval1 += (int) *(strptr1.s+i);
  for( i=0; i<strlen(strptr2.s); i++ )
    pval2 += (int) *(strptr2.s+i);
  for( i=0; i<strlen(strptr3.s); i++ )
    pval3 += (int) *(strptr3.s+i);

  fprintf(stderr,"Date         time        scan id   lag to 1st range   gate length   dwell\n");
  fprintf(stderr,"-------------------------------------------------------------------------\n");

/* Initialize variables */
  *icount = start_beam-1;
  ct      = 0;
  scan_idl= 0;
  lagfrl  = 0;
  smsepl  = 0;
  inttl   = 0;
  count   = 0;
  n_same  = 0;

  while( ct < et ){

    status = read_fit(ifileptr,&dstr);

/* Deal with bad status - maybe end of file 
 * if too many occur then quit                  */
    if( status != 0 ) {
	++count;
	if( status == -1 ){
	    /* these two are never opened; 20020925 DA */
	    /* used in beam_read and scan_read to read from posg.dat and posm.dat */
	    /* fclose(u1); */
	    /* fclose(u2); */
	    status = fit_close(ifileptr);
	    return 0;
	}
	fprintf(stderr,"ERROR: status number: %d   count: %d\n",status,count);
	if( count >= 10 ){
	    status = -1;
	    break;
	}
	else continue;
    }
    
/* Status is ok - zero bad status counter */
    count = 0;

    yr = dstr.p.YEAR;
    mo = dstr.p.MONTH;
    dy = dstr.p.DAY;
    hr = dstr.p.HOUR;
    mn = dstr.p.MINUT;
    sc = dstr.p.SEC;
    
    ct = cnv_mdhms_sec(&yr,&mo,&dy,&hr,&mn,&sc);

/* Only start loading files when current time is greater than start time */
    if (ct > st) {

/* Report change in scan id, lag to first range, gate length, or integration time */
    if ( (lagfrl != dstr.p.LAGFR) || (smsepl != (int)(dstr.p.RSEP/0.15)) || (scan_idl != dstr.p.CP) || (inttl != dstr.p.INTT)) {
      if ( n_same != 1 )
      fprintf(stderr,"%02d/%02d/%04d   %02d%02d %02ds     %5d       %6.1f km       %6.1f km     %2d s\n",dy,mo,yr,hr,mn,sc,dstr.p.CP,dstr.p.LAGFR*0.15,dstr.p.RSEP*1.0,dstr.p.INTT);
      n_same = 1;
      }
    else
      n_same++;

    *st_id   = dstr.p.ST_ID;
    scan_idl = dstr.p.CP;
    inttl    = dstr.p.INTT;
    smsepl=(int)(dstr.p.RSEP/0.15);
    lagfrl=dstr.p.LAGFR;
    
/* Increment beam counter and check its not larger than max_beams */
/* If so, then close files and exit */
    ++(*icount);
    if (*icount == max_beams) {
      fprintf(stderr,"\nERROR: fit file larger than available array size, truncating...");
      status = fit_close(ifileptr);  
      return 0;
    }

    time[*icount]         = ct;
    lagfr[*icount]        = dstr.p.LAGFR;
    smsep[*icount]        = (int)(dstr.p.RSEP/0.15);
    dir[*icount]          = dstr.p.BMNUM;
    scan_id[*icount]      = dstr.p.CP;
    scan_mark[*icount]    = dstr.p.SCAN;
    *(tfreq+*icount)      = ( short )dstr.p.TFREQ;
    *(noise_lev+*icount)  = ( short )dstr.noise_lev;
    *(noise_lag0+*icount) = ( short )dstr.noise_lag0;
    *(noise_vel+*icount)  = ( short )dstr.noise_vel;
    *(atten+*icount)      = ( short )dstr.p.ATTEN;

/* Load fit data into arrays */
    for( i=0; i<=MAX_RANGE-1; i++ ){

      jndx = i+*icount*MAX_RANGE;
      *(a1+jndx)      = 10000.;
      *(a2+jndx)      = 10000.;
      *(a3+jndx)      = 10000.;
      *(grnscat+jndx) = dstr.gscat[i];

      if( pval1 == PWR_LAG0 ) *(a1+jndx) = ( float )dstr.pwr_lag0[i];
      if( pval2 == PWR_LAG0 ) *(a2+jndx) = ( float )dstr.pwr_lag0[i];
      if( pval3 == PWR_LAG0 ) *(a3+jndx) = ( float )dstr.pwr_lag0[i];

      if( dstr.qflag[i] == 1 ){

	switch (pval1){

	case PWR_L: 
	  *(a1+jndx) = ( float )dstr.pwr_l[i];  
	  break;

	case PWR_L_ERR: 
	  *(a1+jndx) = ( float )dstr.pwr_l_err[i];
	  break;
	  
	case PWR_S: 
	  *(a1+jndx) = ( float )dstr.pwr_s[i];
	  break;
	  
	case PWR_S_ERR: 
	  *(a1+jndx) = ( float )dstr.pwr_s_err[i];
	  break;

	case VEL: 
	  *(a1+jndx) = ( float )dstr.vel[i];
	  break;

	case VEL_ERR: 
	  *(a1+jndx) = ( float )dstr.vel_err[i];
	  break;

	case WIDTH_L: 
	  *(a1+jndx) = ( float )dstr.width_l[i];
	  break;

	case WIDTH_L_ERR: 
	  *(a1+jndx) = ( float )dstr.width_l_err[i];
	  break;

	case WIDTH_S: 
	  *(a1+jndx) = ( float )dstr.width_s[i];
	  break;

	case WIDTH_S_ERR: 
	  *(a1+jndx) = ( float )dstr.width_s_err[i];
	  break;
	    
	case STND_DEV_L: 
	  *(a1+jndx) = ( float )dstr.stnd_dev_l[i];
	  break;

	case STND_DEV_S: 
	  *(a1+jndx) = ( float )dstr.stnd_dev_s[i];
	  break;

	case STND_DEV_PHI: 
	  *(a1+jndx) = ( float )dstr.stnd_dev_phi[i];
	  break;

	case NUM_LAGS: 
	  *(a1+jndx) = ( float )dstr.num_lags[i];
	  break;
	}

	switch (pval2){

	case PWR_L: 
	  *(a2+jndx) = ( float )dstr.pwr_l[i];  
	  break;

	case PWR_L_ERR: 
	  *(a2+jndx) = ( float )dstr.pwr_l_err[i];
	  break;
	  
	case PWR_S: 
	  *(a2+jndx) = ( float )dstr.pwr_s[i];
	  break;
	  
	case PWR_S_ERR: 
	  *(a2+jndx) = ( float )dstr.pwr_s_err[i];
	  break;

	case VEL: 
	  *(a2+jndx) = ( float )dstr.vel[i];
	  break;

	case VEL_ERR: 
	  *(a2+jndx) = ( float )dstr.vel_err[i];
	  break;

	case WIDTH_L: 
	  *(a2+jndx) = ( float )dstr.width_l[i];
	  break;

	case WIDTH_L_ERR: 
	  *(a2+jndx) = ( float )dstr.width_l_err[i];
	  break;

	case WIDTH_S: 
	  *(a2+jndx) = ( float )dstr.width_s[i];
	  break;

	case WIDTH_S_ERR: 
	  *(a2+jndx) = ( float )dstr.width_s_err[i];
	  break;
	    
	case STND_DEV_L: 
	  *(a2+jndx) = ( float )dstr.stnd_dev_l[i];
	  break;

	case STND_DEV_S: 
	  *(a2+jndx) = ( float )dstr.stnd_dev_s[i];
	  break;

	case STND_DEV_PHI: 
	  *(a2+jndx) = ( float )dstr.stnd_dev_phi[i];
	  break;

	case NUM_LAGS: 
	  *(a2+jndx) = ( float )dstr.num_lags[i];
	  break;
	}

	switch (pval3){

	case PWR_L: 
	  *(a3+jndx) = ( float )dstr.pwr_l[i];  
	  break;

	case PWR_L_ERR: 
	  *(a3+jndx) = ( float )dstr.pwr_l_err[i];
	  break;
	  
	case PWR_S: 
	  *(a3+jndx) = ( float )dstr.pwr_s[i];
	  break;
	  
	case PWR_S_ERR: 
	  *(a3+jndx) = ( float )dstr.pwr_s_err[i];
	  break;

	case VEL: 
	  *(a3+jndx) = ( float )dstr.vel[i];
	  break;

	case VEL_ERR: 
	  *(a3+jndx) = ( float )dstr.vel_err[i];
	  break;

	case WIDTH_L: 
	  *(a3+jndx) = ( float )dstr.width_l[i];
	  break;

	case WIDTH_L_ERR: 
	  *(a3+jndx) = ( float )dstr.width_l_err[i];
	  break;

	case WIDTH_S: 
	  *(a3+jndx) = ( float )dstr.width_s[i];
	  break;

	case WIDTH_S_ERR: 
	  *(a3+jndx) = ( float )dstr.width_s_err[i];
	  break;
	    
	case STND_DEV_L: 
	  *(a3+jndx) = ( float )dstr.stnd_dev_l[i];
	  break;

	case STND_DEV_S: 
	  *(a3+jndx) = ( float )dstr.stnd_dev_s[i];
	  break;

	case STND_DEV_PHI: 
	  *(a3+jndx) = ( float )dstr.stnd_dev_phi[i];
	  break;

	case NUM_LAGS: 
	  *(a3+jndx) = ( float )dstr.num_lags[i];
	  break;
	}
      }

      if( dstr.x_qflag[i] == 1 ){

	switch (pval1){

	case X_PWR_L: 
	  *(a1+jndx) = ( float )dstr.x_pwr_l[i];
	  break;

	case X_PWR_L_ERR: 
	  *(a1+jndx) = ( float )dstr.x_pwr_l_err[i];
	  break;

	case X_PWR_S: 
	  *(a1+jndx) = ( float )dstr.x_pwr_s[i];
	  break;

	case X_PWR_S_ERR: 
	  *(a1+jndx) = ( float )dstr.x_pwr_s_err[i];
	  break;

	case X_VEL: 
	  *(a1+jndx) = ( float )dstr.x_vel[i];
	  break;

	case X_VEL_ERR: 
	  *(a1+jndx) = ( float )dstr.x_vel_err[i];
	  break;

	case X_WIDTH_L: 
	  *(a1+jndx) = ( float )dstr.x_width_l[i];
	  break;

	case X_WIDTH_L_ERR: 
	  *(a1+jndx) = ( float )dstr.x_width_l_err[i];
	  break;

	case X_WIDTH_S: 
	  *(a1+jndx) = ( float )dstr.x_width_s[i];
	  break;

	case X_WIDTH_S_ERR: 
	  *(a1+jndx) = ( float )dstr.x_width_s_err[i];
	  break;

	case PHI0: 
	  *(a1+jndx) = ( float )dstr.phi0[i];
	  break;
	  
	case PHI0_ERR: 
	  *(a1+jndx) = ( float )dstr.phi0_err[i];
	  break;

	case ELEV: 
	  *(a1+jndx) = ( float )dstr.elev[i];
	  break;

	case ELEV_LOW: 
	  *(a1+jndx) = ( float )dstr.elev_low[i];
	  break;
	  
	case ELEV_HIGH: 
	  *(a1+jndx) = ( float )dstr.elev_high[i];
	  break;
	}

	switch (pval2){

	case X_PWR_L: 
	  *(a2+jndx) = ( float )dstr.x_pwr_l[i];
	  break;

	case X_PWR_L_ERR: 
	  *(a2+jndx) = ( float )dstr.x_pwr_l_err[i];
	  break;

	case X_PWR_S: 
	  *(a2+jndx) = ( float )dstr.x_pwr_s[i];
	  break;

	case X_PWR_S_ERR: 
	  *(a2+jndx) = ( float )dstr.x_pwr_s_err[i];
	  break;

	case X_VEL: 
	  *(a2+jndx) = ( float )dstr.x_vel[i];
	  break;

	case X_VEL_ERR: 
	  *(a2+jndx) = ( float )dstr.x_vel_err[i];
	  break;

	case X_WIDTH_L: 
	  *(a2+jndx) = ( float )dstr.x_width_l[i];
	  break;

	case X_WIDTH_L_ERR: 
	  *(a2+jndx) = ( float )dstr.x_width_l_err[i];
	  break;

	case X_WIDTH_S: 
	  *(a2+jndx) = ( float )dstr.x_width_s[i];
	  break;

	case X_WIDTH_S_ERR: 
	  *(a2+jndx) = ( float )dstr.x_width_s_err[i];
	  break;

	case PHI0: 
	  *(a2+jndx) = ( float )dstr.phi0[i];
	  break;
	  
	case PHI0_ERR: 
	  *(a2+jndx) = ( float )dstr.phi0_err[i];
	  break;

	case ELEV: 
	  *(a2+jndx) = ( float )dstr.elev[i];
	  break;

	case ELEV_LOW: 
	  *(a2+jndx) = ( float )dstr.elev_low[i];
	  break;
	  
	case ELEV_HIGH: 
	  *(a2+jndx) = ( float )dstr.elev_high[i];
	  break;
	}

	switch (pval3){

	case X_PWR_L: 
	  *(a3+jndx) = ( float )dstr.x_pwr_l[i];
	  break;

	case X_PWR_L_ERR: 
	  *(a3+jndx) = ( float )dstr.x_pwr_l_err[i];
	  break;

	case X_PWR_S: 
	  *(a3+jndx) = ( float )dstr.x_pwr_s[i];
	  break;

	case X_PWR_S_ERR: 
	  *(a3+jndx) = ( float )dstr.x_pwr_s_err[i];
	  break;

	case X_VEL: 
	  *(a3+jndx) = ( float )dstr.x_vel[i];
	  break;

	case X_VEL_ERR: 
	  *(a3+jndx) = ( float )dstr.x_vel_err[i];
	  break;

	case X_WIDTH_L: 
	  *(a3+jndx) = ( float )dstr.x_width_l[i];
	  break;

	case X_WIDTH_L_ERR: 
	  *(a3+jndx) = ( float )dstr.x_width_l_err[i];
	  break;

	case X_WIDTH_S: 
	  *(a3+jndx) = ( float )dstr.x_width_s[i];
	  break;

	case X_WIDTH_S_ERR: 
	  *(a3+jndx) = ( float )dstr.x_width_s_err[i];
	  break;

	case PHI0: 
	  *(a3+jndx) = ( float )dstr.phi0[i];
	  break;
	  
	case PHI0_ERR: 
	  *(a3+jndx) = ( float )dstr.phi0_err[i];
	  break;

	case ELEV: 
	  *(a3+jndx) = ( float )dstr.elev[i];
	  break;

	case ELEV_LOW: 
	  *(a3+jndx) = ( float )dstr.elev_low[i];
	  break;
	  
	case ELEV_HIGH: 
	  *(a3+jndx) = ( float )dstr.elev_high[i];
	  break;
	}	  
	}
      }
    }
 }

/* Increment *icount so it gives the number of scans read, not *
 * the index of the last array position                        */
  ++(*icount);

  status = fit_close(ifileptr);
  return 1;
}


