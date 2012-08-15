/*  This routine reads a block of data from a FIT file into arrays that
    are passed to IDL for plotting.  Three parameters are read in, defined by
    variables in argv[3],argv[4], and argv[5]
*/
/*
$Log: block_read.c,v $
 * Revision 1.9  21-JAN-1999 Dieter Andre
 * Take out printf statements
 *
 * Revision 1.8  1995/11/15  20:31:39  bristow
 * added array gpos[3][2][3][2][2][3] so that call to
 * gscat_pos does not bomb.
 *
 * Revision 1.7  1995/09/05  16:58:00  bristow
 * Added code to generate coordinates of groundscatter
 * mapped to the reflection point.
 *
 * Revision 1.6  1995/06/20  12:48:02  bristow
 * Changed call to rbpos so that there is only
 * one call for entire beam-gate array.
 *
 * Revision 1.5  1995/05/31  20:48:05  bristow
 * changed condition for first value of lagfr and smsep.
 * this fixed bug that if lagfr changed during first scan
 * the positions were wrong.
 *
 * Revision 1.4  1995/03/06  19:33:55  bristow
 * Changed call to rbpos so that the first call uses index 1.
 *
 * Revision 1.3  1995/01/17  14:37:50  bristow
 * Revised so the time was begining of scan.
 *
 * Revision 1.2  1995/01/06  14:02:35  bristow
 * Initial revision
 *
*/
#include <stdio.h>

#include "fitfile.h"
#include "endian.h"

#define N_BMS 16
#define N_CELL N_BMS*MAX_RANGE


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

typedef struct {
  unsigned short slen;
  short stype;
  char *s;
} STRING;

extern float local_hfr_loc[76][17][3];

long block_read(argc,argv)
     int argc;
     void *argv[];
{
  float posc[2][MAX_RANGE+1][N_BMS+1];
  float pos[8],gpos[2][2][3],height;
  float *a1,*a2,*a3;
  long st,et,ct,*time,jndx;
  short lagfrl,smsepl,icnt,*icount,*lagfr, count;
  short *noise_lev,*noise_lag0,*noise_vel,*atten;
  short *smsep,*tfreq,*grnscat,mgflag,bmnum;
  short yr,mo,dy,hr,mn,sc,*st_id,err,scan;
  short i,ibm,igate,items,j,iflag;
  int status;
  int pval1,pval2,pval3;
  STRING strptr1,strptr2,strptr3;
  FIT_FILE *ifileptr;
  struct FIT_DATA dstr;
  FILE *u1,*u2, *u3, *u4;
  short int scan_type;
  
  st      = *(long *) argv[0];
  et      = *(long *) argv[1];
  ifileptr= *(FIT_FILE **) argv[2];
  strptr1  = *(STRING *) argv[3];
  strptr2  = *(STRING *) argv[4];
  strptr3  = *(STRING *) argv[5];
  a1      = argv[6];
  a2      = argv[7];
  a3      = argv[8];
  time    = argv[9];
  lagfr   = argv[10];
  smsep   = argv[11];
  tfreq   = argv[12];
  grnscat = argv[13];
  st_id   = (short *) argv[14];
  icount  = (short *) argv[15];
  noise_lev = argv[16];
  noise_lag0= argv[17];
  noise_vel = argv[18];
  atten     = argv[19];
  scan_type = * (short int *) argv[20];
  
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

  if( (u1 = fopen("posg.dat","w")) == NULL )printf("CAN'T OPEN POSG FILE\n");
  if( (u2 = fopen("posm.dat","w")) == NULL )printf("CAN'T OPEN POSM FILE\n");
  if( (u3 = fopen("gs_posg.dat","w")) == NULL )printf("CAN'T OPEN POSG FILE\n");
  if( (u4 = fopen("gs_posm.dat","w")) == NULL )printf("CAN'T OPEN POSM FILE\n");
  
  *icount = 0;
  ct = 0;
  lagfrl = 0;
  smsepl = 0;
  iflag = 0;
  while( ct < et ){
    status = read_fit(ifileptr,&dstr);
    /* !!! temporary patch to avoid range reduction due to Kap phase coding DA 13-JUN-1997 */
    dstr.p.SMSEP= dstr.p.RSEP* ( 20.0/ 3.0);
    if( iflag == 0 ){
      time[*icount] = (long)(dstr.p.HOUR*3600+dstr.p.MINUT*60+dstr.p.SEC);
      lagfr[*icount] = dstr.p.LAGFR;
      smsep[*icount] = dstr.p.SMSEP;
      iflag = 1;
    }
    if( status != 0 ) {
	++count;
	printf("STATUS NUMBER: %d   COUNT:  %d\n",status,count);
	if( status == -1 ){
	    printf("END OF FILE REACHED BEFORE END OF INTERVAL\n");
	    fclose(u1);
	    fclose(u2);
	    status = fit_close(ifileptr);
	    return 0;
	}
	if( count >= 10 ){
	    status = -1;
	    break;
	}
	else continue;
    }
    

    count = 0;

    yr = dstr.p.YEAR;
    mo = dstr.p.MONTH;
    dy = dstr.p.DAY;
    hr = dstr.p.HOUR;
    mn = dstr.p.MINUT;
    sc = dstr.p.SEC;

    ct = cnv_mdhms_sec(&yr,&mo,&dy,&hr,&mn,&sc);

    if( dstr.p.SCAN < 0 && scan_type >= 0 ) continue;
    if (dstr.p.SCAN >= 0 && scan_type < 0 ) continue;

    *st_id = dstr.p.ST_ID;
    bmnum = dstr.p.BMNUM;

    if( (dstr.p.SCAN >0 && scan_type >= 0) || (scan_type < 0) ){
      ++(*icount);
      time[*icount]  = (long)(dstr.p.HOUR*3600+dstr.p.MINUT*60+dstr.p.SEC);
      lagfr[*icount] = dstr.p.LAGFR;
      smsep[*icount] = dstr.p.SMSEP;
    }
      *(tfreq+bmnum+*icount*N_BMS) = ( short )dstr.p.TFREQ;
      *(noise_lev+bmnum+*icount*N_BMS)  = ( short )dstr.noise_lev;
      *(noise_lag0+bmnum+*icount*N_BMS) = ( short )dstr.noise_lag0;
      *(noise_vel+bmnum+*icount*N_BMS)  = ( short )dstr.noise_vel;
      *(atten+bmnum+*icount*N_BMS)  = ( short )dstr.p.ATTEN;

    if( lagfrl != dstr.p.LAGFR ){
      /* printf("change in lagfr %d %d %d %d\n",dstr.p.LAGFR,lagfrl,ct,smsepl);  DA 21-JAN-1999 */
      smsepl=dstr.p.SMSEP;
      lagfrl=dstr.p.LAGFR;
      mgflag=0;
      igate = 1;
      ibm = 0;
      rbpos(&igate,&ibm,&lagfrl,&smsepl,&height,pos,st_id,&mgflag,&err);
      for( ibm=0; ibm<=16; ibm++ ){
	  for( igate=0; igate<=75; igate++ ){
	      posc[0][igate][ibm]=local_hfr_loc[igate][ibm][0];
	      posc[1][igate][ibm]=local_hfr_loc[igate][ibm][1];
	  }
      }

      items = fwrite( posc, sizeof(posc[0][0][0]),(17*76*2),u1);
      if( items != (17*76*2) )printf("PROBLEM WRITING POSG\n");
      mgflag=1;
      igate = 1;
      ibm = 0;
      rbpos(&igate,&ibm,&lagfrl,&smsepl,&height,pos,st_id,&mgflag,&err);
      for( ibm=0; ibm<=16; ibm++ ){
	  for( igate=0; igate<=75; igate++ ){
	      posc[0][igate][ibm]=local_hfr_loc[igate][ibm][0];
	      posc[1][igate][ibm]=local_hfr_loc[igate][ibm][1];
	}
      }
      items = fwrite( posc, sizeof(posc[0][0][0]),(17*76*2),u2);
      if( items != (17*76*2) )printf("PROBLEM WRITING POSM\n");

      mgflag=0;
      igate = 1;
      ibm = 0;
      gscat_pos(&igate,&ibm,&lagfrl,&smsepl,&height,gpos,st_id,&mgflag,&err);
      for( ibm=0; ibm<=16; ibm++ ){
	  for( igate=0; igate<=75; igate++ ){
	      posc[0][igate][ibm]=local_hfr_loc[igate][ibm][0];
	      posc[1][igate][ibm]=local_hfr_loc[igate][ibm][1];
	  }
      }

      items = fwrite( posc, sizeof(posc[0][0][0]),(17*76*2),u3);
      if( items != (17*76*2) )printf("PROBLEM WRITING POSG\n");
      mgflag=1;
      igate = 1;
      ibm = 0;
      gscat_pos(&igate,&ibm,&lagfrl,&smsepl,&height,gpos,st_id,&mgflag,&err);
      for( ibm=0; ibm<=16; ibm++ ){
	  for( igate=0; igate<=75; igate++ ){
	      posc[0][igate][ibm]=local_hfr_loc[igate][ibm][0];
	      posc[1][igate][ibm]=local_hfr_loc[igate][ibm][1];
	}
      }
      items = fwrite( posc, sizeof(posc[0][0][0]),(17*76*2),u4);
      if( items != (17*76*2) )printf("PROBLEM WRITING POSM\n");
    }
    if( smsepl != dstr.p.SMSEP ){
      /* printf("change in smsep %d %d %d %d\n",dstr.p.SMSEP,smsepl,ct,lagfrl); DA 21-JAN-1999 */
      smsepl=dstr.p.SMSEP;
      lagfrl=dstr.p.LAGFR;
      mgflag=0;
      igate = 1;
      ibm = 0;
      rbpos(&igate,&ibm,&lagfrl,&smsepl,&height,pos,st_id,&mgflag,&err);
      for( ibm=0; ibm<=16; ibm++ ){
	  for( igate=0; igate<=75; igate++ ){
	      posc[0][igate][ibm]=local_hfr_loc[igate][ibm][0];
	      posc[1][igate][ibm]=local_hfr_loc[igate][ibm][1];
	}
      }
      items = fwrite( posc, sizeof(posc[0][0][0]),(17*76*2),u1);
      if( items != (17*76*2) )printf("PROBLEM WRITING POSG\n");
      mgflag=1;
      igate = 1;
      ibm = 0;
      rbpos(&igate,&ibm,&lagfrl,&smsepl,&height,pos,st_id,&mgflag,&err);
      for( ibm=0; ibm<=16; ibm++ ){
	  for( igate=0; igate<=75; igate++ ){
	      posc[0][igate][ibm]=local_hfr_loc[igate][ibm][0];
	      posc[1][igate][ibm]=local_hfr_loc[igate][ibm][1];
	}
      }
      items = fwrite( posc, sizeof(posc[0][0][0]),(17*76*2),u2);
      if( items != (17*76*2) )printf("PROBLEM WRITING POSM\n");
      mgflag=0;
      igate = 1;
      ibm = 0;
      gscat_pos(&igate,&ibm,&lagfrl,&smsepl,&height,gpos,st_id,&mgflag,&err);
      for( ibm=0; ibm<=16; ibm++ ){
	  for( igate=0; igate<=75; igate++ ){
	      posc[0][igate][ibm]=local_hfr_loc[igate][ibm][0];
	      posc[1][igate][ibm]=local_hfr_loc[igate][ibm][1];
	  }
      }

      items = fwrite( posc, sizeof(posc[0][0][0]),(17*76*2),u3);
      if( items != (17*76*2) )printf("PROBLEM WRITING POSG\n");
      mgflag=1;
      igate = 1;
      ibm = 0;
      gscat_pos(&igate,&ibm,&lagfrl,&smsepl,&height,gpos,st_id,&mgflag,&err);
      for( ibm=0; ibm<=16; ibm++ ){
	  for( igate=0; igate<=75; igate++ ){
	      posc[0][igate][ibm]=local_hfr_loc[igate][ibm][0];
	      posc[1][igate][ibm]=local_hfr_loc[igate][ibm][1];
	}
      }
      items = fwrite( posc, sizeof(posc[0][0][0]),(17*76*2),u4);
      if( items != (17*76*2) )printf("PROBLEM WRITING POSM\n");

    }
    for( i=0; i<=MAX_RANGE-1; i++ ){
      jndx = i+bmnum*MAX_RANGE+*icount*N_CELL;
      *(a1+jndx) = 10000.;
      *(a2+jndx) = 10000.;
      *(a3+jndx) = 10000.;
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

	case NUM_LAGS: 
	  *(a1+jndx) = ( float )dstr.num_lags[i];
	  break;

	  
	  printf("PARAMETER NOT FOUND\n");
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
	  
	case NUM_LAGS: 
	  *(a2+jndx) = ( float )dstr.num_lags[i];
	  break;
	  
	  
	  printf("PARAMETER NOT FOUND\n");
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
	  
	case NUM_LAGS: 
	  *(a3+jndx) = ( float )dstr.num_lags[i];
	  break;
	  
	  
	  printf("PARAMETER NOT FOUND\n");
	}
      }
    }
  }
  fclose(u1);
  fclose(u2);
  status = fit_close(ifileptr);  
return 1;
}
