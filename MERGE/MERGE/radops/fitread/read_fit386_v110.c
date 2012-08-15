/* this routine reads the radops 386 version 1.1 and 1.2 fit files */

/* 
   read_fit386_v110 reads a logical record from a FIT file
   the FIT file must have been produced by FITACF 386 version 1.1 to 1.3

   it has 2 arguments:
   fp = pointer to FIT_FILE structure = pointer to the data base which
        contains the offset to index and fit files, time etc. 
   fd = pointer to the FIT_DATA structure = pointer to the data

   it returns EOK  : if there is no error
   and        error code : otherwise
*/

/*
$Log:	read_fit386_v110.c,v $
 * Revision 1.2  94/09/06  10:30:47  10:30:47  baker (Kile Baker S1G)
 * removed path portionof includes
 * 
 * Revision 1.1  1993/08/20  20:42:55  baker
 * Initial revision
 *
*/

#include <stdio.h> 
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "fitfile.h"
#include "fit_errno.h"
#include "endian.h"

char read_fit386_v110_rev[] = {"$Revision: 1.2 $"};

int read_fit386_v110( FIT_FILE *fp, struct FIT_DATA *fd )
{
   union INX_REC  inx_rec;
   union FIT_REC fit_rec;   
   int iostat;
   int i;
   int j;
   int nsel;
   int nd;
   int np;
   int k;


   /* read the index record and the first of fit record */
   iostat = read(fp->inxdes,&inx_rec,sizeof(inx_rec));
   if(iostat == -1) {
     perror("I/O error reading index file");
     return (iostat);
   }
   else if (iostat == 0) return (-1);
   else if (iostat != sizeof(inx_rec)) {
      printf("I/O error while reading index file. Bytes read = %d\n",iostat);
      return (-1);
   } 
   fp->inx_offset = fp->inx_offset + sizeof(inx_rec);


   iostat = read(fp->fitdes,&fit_rec.v11_r0,sizeof(fit_rec.v11_r0));
   if(iostat != sizeof(fit_rec.v11_r0)) {
      perror("read fit file:");
      return (errno);
   } 
   fp->fit_offset = fp->fit_offset + sizeof(fit_rec.v11_r0);

   /* check the time */
   if(inx_rec.index.rtime != fit_rec.v11_r0.rtime) {
      printf("inconsistent time\n");
      return(RDFIT_INV_PARM_TIME);
   }

   if(fit_rec.v11_r0.rrn != 0) {
      printf("not a parameter record\n");
      return(RDFIT_INV_PARM_RECORD);
   }

   /* move the parameters to the data structure */
   fd->rec_time = fit_rec.v11_r0.rtime;
   memcpy(&(fd->p),&fit_rec.v11_r0.r_parm,sizeof(fit_rec.v11_r0.r_parm));
   if(fd->p.NOISE > 32767)
      fd->p.NOISE = 32767;

   /* copy the pulse pattern */
   for(i=0;i < V11_PULSE_PAT_LEN;++i)   
      fd->PULSE_PATTERN[i] = fit_rec.v11_r0.r_ppat[i];

   /* copy lag table */
   for(j=0; j < 2; ++j)
      for(i=0;i < V11_LAG_TAB_LEN; ++i)
         fd->LAG_TABLE[j][i] = fit_rec.v11_r0.r_lagtable[j][i];

   /* copy comment buffer */
   memcpy(fd->COMBF,fit_rec.v11_r0.r_combf,COMBF_SIZE);

   /* initialize the slist and power arrays */
   memset(fd->slist,0,sizeof(fd->slist));
   memset(fd->pwr_lag0,-50,sizeof(fd->pwr_lag0));

   /* set the value of the common program to 0 and set the value
      of scan to 1 if beam number = 0
   */
   fd->p.CP = 0;
   if(fd->p.BMNUM == 0)
      fd->p.SCAN = 1;
   else
      fd->p.SCAN = 0;


   /* set the value of nsel */
   nsel = 0;
   while((fit_rec.v11_r0.r_slist[nsel] > 0) && (nsel < fd->p.NRANG)) {
      fd->slist[nsel] = fit_rec.v11_r0.r_slist[nsel];
      ++nsel;
   }


   fd->nsel = nsel;

   /* the following statement corrects a bug in fitacf which causes
      a data record to be written even if there were NO succesfull
      fits
   */
   if((nsel == 0) && (inx_rec.index.nrec > 1)) {
      for(i=2;i <= inx_rec.index.nrec;++i) {
         iostat = read(fp->fitdes,&fit_rec.r1,sizeof(fit_rec.r1));
         if(iostat != sizeof(fit_rec.r1)) {
            perror("read fit file");
            return(errno);
         }
         fp->fit_offset = fp->fit_offset + sizeof(fit_rec.r1);
      }
      return(EOK);
   }

   /* transfer the noise value */
   fd->noise_lev = fit_rec.v11_r0.r_noise;
   fd->noise_lag0 = fit_rec.v11_r0.r_noise_lag0;
   fd->noise_vel = fit_rec.v11_r0.r_noise_vel;

   /* transfer the lag0 power */
   for(i=0; i < fd->p.NRANG; ++i)
      fd->pwr_lag0[i] = (double) ((float)(fit_rec.v11_r0.r_pwr_0[i])/100.);

   /* init qflag, xqflag and num_lags */
   for(i=0;i < MAX_RANGE;++i) {
      fd->qflag[i] = 0;
      fd->x_qflag[i] = 0;
      fd->num_lags[i] = 0;
   }


   /* read the data records and transfer to user's buffer */
   nd = 1;
   while( nd <= nsel) {

      /* read the acfs */
      np = 0;          
      iostat = read(fp->fitdes,&fit_rec.r1,sizeof(fit_rec.r1));
      if(iostat != sizeof(fit_rec.r1)) {
         perror("read fit file");
         return(errno);
      }    
      fp->fit_offset = fp->fit_offset + sizeof(fit_rec.r1);
   
      if((fit_rec.r1.rrn == 0) || (fit_rec.r1.r_xflag)) {
         printf("invalid record type\n");
         return(RDFIT_INV_RECORD);
      }

      if(inx_rec.index.rtime != fit_rec.r1.rtime) {
         printf("invalid time\n");
         return(RDFIT_INV_TIME);
      }


      /* transfer the acfs */
      while((np < 25) && (nd <= nsel)) {
         k = fit_rec.r1.r_range_list[np];
         if( k != fd->slist[nd]) {
            printf("range problem\n");
            return(RDFIT_INV_RANGE);
         }
         fd->qflag[k] = fit_rec.r1.r_qflag[np];
         fd->pwr_l[k] = (double) ((float)(fit_rec.r1.r_pwr_l[np])/100.);
         fd->pwr_s[k] = (double) ((float)(fit_rec.r1.r_pwr_s[np])/100.);
         fd->pwr_l_err[k]=(double)((float)(fit_rec.r1.r_pwr_l_err[np])/100.);
         fd->pwr_s_err[k]=(double)((float)(fit_rec.r1.r_pwr_s_err[np])/100.);
         fd->vel[k] = (double)((float)(fit_rec.r1.r_vel[np])/10.);
         fd->vel_err[k] = (double)((float)(fit_rec.r1.r_vel_err[np])/10.);
         fd->width_l[k] = (double)((float)(fit_rec.r1.r_w_l[np])/10.);
         fd->width_l_err[k] = (double)((float)(fit_rec.r1.r_w_l_err[np])/10.);
         fd->width_s[k] = (double)((float)(fit_rec.r1.r_w_s[np])/10.);
         fd->width_s_err[k]=(double)((float)(fit_rec.r1.r_w_s_err[np])/10.);
         fd->stnd_dev_l[k]=(double)((float)(fit_rec.r1.r_sdev_l[np])/1000.);
         fd->stnd_dev_s[k]=(double)((float)(fit_rec.r1.r_sdev_s[np])/1000.);
         if(fit_rec.r1.r_sdev_phi[np] == 32767)
            fd->stnd_dev_phi[k] = 32767.;
         else
            fd->stnd_dev_phi[k] = 
                        (double)((float)(fit_rec.r1.r_sdev_phi[np])/100.);
         fd->gscat[k] = fit_rec.r1.r_gscat[np];
         ++np;
         ++nd;
      } /* while((np... */

   } /* while(nd <= nsel) */

   /* transfer the xcfs */
   nd = 1;
   if(fd->p.XCF != 0) {
      while(nd <= nsel) {

         /* read the xcfs */
         np = 0;
         iostat = read(fp->fitdes,&fit_rec.r1,sizeof(fit_rec.r1));
         if(iostat != sizeof(fit_rec.r1)) {
            perror("read fit file");
            return(errno);
         }    
         fp->fit_offset = fp->fit_offset + sizeof(fit_rec.r1);

         if((fit_rec.r1.rrn == 0) || (!fit_rec.r1.r_xflag)) {
            printf("invalid record type\n");
            return(RDFIT_INV_RECORD);
         }

         if(inx_rec.index.rtime != fit_rec.r1.rtime) {
            printf("invalid time\n");
            return(RDFIT_INV_TIME);
         }
 

         /* transfer the xcfs */
         while((np < 25) && (nd <= nsel)) {
            k = fit_rec.r1.r_range_list[np];
            if( k != fd->slist[nd]) {
               printf("range problem\n");
               return(RDFIT_INV_RANGE);
            }
            fd->x_qflag[k] = fit_rec.r1.r_qflag[np];
            fd->x_pwr_l[k] = (double) ((float)(fit_rec.r1.r_pwr_l[np])/100.);
            fd->x_pwr_s[k] = (double) ((float)(fit_rec.r1.r_pwr_s[np])/100.);
            fd->x_pwr_l_err[k]=
                           (double)((float)(fit_rec.r1.r_pwr_l_err[np])/100.);
            fd->x_pwr_s_err[k]=
                           (double)((float)(fit_rec.r1.r_pwr_s_err[np])/100.);
            fd->x_vel[k] = (double)((float)(fit_rec.r1.r_vel[np])/10.);
            fd->x_vel_err[k] = (double)((float)(fit_rec.r1.r_vel_err[np])/10.);
            fd->x_width_l[k] = (double)((float)(fit_rec.r1.r_w_l[np])/10.);
            fd->x_width_l_err[k] = 
                           (double)((float)(fit_rec.r1.r_w_l_err[np])/10.);
            fd->x_width_s[k] = (double)((float)(fit_rec.r1.r_w_s[np])/10.);
            fd->x_width_s_err[k]=
                           (double)((float)(fit_rec.r1.r_w_s_err[np])/10.);
            fd->phi0[k] = (double)((float)(fit_rec.r1.r_phi0[np])/100.);
            fd->phi0_err[k] = (double)((float)(fit_rec.r1.r_phi0_err[np])/100.);
            fd->elev[k] = (double)((float)(fit_rec.r1.r_elev[np])/100.);
            fd->elev_low[k]=(double)((float)(fit_rec.r1.r_el_low[np])/100.);
            fd->elev_high[k]=(double)((float)(fit_rec.r1.r_el_high[np])/100.);
            fd->x_stnd_dev_l[k]=
                            (double)((float)(fit_rec.r1.r_sdev_l[np])/1000.);
            fd->x_stnd_dev_s[k]=
                            (double)((float)(fit_rec.r1.r_sdev_s[np])/1000.);
            if(fit_rec.r1.r_sdev_phi[np] == 32767)
               fd->x_stnd_dev_phi[k] = 32767.;
            else
               fd->x_stnd_dev_phi[k] = 
                        (double)((float)(fit_rec.r1.r_sdev_phi[np])/100.);
            fd->gscat[k] = fit_rec.r1.r_gscat[np];
            ++np;
            ++nd;
         } /* while((np... */

      } /* while(nd <= nsel */

   } /* if */

   /* everything is done and if it reaches this point it means that everyting
      is ok 
   */

   return(EOK);


} /* int read_fit386_v1_1 */

