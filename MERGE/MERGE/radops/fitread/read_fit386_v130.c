/* this routine reads the radops 386 version 1.3 and above */

/* 
   read_fit386_v130 reads a logical record from a FIT file
   the FIT file must have been produced by FITACF 386 version 1.3 and above

   it has 2 arguments:
   fp = pointer to FIT_FILE structure = pointer to the data base which
        contains the offset to index and fit files, time etc. 
   fd = pointer to the FIT_DATA structure = pointer to the data

   it returns EOK  : if there is no error
   and        error code : otherwise
*/

/*
$Log:	read_fit386_v130.c,v $
 * Revision 1.8  94/09/06  10:31:14  10:31:14  baker (Kile Baker S1G)
 * removed path portion of includes\
 * 
 * Revision 1.7  1994/01/05  19:45:12  baker
 * fix the routine so that it can read multiple instances of 0 0 0 0,
 * returning -RDFIT_INV_INX each time.
 *
 * Revision 1.6  1994/01/05  19:01:12  baker
 * If INX file contains 0 0 0 0 we will return a code of -16 (RDFIT_INV_INX)
 * rather than EOF.  It will be up to the use to decide whether to attempt
 * another read operation or to give up.
 *
 * Revision 1.5  1994/01/05  18:44:54  baker
 * If index record continains 0 0 0 0  we will assume it is an end of file
 *
 * Revision 1.4  1994/01/05  18:28:33  baker
 * added even more printouts for error conditions
 *
 * Revision 1.3  1994/01/05  18:15:46  baker
 * added additional print out when errors occur doruiuring a read operation.
 *
 * Revision 1.2  1993/09/27  16:28:49  baker
 * The integer etst that is used in the endian test is now explicitly
 * declared to be a short (16-bit) integer rather than a default integer.
 *
 * Revision 1.1  1993/08/20  20:39:25  baker
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

char read_fit386_v130_rev[] = {"$Revision: 1.8 $"};

int read_fit386_v130( FIT_FILE *fp, struct FIT_DATA *fd )
{
   union INX_REC  inx_rec, inx_rec1;
   union FIT_REC fit_rec, fit_rec1;   
   int iostat;
   int i;
   int j;
   int nsel;
   int nd;
   int np;
   int k;
   int cp_num_lag;
   short int etst;

   /* read the index record and the first of fit record */
   iostat = read(fp->inxdes,&inx_rec1,sizeof(inx_rec));
   if(iostat == -1) {
     perror("I/O error reading index file");
     return (iostat);
   }
   else if ( iostat == 0) return (-1);
   else if (iostat != sizeof(inx_rec)) {
      printf ("I/O error while reading index file.  Bytes read = %d\n", iostat);
      return (-1);
   } 
   fp->inx_offset = fp->inx_offset + sizeof(inx_rec);

/*	if this is a big endian machine we have to swap bytes */
   if (endian(&etst) == 0)
      for (i=0; i<4; ++i) swab_dword(&inx_rec.iv[i], &inx_rec1.iv[i]);
   else memcpy(&inx_rec, &inx_rec1, sizeof(inx_rec));

/*	now check the index records offset value for consistency
	with what we expect */

   if ((inx_rec.index.recno - 1)*fp->fit_recl != fp->fit_offset) {
	printf("Inconsistent offsets in INX file and FIT file\n");
	printf("current offset of index file is: %d\n",fp->inx_offset);
	printf("current offset of fit file is:   %d\n",fp->fit_offset);
	printf("index record contains: %d %d %d %d\n",inx_rec1.iv[0],
	       inx_rec1.iv[1],inx_rec1.iv[2],inx_rec1.iv[3]);
        if (inx_rec.index.recno <= 0) {
	  printf("Calculated FIT offset < 0.  No SEEK will be done\n");
	  return RDFIT_INV_INX;
	}

	fp->fit_offset = (inx_rec.index.recno - 1)*fp->fit_recl;
	printf("calculated offset of fit file is: %d\n",fp->fit_offset);

	iostat = lseek(fp->fitdes, fp->fit_offset, SEEK_SET);
	if (iostat != 0) {
	  i = errno;
	  printf("Invalid seek to %ld\n",fp->fit_offset);
	  errno = i;
	  perror("READ_FIT386_v130");
	  return i;
	  }
	}

   iostat = read(fp->fitdes,&fit_rec1.v13_r0,sizeof(fit_rec));
   if(iostat != sizeof(fit_rec)) {
      i = errno;
      printf("Error reading FIT file. Return value from READ is %d\n",iostat);
      errno = i;
      perror("READ_FIT386_V130: ");
      return (i);
   } 

/*	again find out if we are on a big endian machine.  If so do the
	appropriate byte swapp ing */

   if (endian(&etst) == 0) swap_v13_r0(&fit_rec, &fit_rec1);
   else memcpy(&fit_rec, &fit_rec1, sizeof(fit_rec));


   fp->fit_offset = fp->fit_offset + sizeof(fit_rec);

   /* check the time */
   if(inx_rec.index.rtime != fit_rec.v13_r0.rtime) {
      printf("inconsistent time\n");
      return(RDFIT_INV_PARM_TIME);
   }

   if(fit_rec.v13_r0.rrn != 0) {
      printf("not a parameter record\n");
      return(RDFIT_INV_PARM_RECORD);
   }

   /* move the parameters to the data structure */
   fd->rec_time = fit_rec.v13_r0.rtime;
   memcpy(&(fd->p),&fit_rec.v13_r0.r_parm,sizeof(fit_rec.v13_r0.r_parm));
   if(fd->p.NOISE > 32767)
      fd->p.NOISE = 32767;

   /* copy the pulse pattern */
   for(i=0;i < PULSE_PAT_LEN;++i)   
      fd->PULSE_PATTERN[i] = fit_rec.v13_r0.r_ppat[i];

   /* copy lag table */
   for(j=0; j < 2; ++j)
      for(i=0;i < LAG_TAB_LEN; ++i)
         fd->LAG_TABLE[j][i] = fit_rec.v13_r0.r_lagtable[j][i];

   /* copy comment buffer */
   memcpy(fd->COMBF,fit_rec.v13_r0.r_combf,COMBF_SIZE);

   /* initialize the slist and power arrays */
   memset(fd->slist,0,sizeof(fd->slist));
   memset(fd->pwr_lag0,0,sizeof(fd->pwr_lag0));

   /* for interpreter version prior to 1.20 the parameters cp and
      scan were not defined
   */
   if((fd->p.REV.MAJOR <= 1) && (fd->p.REV.MINOR < 20)) {
      fd->p.CP=0;
      if(fd->p.BMNUM == 0) fd->p.SCAN = 1;
      else fd->p.SCAN = 0;
      }

   /* set the value of nsel */
   nsel = 0;
   while((fit_rec.v13_r0.r_slist[nsel] > 0) && (nsel < fd->p.NRANG)) {
      fd->slist[nsel] = fit_rec.v13_r0.r_slist[nsel];
      ++nsel;
   }


   /* starting with QNX version 1.70 and VAX version 1.30, the array
      num_lags was included in the data. This array gives the number of 
      good lags that were used for the fit at each range. For versions
      prior to this, this table will be set to 0
   */
   cp_num_lag = 1;
   if(strncmp(fp->source,"QNX",3) == 0)  
      if((fp->version[0] < '2') && (fp->version[2] < '7'))
         cp_num_lag = 0;
   
   if(cp_num_lag)
      for(i=0;i< MAX_RANGE;++i)
         fd->num_lags[i] = fit_rec.v13_r0.r_numlags[i];
   else
      memset(fd->num_lags,0,sizeof(fd->num_lags));

   fd->nsel = nsel;

   /* the following statement corrects a bug in fitacf which causes
      a data record to be written even if there were NO succesfull
      fits
   */
   if((nsel == 0) && (inx_rec.index.nrec > 1)) {
      for(i=2;i <= inx_rec.index.nrec;++i) {
         iostat = read(fp->fitdes,&fit_rec1.r1,sizeof(fit_rec));
         if(iostat != sizeof(fit_rec)) {
	    i = errno;
	    printf("Error reading FIT file.  Return value from READ is %d\n",
		   iostat);
	    errno = i;
            perror("READ_FIT386_V130: ");
            return(i);
         }
         fp->fit_offset = fp->fit_offset + sizeof(fit_rec);

	/* if this is a big endian machine do the swaps */

	if (endian(&etst) == 0) swap_rrn1(&fit_rec, &fit_rec1);
	else memcpy(&fit_rec, &fit_rec1, sizeof(fit_rec));
      }
      return(EOK);
   }

   /* transfer the noise value */
   fd->noise_lev = fit_rec.v13_r0.r_noise;
   fd->noise_lag0 = fit_rec.v13_r0.r_noise_lag0;
   fd->noise_vel = fit_rec.v13_r0.r_noise_vel;

   /* transfer the lag0 power */
   for(i=0; i < fd->p.NRANG; ++i)
      fd->pwr_lag0[i] = (double) ((float)(fit_rec.v13_r0.r_pwr_0[i])/100.);

   /* init qflag and xqflag */
   for(i=0;i < MAX_RANGE;++i) {
      fd->qflag[i] = 0;
      fd->x_qflag[i] = 0;
   }


   /* read the data records and transfer to user's buffer */
   nd = 0;
   while( nd < nsel) {

      /* read the acfs */
      np = 0;          
      iostat = read(fp->fitdes,&fit_rec1.r1,sizeof(fit_rec));
      if(iostat != sizeof(fit_rec)) {
	 i = errno;
	 printf("Error reading FIT file.  Return from READ is %d\n",iostat);
	 errno = i;
         perror("read fit file");
         return(i);
      }    
      fp->fit_offset = fp->fit_offset + sizeof(fit_rec);

      /* if this is a big endian machine do the swaps */

      if (endian(&etst) == 0) swap_rrn1(&fit_rec, &fit_rec1);
      else memcpy(&fit_rec, &fit_rec1, sizeof(fit_rec));
   
      if((fit_rec.r1.rrn == 0) || (fit_rec.r1.r_xflag)) {
         printf("invalid record type\n");
         return(RDFIT_INV_RECORD);
      }

      if(inx_rec.index.rtime != fit_rec.r1.rtime) {
         printf("invalid time\n");
         return(RDFIT_INV_TIME);
      }


      /* transfer the acfs */
      while((np < 25) && (nd < nsel)) {
         k = fit_rec.r1.r_range_list[np];
         if( k != fd->slist[nd]) {
            printf("range problem\n");
            return(RDFIT_INV_RANGE);
            }
	 --k;	/* decrement k since ranges run from 1 - 75 rather than 0 -74*/

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

   } /* while(nd < nsel) */

   /* transfer the xcfs */
   nd = 0;
   if(fd->p.XCF != 0) {
      while(nd < nsel) {

         /* read the xcfs */
         np = 0;
         iostat = read(fp->fitdes,&fit_rec1.r1,sizeof(fit_rec));
         if(iostat != sizeof(fit_rec)) {
            perror("read fit file");
            return(errno);
         }    
         fp->fit_offset = fp->fit_offset + sizeof(fit_rec);

      /* if this is a big endian machine do the swaps */

         if (endian(&etst) == 0) swap_rrn1(&fit_rec, &fit_rec1);
         else memcpy(&fit_rec, &fit_rec1, sizeof(fit_rec));
   

         if((fit_rec.r1.rrn == 0) || (!fit_rec.r1.r_xflag)) {
            printf("invalid record type\n");
            return(RDFIT_INV_RECORD);
         }

         if(inx_rec.index.rtime != fit_rec.r1.rtime) {
            printf("invalid time\n");
            return(RDFIT_INV_TIME);
         }
 

         /* transfer the xcfs */
         while((np < 25) && (nd < nsel)) {
            k = fit_rec.r1.r_range_list[np];
            if( k != fd->slist[nd]) {
               printf("range problem\n");
               return(RDFIT_INV_RANGE);
            }
	    --k;
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


} /* int read_fit386_v1_3 */
