/* find_fit_rec seeks the fit file for the correct location given the time
   it also positions the fit file at the time_found so that the next
   read to the fit file will in fact give the time_found record
   in addition, it positions the fp->inx_offset is set to the 
   find_fit_rec takes 3 arguments
      fp = pointer to structure FIT_FILE which contains info such as
           file descriptor for fit and inx file,version, fit_offset, inx_offset
           etc.
      time_req = the requested time in seconds since the beginning of the
                 year
      time_found = pointer to long int will contain the time found when
                   find_fit_rec returns 

   It returns
      EOK = found exact time
      F_FIT_REC_LT = requested time < start time
                     it positions the fit file to the beginning of file
      F_FIT_REC_INEXACT = inexact time found
                          it positions fit file such that 
                          requested time < time_found
      F_FIT_REC_GT = requested time > end time      
      RDFIT_INV_INX = the index file is invalid and must be reconstructed
      errno = for other error conditions i.e. read error, seek error etc.
*/
/*
$Log:	find_fit_rec.c,v $
 * Revision 1.6  96/07/02  15:50:09  15:50:09  baker (Kile Baker SRA)
 * There was a bug that caused the routine to fail if the 
 * requested time was exactly equal to the beginning time
 * of the file.  This has now beeThe routine would initialize the index
 * file to the header record of the index file instead of
 * the first data record of the index file.
 * 
 * Revision 1.5  96/03/08  16:03:35  16:03:35  baker (Kile Baker SRA)
 * Modified this routine to allow it to work properly when
 * a file spans a year boundary.
 * 
 * Revision 1.4  1996/02/21  21:12:28  baker
 * cleaned up the print statments and made them
 * all fprintf(stderr,... ) instead of printf to stdout.
 *
 * Revision 1.3  1996/02/21  20:45:31  bristow
 * changed exit to return(err) to stop this darn
 * thing from screwing me up
 *
 * Revision 1.2  1994/09/06  10:24:47  baker
 * removed the path portion of the "include" files.
 *
 * Revision 1.1  1993/08/20  20:46:35  baker
 * Initial revision
 *
*/
 
#include <stdio.h>
#include <errno.h>


#include "fit_errno.h"
#include "fitfile.h"
#include "endian.h"

char find_fit_rec_rev[] = {"$Revision: 1.6 $"};

int find_fit_rec(FIT_FILE *fp,long int time_req, long int *time_found)
{
   int iostat, i;
   union INX_REC inx_rec, inx_rec1;
   short int yr;
   long int long_t;		/* temporary long var */
   long int offset;
   long int prev_rec;
   long int recno;
   long int end_time;
   long int last_rec;
   long int start_time;
   long int first_rec;
   short int mxmon, mxday, mxhour, mxmin, mxsec;
   short int etst;


/* First, check the time requested and compare to the
   start time.  If the difference is really large it must
   be because the file spans a year boundary.  So we have
   to add a 1-year offset to the time requested. */
/*   fprintf(stderr,"value of inx_yr_offset = %d\n",fp->inx_yr_offset); */

   long_t = fp->stime - time_req;
   if(long_t > 10000000L) 
   {
/*       fprintf(stderr,"adjusting time_req by 1 yr\n");
*/       
      time_req = time_req + fp->inx_yr_offset;    
      long_t = fp->stime - time_req;
   }
   

   /* if requested time < start time 
      position fit file at the beginning of the file
   */
   if(long_t > 0L) {
      offset = fp->inx_srec;
      iostat = lseek(fp->inxdes,offset,SEEK_SET);
      if(iostat == -1) {
         perror("lseek index error1:");
         return (errno);
      }
      
      iostat = read(fp->inxdes,&inx_rec,sizeof(inx_rec));
      if(iostat == -1) {
         perror("read index error2:");
         return (errno);
      }

/* see if this is a big endian machine.  If so, swap bytes */

   if (endian(&etst) == 0) {
      memcpy(&inx_rec1, &inx_rec, sizeof(inx_rec));
      for (i=0; i<4; ++i) swab_dword(&inx_rec.iv[i], &inx_rec1.iv[i]);
      }


      fp->inx_offset = offset;
      fp->fit_offset = (inx_rec.index.recno - 1) * sizeof(union FIT_REC);

      /* position the file pointers of index and the fit files */      
      offset = fp->inx_srec;
      iostat = lseek(fp->inxdes,offset,SEEK_SET);
      if(iostat == -1) {
         perror("lseek index error3:");
         return (errno);
      }

      iostat = lseek(fp->fitdes,fp->fit_offset,SEEK_SET);
      if(iostat == -1) {
         perror("lseek fit error3:");
         return (errno);
      }

      *time_found = inx_rec.index.rtime;

      return(F_FIT_REC_LT);


   } /* if( long_t > 0L ) */
   else if(time_req > fp->etime + fp->inx_yr_offset) {
      
      /* requested time > end time */
      offset = fp->inx_erec;
      
      iostat = lseek(fp->inxdes,offset,SEEK_SET);
      if(iostat == -1) {
         perror("lseek index error4:");
         return (errno);
      }
      
      iostat = read(fp->inxdes,&inx_rec,sizeof(inx_rec));
      if(iostat == -1) {
         perror("read index error4:");
         return (errno);
      }

/* see if this is a big endian machine.  If so, swap bytes */

   if (endian(&etst) == 0) {
      memcpy(&inx_rec1, &inx_rec, sizeof(inx_rec));
      for (i=0; i<4; ++i) swab_dword(&inx_rec.iv[i], &inx_rec1.iv[i]);
      }
/*fprintf(stderr,"last index rec = %d %d %d %d\n",
	inx_rec.iv[0],inx_rec.iv[1],inx_rec.iv[2],inx_rec.iv[3]);
*/      
         
      fp->inx_offset = offset;
      fp->fit_offset = (inx_rec.index.recno - 1) * sizeof(union FIT_REC);

      /* position the file pointers of index and the fit files */      
      iostat = lseek(fp->inxdes,offset,SEEK_SET);
      if(iostat == -1) {
         perror("lseek index error5:");
         return (errno);
      }
/*fprintf(stderr,"fit seek recno=%d, fit_offset=%d\n",inx_rec.index.recno,
	fp->fit_offset);
*/      
      iostat = lseek(fp->fitdes,fp->fit_offset,SEEK_SET);
      if(iostat == -1) {
         perror("lseek fit error5:");
         return (errno);
      }

      *time_found = inx_rec.index.rtime;

      return(F_FIT_REC_GT);

   } /* else if(time_req > inx_rec... */

   else {
      /* the start time <= requested time <= end time */

      prev_rec = 0L;
      recno = fp->inx_srec/fp->inx_recl;
      start_time = fp->stime;
      end_time = fp->etime + fp->inx_yr_offset;
      first_rec = fp->inx_srec/fp->inx_recl;
      last_rec = fp->inx_erec/fp->inx_recl;
      
      while(prev_rec != recno) {
         prev_rec = recno;
         recno = (long)((float)(time_req - start_time)/
                        (float)(end_time - start_time) * 
                        (float)(last_rec - first_rec ) + first_rec); 
         
         offset = recno * sizeof(inx_rec);
	 
         iostat = lseek(fp->inxdes,offset,SEEK_SET);
         if(iostat == -1) {
            perror("lseek index error6:");
            return (errno);
         }
      
         iostat = read(fp->inxdes,&inx_rec,sizeof(inx_rec));
         if(iostat == -1) {
            perror("read index error6:");
            return (errno);
         }

/* see if this is a big endian machine.  If so, swap bytes */

   if (endian(&etst) == 0) {
      memcpy(&inx_rec1, &inx_rec, sizeof(inx_rec));
      for (i=0; i<4; ++i) swab_dword(&inx_rec.iv[i], &inx_rec1.iv[i]);
      }


         /* check for year wrap around */
         if(inx_rec.index.rtime < start_time) 
	 {
            inx_rec.index.rtime = inx_rec.index.rtime + fp->inx_yr_offset;
	}
	 
         /* if the time found is exactly time_req */
         if(inx_rec.index.rtime == time_req) {

            fp->inx_offset = offset;
            fp->fit_offset = (inx_rec.index.recno - 1) * sizeof(union FIT_REC);

            /* position the file pointers of index and the fit files */      
            iostat = lseek(fp->inxdes,offset,SEEK_SET);
            if(iostat == -1) {
               perror("lseek index error7:");
               return (errno);
            }

            iostat = lseek(fp->fitdes,fp->fit_offset,SEEK_SET);
            if(iostat == -1) {
               perror("lseek fit error7:");
               return (errno);
            }
                        
            *time_found = inx_rec.index.rtime;

            return(EOK);
         } /* if(inx_rec.index... */

         else if(inx_rec.index.rtime < time_req) {

            first_rec = recno;
            start_time = inx_rec.index.rtime;
         } /* if(inx_rec.index */
         else {

            end_time = inx_rec.index.rtime;
            last_rec = recno;
         }
      } /* while */

      /* couldn't find the exact time */
      recno = first_rec;
      if(recno > last_rec)
         recno = last_rec;
 
      offset = recno * sizeof(inx_rec);
      
      iostat = lseek(fp->inxdes,offset,SEEK_SET);
      if(iostat == -1) {
         perror("lseek index error8:");
         return (errno);
      }

      iostat = read(fp->inxdes,&inx_rec,sizeof(inx_rec));
      if(iostat == -1) {
         perror("read index error8:");
         return (errno);
      }

/* see if this is a big endian machine.  If so, swap bytes */

   if (endian(&etst) == 0) {
      memcpy(&inx_rec1, &inx_rec, sizeof(inx_rec));
      for (i=0; i<4; ++i) swab_dword(&inx_rec.iv[i], &inx_rec1.iv[i]);
      }


      fp->inx_offset = offset;
      fp->fit_offset = (inx_rec.index.recno - 1) * sizeof(union FIT_REC);

      /* position the file pointers of index and the fit files */      
      iostat = lseek(fp->inxdes,offset,SEEK_SET);
      if(iostat == -1) {
         perror("lseek index error9:");
         return (errno);
      }
      iostat = lseek(fp->fitdes,fp->fit_offset,SEEK_SET);
      if(iostat == -1) {
         perror("lseek fit error9:");
         return (errno);
      }

      *time_found = inx_rec.index.rtime;
                     
      return(F_FIT_REC_INEXACT);

   } /* else */
 
} /* find_fit_rec() */

