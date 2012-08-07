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
$Log: find_fit_rec.c,v $
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

char find_fit_rec_rev[] = {"$Revision: 1.4 $"};

int find_fit_rec(FIT_FILE *fp,long int time_req, long int *time_found)
{
   int iostat, i;
   union INX_REC inx_rec, inx_rec1;
   short int yr;
   long int yr_offset;
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

   /* initialization of some variables */
   yr_offset = 0L;
   

   /* read the header of the index file */
   iostat = lseek(fp->inxdes,0,SEEK_SET);
   if(iostat == -1) {
      perror("lseek index header error:");
      return (errno);
   }

   iostat = read(fp->inxdes,&inx_rec,sizeof(inx_rec));
   if(iostat == -1) {
      perror("read index header error:");
      return (errno);
   }

/* see if this is a big endian machine.  If so, swap bytes */

   if (endian(&etst) == 0) {
      memcpy(&inx_rec1, &inx_rec, sizeof(inx_rec));
      for (i=0; i<4; ++i) swab_dword(&inx_rec.iv[i], &inx_rec1.iv[i]);
      }

   end_time = inx_rec.inx_hdr.etime;
   last_rec = inx_rec.inx_hdr.erec;
   first_rec  = inx_rec.inx_hdr.srec;
   start_time = inx_rec.inx_hdr.stime;




   /* check for the time at the start and end of fit file */
   if(end_time <= 0) {
      fprintf(stderr,"incomplete index file\n");
      fprintf(stderr,"run RECONSTRUCT to attempt to fix the file\n");
      return(RDFIT_INV_INX);
   }

   if(end_time < start_time) {
      fprintf(stderr,"end time is earlier than the start time\n");
      fprintf(stderr,"if this is due to change in year, enter the beginning year\n");
      fprintf(stderr,"otherwise enter Cntrl-C to abort and use RECONSTRUCT to\n");
      fprintf(stderr,"repair the index file\n");
      fprintf(stderr,"start year ? :");
      scanf("%d",&yr); 
      mxmon = 12;
      mxday = 31;
      mxhour = 23;
      mxmin = 59;
      mxsec = 59;
      yr_offset = cnv_mdhms_sec(&yr,&mxmon,&mxday,&mxhour,&mxmin,&mxsec) + 1;
      inx_rec.inx_hdr.etime = inx_rec.inx_hdr.etime + yr_offset;
   }

   long_t = inx_rec.inx_hdr.stime - time_req;
   if(long_t > 10000000L)
      time_req = time_req + yr_offset;    
   
   /* if requested time < start time 
      position fit file at the beginning of the file
   */
   if(long_t > 0L) {
      offset = inx_rec.inx_hdr.srec * sizeof(inx_rec);
      iostat = lseek(fp->inxdes,offset,SEEK_SET);
      if(iostat == -1) {
         perror("lseek index error:");
         return (errno);
      }
      
      iostat = read(fp->inxdes,&inx_rec,sizeof(inx_rec));
      if(iostat == -1) {
         perror("read index error:");
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
      offset = first_rec * sizeof(inx_rec);
      iostat = lseek(fp->inxdes,offset,SEEK_SET);
      if(iostat == -1) {
         perror("lseek index error:");
         return (errno);
      }

      iostat = lseek(fp->fitdes,fp->fit_offset,SEEK_SET);
      if(iostat == -1) {
	  printf("First  fitdes %d  fit_offset %d\n",fp->fitdes,fp->fit_offset);
         perror("lseek fit error:");
         return (errno);
      }

      *time_found = inx_rec.index.rtime;

      return(F_FIT_REC_LT);


   } /* if( long_t > 0L ) */
   else if(time_req > inx_rec.inx_hdr.etime) {
      
      /* requested time > end time */
      offset = last_rec * sizeof(inx_rec);
      
      iostat = lseek(fp->inxdes,offset,SEEK_SET);
      if(iostat == -1) {
         perror("lseek index error:");
         return (errno);
      }
      
      iostat = read(fp->inxdes,&inx_rec,sizeof(inx_rec));
      if(iostat == -1) {
         perror("read index error:");
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
         perror("lseek index error:");
         return (errno);
      }

      iostat = lseek(fp->fitdes,fp->fit_offset,SEEK_SET);
      if(iostat == -1) {
	  printf("Second  fitdes %d  fit_offset %d\n",fp->fitdes,fp->fit_offset);
         perror("lseek fit error:");
         return (errno);
      }

      *time_found = inx_rec.index.rtime;

      return(F_FIT_REC_GT);

   } /* else if(time_req > inx_rec... */

   else {
      /* the start time <= requested time <= end time */

      prev_rec = 0L;
      recno = inx_rec.inx_hdr.srec;

      while(prev_rec != recno) {
         prev_rec = recno;
         recno = (long)((float)(time_req - start_time)/
                        (float)(end_time - start_time) * 
                        (float)(last_rec - first_rec ) + first_rec); 
         
         offset = (recno - 1) * sizeof(inx_rec);
      
         iostat = lseek(fp->inxdes,offset,SEEK_SET);
         if(iostat == -1) {
            perror("lseek index error:");
            return (errno);
         }
      
         iostat = read(fp->inxdes,&inx_rec,sizeof(inx_rec));
         if(iostat == -1) {
            perror("read index error:");
            return (errno);
         }

/* see if this is a big endian machine.  If so, swap bytes */

   if (endian(&etst) == 0) {
      memcpy(&inx_rec1, &inx_rec, sizeof(inx_rec));
      for (i=0; i<4; ++i) swab_dword(&inx_rec.iv[i], &inx_rec1.iv[i]);
      }


         /* check for year wrap around */
         if(inx_rec.index.rtime < start_time)
            inx_rec.index.rtime = inx_rec.index.rtime + yr_offset;

         /* if the time found is exactly time_req */
         if(inx_rec.index.rtime == time_req) {
            fp->inx_offset = offset;
            fp->fit_offset = (inx_rec.index.recno - 1) * sizeof(union FIT_REC);

            /* position the file pointers of index and the fit files */      
            iostat = lseek(fp->inxdes,offset,SEEK_SET);
            if(iostat == -1) {
               perror("lseek index error:");
               return (errno);
            }

            iostat = lseek(fp->fitdes,fp->fit_offset,SEEK_SET);
            if(iostat == -1) {
	  printf("Third  fitdes %d  fit_offset %d\n",fp->fitdes,fp->fit_offset);
               perror("lseek fit error:");
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
      recno = first_rec + 1L;
      if(recno > last_rec)
         recno = last_rec;
 
      offset = (recno - 1) * sizeof(inx_rec);
      
      iostat = lseek(fp->inxdes,offset,SEEK_SET);
      if(iostat == -1) {
         perror("lseek index error:");
         return (errno);
      }

      iostat = read(fp->inxdes,&inx_rec,sizeof(inx_rec));
      if(iostat == -1) {
         perror("read index error:");
         return (errno);
      }

/* see if this is a big endian machine.  If so, swap bytes */

   if (endian(&etst) == 0) {
      memcpy(&inx_rec1, &inx_rec, sizeof(inx_rec));
      for (i=0; i<4; ++i) swab_dword(&inx_rec.iv[i], &inx_rec1.iv[i]);
      }


      fp->inx_offset = offset;
      fp->fit_offset = (inx_rec.index.recno - 1) * sizeof(union FIT_REC);
      printf("offsets %d  %d  %d\n",fp->inx_offset,inx_rec.index.recno,fp->fit_offset);
      /* position the file pointers of index and the fit files */      
      iostat = lseek(fp->inxdes,offset,SEEK_SET);
      if(iostat == -1) {
         perror("lseek index error:");
         return (errno);
      }
      iostat = lseek(fp->fitdes,fp->fit_offset,SEEK_SET);
      if(iostat == -1) {
	  printf("Fourth  fitdes %d  fit_offset %d\n",fp->fitdes,fp->fit_offset);
         perror("lseek fit error:");
         return (errno);
      }

      *time_found = inx_rec.index.rtime;
                     
      return(F_FIT_REC_INEXACT);

   } /* else */
 
} /* find_fit_rec() */

