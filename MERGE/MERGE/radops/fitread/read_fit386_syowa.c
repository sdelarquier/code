/* This routine is used to correct the velocity sign error that switched
   the sign on the original Syowa radar data.  The sign error is 
   present prior to 1996 and in 1996 up to April 24.

   The routine first calls the usual read_fit386_v130 and then
   simply reverses the sign on all the velocity values.
*/

/*
$Log: read_fit386_syowa.c,v $
Revision 1.1  1997/04/16 15:10:54  baker
Initial revision

*/

#include "fitfile.h"
#include "fit_errno.h"

extern int read_fit386_v130( FIT_FILE *, struct FIT_DATA *);

char read_fit386_syowa_rev[] = {"$Revision: 1.1 $"};


int read_fit386_syowa( FIT_FILE *fp, struct FIT_DATA *fd) 
{
  int k, status;
  
  status = read_fit386_v130(fp, fd);
  
  for (k = 0; k < MAX_RANGE; ++k) {
    fd->vel[k]   = -(fd->vel[k]);
    fd->x_vel[k] = -(fd->x_vel[k]);
  }
  
  return status;
}
