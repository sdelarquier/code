/*  $Revision: 1.2 $ */
/*  This routine provides the IDL interface to the
    magnetic coordinates routine, "cnv$coord"
    It is called from IDL using the call_external mechanism.
    The calling sequence is:
    ret_val = call_external(<libpgm.sl>,"cnv_coord_idl",
               in_pos, order, out_pos, mg_flag, err, model)

    where in_pos is the 3-element input position,
          order is a left-over from old software and is ignored
	  out_pos is the 3-element output position
	  mg_flag is 1 for geo -> mag  and 2 for mag->geo
	  err is the error code 
	  model is the date of the IGRF model (1990 or 1995)
*/
/*
$Log: cnvcoord_idl.c,v $
 * Revision 1.2  1996/03/20  22:54:14  baker
 * Changed the declaration of the arguments to use
 * the types "int16" and "int32" instead of short and long.
 * This is to avoid conflicts with machines that have
 * different word sizes.  This requires a new header
 * file, "SD_types.h" which provides the typedef
 * for int16 and int32.
 *
 * Revision 1.1  1996/03/12  18:23:30  baker
 * Initial revision
 *
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "SD_types.h"

extern double sfc$$coeffs_com[2][3][3][121];

long cnvcoord_idl(argc,argv)
     int argc;
     void *argv[];
{
  static short myear = 1995;
    
  int16 *mgflag;
  int16 *order;
  int16 *err;
  int16 *model;
  
  float *inpos;
  float *in_lat;
  float *in_long;
  float *height;
  float *outpos;
  float *out_lat;
  float *out_long;
  float *out_r;

  char *filename;
  char ascii_flag = 1;
  
  if (argc < 5) 
  {
      fprintf(stderr,"cnvcoord_idl: invalid argument list. argc < 5\n");
      return -1;
  }
  
  inpos  = (float *) argv[0];
  order  = (int16 *) argv[1];
  outpos = (float *) argv[2];
  mgflag = (int16 *) argv[3];
  err    = (int16 *) argv[4];

  if (argc >= 6) model  = (short *) argv[5];
  else model = NULL;
  
  if (inpos == NULL || outpos == NULL || mgflag == NULL || err == NULL) 
  {
      fprintf(stderr,"cnvcoord_idl: invalid argument list from call_external\n");
      return -1;
  }
  
  in_lat  = inpos;
  in_long = (inpos+1);
  height  = (inpos+2);

  out_lat  = outpos;
  out_long = (outpos+1);
  out_r    = (outpos+2);

/* check the coordinate model year.  If no model is specified,
   the default is the most recent model.  If a model is
   specified and it is not the same as the previous model,
   then aacgm_init is called to load the coefficients of
   the desired model. */

  if (model == NULL) myear = 1995;
  else if (*model != myear) 
  {
      myear = *model;
      switch (myear) 
      {
      case 1995:
	  filename = getenv("AACGM95_DAT");
	  break;
      case 1990:
	  filename = getenv("AACGM90_DAT");
	  break;
      default:
	  fprintf(stderr,"invalid AACGM model date %d\n",myear);
	  return -1;
	  break;
      }
      if (filename == NULL) 
      {
	  fprintf(stderr,"MISSING ENVIRONMENT variables for AACGM\n");
	  return -1;
      }
      
      aacgm_init(filename,&ascii_flag);

  }
  
  cnv$coord(in_lat,in_long,height,order,out_lat,out_long,out_r,mgflag,err);
  if (*err != 0){
      return -1;
  }
  else return 0;
}
