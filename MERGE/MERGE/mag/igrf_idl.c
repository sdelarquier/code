/* This program provides an interface between IDL and the subroutine
   IGRFCALL, which computes the magnetic field at a specified geographic
   point.

   The calling sequence is:

   status = call_external("library","igrf_idl",date,glat,
                            glong,altitude,Bx,By,Bz)

*/
/*
   $Log:	igrf_idl.c,v $
 * Revision 1.1  94/11/29  12:19:46  12:19:46  baker (Kile Baker S1G)
 * Initial revision
 * 
*/
#include <stdio.h>
#include <math.h>

static double igrf_idl_last_date;
extern void igrfcall();

long igrf_idl(argc, argv)
  int argc;
  void *argv[];
{
    short ifrst;
    long retval;
    double *date;
    double *flat, *flon, *alt, *Bx, *By, *Bz;
    
    if (argc !=7) 
    {
	printf("igrf_idl:  Invalid argument list\n");
	retval = -1;
	return retval;
    }
    
    date = (double *) argv[0];
    flat = (double *) argv[1];
    flon = (double *) argv[2];
    alt  = (double *) argv[3];
    Bx   = (double *) argv[4];
    By   = (double *) argv[5];
    Bz   = (double *) argv[6];

    if (fabs(*date - igrf_idl_last_date) > 1.0) {
	ifrst = 1; 
    }
    else ifrst = 0;

    igrf_idl_last_date = *date;
    
    igrfcall(&ifrst, date, flat, flon, alt, Bx, By, Bz);
    
    retval = 0;
    return retval;
}

