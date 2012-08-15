/* These routines provide the IDL interface to the standard library
   routines that check for bad lags */
#include <stdio.h>
#include "radops.h"

int badlags_idl(argc, argv)
   int argc;
   char *argv[];
{
    struct rawdata *raddat;
    
    raddat = (struct rawdata *)argv[0];
    
    badlags(raddat);
    return 0;
}

int ckrng_idl(argc, argv)
   int argc;
   char *argv[];
{
    short int range;
    short int *badlag;
    struct rawdata *raddat;
    
    long int status;
    
    range = *( (short *) argv[0]);
    badlag = (short *) argv[1];
    raddat = (struct rawdata *) argv[2];
    
    status = ckrng(range, badlag, raddat);
    
    return 0;
}    
