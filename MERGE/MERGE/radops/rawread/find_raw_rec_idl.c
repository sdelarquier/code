/* This routine provides the interface between the IDL procedure for locating 
a specific point in a raw data file and the standard C routines.
*/

#include <stdio.h>

#include "rawfile.h"

long find_raw_rec_idl(argc,argv)
     int argc;
     void *argv[];
{

 RAW_FILE *fp;
 long int time_req;
 long int *time_found;
 long int find_raw_rec();
 
 fp = *(RAW_FILE **)argv[0];
 time_req = *(long int *)argv[1];
 time_found = (long int *)argv[2];

 return find_raw_rec( time_req, time_found, fp );

}
