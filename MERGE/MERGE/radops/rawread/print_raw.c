/* This program reads the raw data files and prints out the data.
   There are two ways of invoking the program:
   1) simply enter the command "print_raw".  The program will then prompt
   you for a file name, a start time and and end time.  The data will be
   printed to the screen.
   2) enter the command with 1 or 2 command line arguments:
        print_raw <input_file> <output_file>
      The program will read the data from the file specified by the first
      argument.  If a second argument is specified, the results will be printed
      to that file.
*/
/*
$Log:	print_raw.c,v $
 * Revision 1.2  94/07/26  15:15:22  15:15:22  baker (Kile Baker S1G)
 * Changed the "gets" to "fgets" and flushed the input buffer before reading
 * the start time.
 * 
*/

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "rawfile.h"
#include "radops.h"

extern char *strcrep();
extern long cnv_mdhms_sec();
extern RAW_FILE * rawropen();
extern long find_raw_rec();

main(int argc, char *argv[]) {

RAW_FILE *fp;
struct rawdata dt, *raddat;
char filename[80];
int i,j;
long t1, t2, tstart, t;
short syr, smo, sday, shr, smin, ssec;
short eyr, emo, eday, ehr, emin, esec;
long status, offset;
FILE *outfile;
char ofile[80], line[80];

short int badlag[50];

raddat = &dt;

if (argc > 1) strcpy (filename, argv[1]);
else {
  printf("Enter the filename with no extension: ");
  scanf("%s",filename);
}

if (argc > 2) {
  strcpy (ofile, argv[2]);
  outfile = fopen(ofile,"w");
}
else outfile = stdout;

fp = rawropen(filename);

if (fp == 0) {
  printf("file: %s NOT FOUND\n",filename);
  errno = ENOENT;
  perror("testread: ");
  exit(ENOENT);
}

t1 = -1;  t2 = -1;

while (t1 < 0) {
  printf("Enter start time (yr, mo, day, hr, min, sec): ");
  sscanf(strcrep(fgets(line, sizeof(line), stdin),",/:",' '),
	"%hd%hd%hd%hd%hd%hd",&syr,&smo,&sday,&shr,&smin,&ssec);
  t1 = cnv_mdhms_sec(&syr, &smo, &sday, &shr, &smin, &ssec);
}

while (t2 < 0) {
  printf("Enter stop time (yr, mo, day, hr, min, sec): ");
  sscanf(strcrep(fgets(line, sizeof(line), stdin),",/:",' '),
	 "%hd%hd%hd%hd%hd%hd",&eyr,&emo,&eday,&ehr,&emin,&esec);
  t2 = cnv_mdhms_sec(&eyr, &emo, &eday, &ehr, &emin, &esec);
}

offset = find_raw_rec(t1, &tstart, fp);

if (offset < 0) {
  errno = EIO;
  perror("Invalid offset returned from find_raw_rec\n");
  exit(EIO);
}
else printf("offset from find_raw_rec = %d\n",offset);

/* mod for badlags */
status = raw_read(fp, offset, raddat);
do {
  status = raw_read(fp, 0, raddat);
} while ( (dt.PARMS.MPPUL != 9) && (dt.PARMS.MPINC != 1500));
for (i=0; i<dt.PARMS.NRANG; ++i) {
  dt.pwr0[i]= 0;
}
for (i=0; i<dt.PARMS.NRANG; ++i) {
  status= ckrng(i, badlag, raddat);
}

status = raw_read(fp, offset, raddat);
if (status == EOF) {
  errno = EIO;
  perror ("Bad status returned from first read operation\n");
  exit (EIO);
}

/* printf("status from first raw_read = %d\n",status); */

while (t <= t2 && status != EOF) {
  t = cnv_mdhms_sec(&dt.PARMS.YEAR, &dt.PARMS.MONTH, &dt.PARMS.DAY,
		    &dt.PARMS.HOUR, &dt.PARMS.MINUT, &dt.PARMS.SEC);
  fprintf(outfile,"%d = %4hd/%2hd/%2hd %2hd:%2hd:%2hd  bmnum: %2hd\n",
	  t,dt.PARMS.YEAR,dt.PARMS.MONTH,dt.PARMS.DAY,dt.PARMS.HOUR,
	  dt.PARMS.MINUT,dt.PARMS.SEC,dt.PARMS.BMNUM);
  fprintf(outfile,"FRANGE = %4hd, RSEP = %3hd, NOISE = %d\n",
	  dt.PARMS.FRANG, dt.PARMS.RSEP, dt.PARMS.NOISE);
  fprintf(outfile,"Lag-0 power:\n");
  for (i=0; i<dt.PARMS.NRANG; ++i) 
    fprintf(outfile,"%3d  %8d\n",i+1,dt.pwr0[i]);
  for (i=0; i<dt.PARMS.NRANG; ++i) {
    if (dt.acfd[i][0][0] == 0) continue;
    fprintf(outfile,"\nACF for range %d\n",i+1);
    for (j=0; j < dt.PARMS.MPLGS; ++j) {
      if (j % 3 == 0) fprintf(outfile,"\n");
      fprintf(outfile,"%2hd (%8d,%8d)     ",
	      dt.LAG_TABLE[1][j]-dt.LAG_TABLE[0][j],
	     dt.acfd[i][j][0],dt.acfd[i][j][1]);
    }
  }
  fprintf(outfile,"\n");
  status = raw_read(fp, 0, raddat);
}
printf("END OF TIME or END OF FILE\n");
}





















