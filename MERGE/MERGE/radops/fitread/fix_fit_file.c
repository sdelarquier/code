/*  This program fixes the problem with fitacf files in
    version 3.x where the version number was being set
    incorrectly.  The correct format for the version number
    in the header record should be:
    . . . . version 3.x (QNX)
    The incorrect files are missing the space between the
    the word 'version' and the actual version number */
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

/*
$Log$
*/

#define FIT_RECL 1024

main ( int argc, char * argv[])
{
  FILE *fp;
  char buf[FIT_RECL], filename[128], *sp, *vp;
  int nbytes, nchar;
  
  /* try to open the file */
  if (argc != 2) {
    fprintf(stderr,"fix_fit_file: Invalid argument list.\nargc=%d\n",argc);
    exit(-1);
  }

  /* check the name of the file.  If the name does not contain
     the string '.fit', append that to the name before proceeding */
  sp = strstr(argv[1],".fit");
  if (sp == NULL) {
    strcpy(filename,argv[1]);
    strcat(filename,".fit");
  }
  else
    strcpy(filename,argv[1]);
  
  fp = fopen(filename,"r+");
  if (fp == 0) {
    perror("fix_fit_file: fopen error\n");
    fprintf(stderr,"Unable to open file %s\n",argv[1]);
    exit(-1);
  }
  
  /* OK the file has been opened.  Read the header record and see if
     it needs to be fixed */

  nbytes = fread(buf, 1, FIT_RECL, fp);
  if (nbytes != FIT_RECL) {
    perror("fix_fit_file: fread error\n");
    fprintf(stderr,"Unable to read the header. nbytes=%d\n",
	    nbytes);
    exit(-1);
  }
  
  /* set the pointer sp to the beginning of the string (i.e. skip over
     the first two words, which contain numbers that are not part of
     the header strings */

  sp = buf+4;
  
  /* now search for the string "version" */

  vp = strstr(sp,"version");
  if (vp == NULL) {
    fprintf(stderr,"fix_fit_file: Version could not be found.  Is this really a FIT file?\n");
    exit(-1);
  }
  
  vp = vp + strlen("version");  /* point to the first char after "version" */
  if (*vp == ' ') {
    fprintf(stderr,"fix_fit_file: The file was already OK\n");
    exit(0);
  }
  
  /* compute the number of characters to be shifted to the right */
  nchar = FIT_RECL - (vp - buf) -1;
  memmove(vp+1, vp, nchar);
  *vp = ' ';
  
  /* OK, the header has been fixed in memory.  Now write it back out */
  
  if (fseek(fp, (long) 0, SEEK_SET) != 0) {
    perror("fix_fit_rec: Unable to reset file to the beginning\n");
    exit(-1);
  }
  
  nbytes = fwrite(buf, 1, FIT_RECL, fp);
  if (nbytes != FIT_RECL) {
    perror("fix_fit_file: fwrite error\n");
    fprintf(stderr,"Could not write the corrected header. Nbytes = %d\n",nbytes);
    exit (-1);
  }
  /* Everything seems to be ok */
  fflush(fp);
  fclose(fp);
  exit (0);
}

  
