/*	This program reads the raw data files and produces an index file
	which can then be used to provide direct access to the raw data.

	NOTE:  the index file will be created in the current directory.
	if the current directory is not the same as the directory where
	the raw data file exists, the index file will have to be moved to the
	correct directory after it has been created.
*/

/* 
$Log:	create_raw_index.c,v $
 * Revision 1.1  94/07/22  12:22:42  12:22:42  baker (Kile Baker S1G)
 * Initial revision
 * 
*/

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "radops.h"
#include "rawfile.h"
#include "endian.h"

extern size_t raw_read();
extern RAW_FILE *rawropen();
extern long cnv_mdhms_sec();

main()
{
long t1, told, rec_offset;
long indrec[2];
short int indata[8192];
int rind_file;
RAW_FILE *raw_file;
size_t iostat;
struct rawdata *raddat;
short int etst;
long int ltemp;
int i;

char raw_name[80], rind_name[80], filename[80];

raddat = (struct rawdata *) calloc(1, sizeof(struct rawdata));

rec_offset = 0;		/* initialize the byte number */
told = 0;

printf("Ready to create an index file to the raw RADOPS 386 radar data\n");
printf("\nEnter the file name (no extension) of the raw data file: ");
scanf("%s",raw_name);
getchar();	/* get rid of the <cr> at the end */

strcpy(filename,raw_name);
strcat(rind_name,raw_name);

if ((raw_file = rawropen(filename)) == 0)
	{
	printf("Unable to open raw data file\n");
	exit(ENOENT);
	}

/* Now open the index file for output */

strcat(rind_name,".rin");
rind_file = open(rind_name,O_RDWR|O_CREAT|O_TRUNC,0664);
if(rind_file <= 0) 
	{
	printf("Unable to open output file %s, fp=%x\n",rind_name,rind_file);
	exit(ENOENT);	
	}
else 
{
    printf("The index file will be created in the current directory\n");
}

/*	now start reading the raw data file and filing in the index */

while (1)
	{
	rec_offset = raw_file->raw_offset;
	iostat = raw_read(raw_file, 0, raddat);
	if (iostat == EOF)
		{
		close(rind_file);
		raw_close(raw_file);
		exit(0);
		}
	t1 = cnv_mdhms_sec(&(raddat->PARMS.YEAR),
			&(raddat->PARMS.MONTH),
			&(raddat->PARMS.DAY),
			&(raddat->PARMS.HOUR),
			&(raddat->PARMS.MINUT),
			&(raddat->PARMS.SEC));
	indrec[0]=t1;
	indrec[1]=rec_offset;
	
	if (endian(&etst) == 0) {
	  for (i=0; i < 2; ++i) {
	    ltemp = indrec[i];
	    swab_dword(&indrec[i],&ltemp);
	  }
	}
	write(rind_file, indrec, sizeof(indrec[0])*2);
	if (t1-told > 900)
		{
		printf("%d  %d\n",t1,rec_offset);
		told = t1;
		}
	}
}
