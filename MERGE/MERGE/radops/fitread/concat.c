/* File: /home/darn/radops/fitread/concat.c */
/* Last Modification: 07-JAN-2002 */

/* Use:

   concat dir day_string radar
or
   concat file_list
   */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <fitfile.h>
#include <endian.h>

main(int argc, void *argv[])
{
    DIR *dirp;
    struct dirent *thedir;
    char dirname[100];
    int fp1,fp2,fp3,compare();
    char day_string[6];
    char filename[100];
    char fitfile[100];
    char inxfile[100];
    char froot[20];
    char *file_list[100];
    char *rad;
    short i,j,fcount,first;
    long start_t,end_t,start_r,end_r;
    long rec_num,nrec,xcf,rec_t,two;
    int endi, etst;
    size_t stlen;
    unsigned int buf_size;
    union BUF 
    {
	char buf[FIT_RECL];
	union FIT_REC fit_rec;
    } fit_buf;
    union INX_BUF
    {
	char buf[INX_RECL];
	struct INX_HDR inx_hdr;
	struct INX_INDEX inx_rec;
    } inx_buf;
    
    if (argc <= 2)
    {
      printf( "Use:\n");
      printf( "concat dir day_string radar\n");
      printf( "or\n");
      printf( "concat file_list\n");
      exit( 0);
    }

    bzero( dirname, 100);
    bzero( day_string, 6);
    bzero( filename, 100);
    bzero( fitfile, 100);
    bzero( inxfile, 100);
    bzero( froot, 20);

    endi= endian( &etst);

    two = 2;
    rad = (char *)argv[3];
    if( ((argc > 3) && (strlen(rad) > 1)) || (argc <= 3))
    {
	for( j=1; j<=argc-1; j++ )
	{
	    file_list[j-1] = (char *)argv[j];
	}
	strcpy(dirname,".");
	fcount = j;
    } else {
	strcpy(dirname,(char *)argv[1]);
	strncpy(day_string,(char *)argv[2],6);
	if( (dirp = opendir(dirname)) == NULL )
	{
	    fprintf(stderr,"ERROR--COULD NOT OPEN DIRECTORY\n");
	}
	j=0;
	while( (thedir = readdir(dirp)) != NULL)
	{
	    if((strstr(thedir->d_name,day_string) != NULL) && 
	       ((strpbrk(thedir->d_name,rad) - thedir->d_name)< 10) &&
	       (strpbrk(thedir->d_name,rad) != NULL) &&
	       (strstr(thedir->d_name,"fit") != NULL) &&
	       (strpbrk(thedir->d_name,"C") == NULL))
	    {
		file_list[j] = (char *) malloc( strlen(thedir->d_name)+1 );
		strcpy(file_list[j],thedir->d_name);
		j ++;
	    }
	}
	fcount = j+1;
    }
    qsort(file_list,fcount-1,sizeof(char *), compare);
    /* Mod to four digit years; 20000111 DA */
    strncpy(froot,file_list[0], 11);
    strcat(strcpy(fitfile,froot),"C.fit");
    strcat(strcpy(inxfile,froot),"C.inx");
    printf("New File: %s\n",fitfile);
    fflush(stdout);
    if((fp2=open(fitfile, O_WRONLY | O_CREAT, S_IRUSR | 
		 S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH)) == -1)
    {
	fprintf(stderr,"ERROR--COULD NOT OPEN FILE %s\n",fitfile);
	exit(0);
    }
    if((fp3=open(inxfile, O_WRONLY | O_CREAT, S_IRUSR | 
		 S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH)) == -1)
    {
	fprintf(stderr,"ERROR--COULD NOT OPEN FILE %s\n",inxfile);
	exit(0);
    }
    strcpy(filename,dirname);
    strcat(filename,"/");
    strcat(filename,file_list[0]);
    if((fp1=open(filename,O_RDONLY)) == -1)
    {
	fprintf(stderr,"ERROR--COULD NOT OPEN FILE %s\n",filename);
	exit(0);
    }
    read(fp1,fit_buf.buf,FIT_RECL);
    write(fp2,fit_buf.buf,FIT_RECL);
    write(fp3,inx_buf.buf,INX_RECL);
    rec_num=2;
    start_t = 0;
    end_r = 0;
    first = 1;
    nrec = 0;
    for( i=1; i<fcount; i++ )
    {
	printf("FILE:  %s\n",filename);
	while( read(fp1,fit_buf.buf,FIT_RECL) == FIT_RECL )
	{
	    if( fit_buf.fit_rec.v13_r0.rtime != 0 )
	    {
		write(fp2,fit_buf.buf,FIT_RECL);
		if( fit_buf.fit_rec.v13_r0.rrn == 0 && first != 1 )
		{
		    if (endi == 0)
		    {
		      swab_dword(&inx_buf.inx_rec.rtime,&rec_t);
		      swab_dword(&inx_buf.inx_rec.recno,&rec_num);
		      swab_dword(&inx_buf.inx_rec.nrec,&nrec);
		      swab_dword(&inx_buf.inx_rec.xflag,&xcf);
		    }
		    write(fp3,inx_buf.buf,INX_RECL);
		    if (endi == 0)
		    {
		      swab_dword(&rec_t,&fit_buf.fit_rec.v13_r0.rtime);
		    }
		    rec_num += nrec;
		    xcf = fit_buf.fit_rec.v13_r0.r_parm.XCF;
		    nrec = 0;
		    end_r ++;
		} else if( first == 1 ) {
		    fflush(stdout);
		    first = 0;
		    if (endi == 0)
		    {
		      swab_dword(&start_t,&fit_buf.fit_rec.v13_r0.rtime);
		    }
		    rec_t = start_t;
		    fflush(stdout);
		}
		nrec++;
	    }
	}
	close(fp1);
	if ( i < (fcount - 1) )
	{
	  strcpy(filename,dirname);
	  strcat(filename,"/");
	  strcat(filename,file_list[i]);
	  if((fp1=open(filename,O_RDONLY)) == -1)
	  {
	      fprintf(stderr,"ERROR--COULD NOT OPEN FILE %s\n",filename);
	  }
	  read(fp1,fit_buf.buf,FIT_RECL);
	}
    }
    if (endi == 0)
    {
      swab_dword(&inx_buf.inx_rec.rtime,&rec_t);
      swab_dword(&inx_buf.inx_rec.recno,&rec_num);
      swab_dword(&inx_buf.inx_rec.nrec,&nrec);
      swab_dword(&inx_buf.inx_rec.xflag,&xcf);
    }
    write(fp3,inx_buf.buf,INX_RECL);
    if (endi == 0)
    {
      swab_dword(&inx_buf.inx_hdr.stime,&start_t);
      swab_dword(&inx_buf.inx_hdr.etime,&rec_t);
      swab_dword(&inx_buf.inx_hdr.srec,&two);
     swab_dword(&inx_buf.inx_hdr.erec,&end_r);
    }
    lseek(fp3,0,SEEK_SET);
    write(fp3,inx_buf.buf,INX_RECL);
    close(fp2);
    close(fp3);
}

int compare( char **p1, char **p2)
{
    return( strcmp( *p1, *p2) );
}



