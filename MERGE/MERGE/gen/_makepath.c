/*  This routine implements the WATCOM makepath function for  VAX/VMS */
/*
$Log:	_makepath.c,v $
 * Revision 1.1  93/08/20  20:27:55  20:27:55  baker (Kile Baker S1G)
 * Initial revision
 * 
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char _makepath_rev[] = {"$Revision: 1.1 $"};

void _makepath (char *fullpath, char *node, char *dir, char *fname, char *ext)
{
int n;
char *s;

strcpy(fullpath, "");
if (strlen(node) != 0) {
	strcpy(fullpath, node);
	n = strlen(fullpath);
	if (strncmp(&fullpath[n-1], ":", 1) != 0) strcat(fullpath,":");
	}
if (strlen(dir) > 0) {
	/* the directory must end with a slash */
	strcat (fullpath, dir);
	n = strlen(dir);
	
	if ( dir[n-1] != '/' ) strcat(fullpath, "/");
	}
s = strchr(fname,' ');
if (s !=0 ) *s = 0;  /* get rid of trailing blanks */

strcat(fullpath, fname);
if (strlen(ext) > 0) {
	if (ext[0] != '.') strcat(fullpath, ".");
	strcat(fullpath, ext);
	}
}


