/*	This routine implements the WATCOM _splitpath function for other UNIX and
	VAX/VMS systems */
/*
$Log:	_splitpath.c,v $
 * Revision 1.1  94/09/06  10:22:22  10:22:22  baker (Kile Baker S1G)
 * Initial revision
 * 
 * Revision 1.6  1994/02/25  21:11:23  baker
 * *** empty log message ***
 *
 * Revision 1.4  1993/10/04  15:51:54  baker
 * Fixed a bug in determining the file name if an extension has also included.
 *
 * Revision 1.3  1993/09/07  15:04:01  baker
 * Changed the routine to return an int instead of void.
 *
 * Revision 1.2  1993/08/26  15:39:36  baker
 * This revision fixes a bug in handling the directory, when a path
 * with directory is specified.
 *
 * Revision 1.1  1993/08/20  20:31:52  baker
 * Initial revision
 *
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char _splitpath_rev[] = {"$Revision: 1.1 $"};

int _splitpath(char *fullpath, char *node, char *dir, char *fname, char *ext)
{
char temp[256], *s, *t;

strcpy(node, "");	/* initialize node to the null string */
strcpy(dir, "");	/* initialize directory to null */
strcpy(fname, "");
strcpy(ext, "");

strcpy(temp, fullpath);

s = fullpath;

/* first, skip over any whitespace that is present at the beginning of the
string */

while (isspace( (int) *s)) ++s;

/* then work from the end of the string and replace all white space at
   the end with nulls */

t = s + strlen(s) - 1;
if (t < s) return 1;

while (isspace( (int) *t)) {
    *t = (char) 0;
    --t;
}

/* now look for the last slash,  which would end the directory information */

t = strrchr(s, '/');

if (t != NULL) {
	++t;
	strncpy(dir, s, (int) (t - s));
	dir[(int) (t - s)] = 0;
	s = t;
	}

/* now look for '.' to begin the extension */

t = strchr(s, '.');
if (t == NULL) {
	strcpy(fname, s); 	/* there is no extension provided. Assume.DAT*/
	strcpy(ext, "");
	}
else {
	strncpy(fname, s, (int) (t - s));
	fname[(int) (t - s)] = 0;
	strcpy(ext, t+1);
	}
return 0;
}





