/* This routine replaces characters in a string with a single character.
   For example, it can be used to replace all instances of a particular
   character (or set of characters) with space.

   The calling sequence is:

   s  = strcrep ( char *src, char *rlist, char rep );

   The source string must be a null delimited character string.
   The character string rlist must also be a null deliminted character
   string specifying all the characters that are to be replaced.

   The third argument is a single character that is used to replace the
   characters the rlist.  NOTE:  This replacement character must not be
   a NULL.

   Example, to replace all commas, slashes and colons with blanks use:
   s = strcrep( src, ",/:",' ');

   The function returns the pointer to the (now modified) source string.
   This allows the function to be used with the "sscanf" function.  For
   example, to scan a string for a sequence of 3 integers that might
   be separated by blanks, commas or slashes you could use:

   sscanf(strcrep(src,",/",' '), "%d%d%d",&a, &b, &c);
*/

/*
$Log:	strcrep.c,v $
 * Revision 1.1  94/07/26  15:18:45  15:18:45  baker (Kile Baker S1G)
 * Initial revision
 * 
*/

#include <string.h>

char strcrep_rev[] = {"$Revision: 1.1 $"};

#if defined(__STDC__) || defined(PROTOTYPE)

char *strcrep( char *src, char *rlist, char rep)

#else

char *strcrep(src, rlist, rep)
  char *src, *rlist, rep;

#endif

{
    char * s;

/* first check the arguments.  If the source string is null or empty
   or the replacement list is null or empty or the replacement character
   is null, return with no changes */

    if (src == NULL) return NULL;
    if (strlen(src) == 0) return src;
    if (rlist == NULL || strlen(rlist) == 0) return src;
    if (rep == NULL) return src;
    
    s = src;
    while ( s != NULL  &&  ( s < src + strlen(src))) 
    {
	s = strpbrk(s, rlist);
	if (s != NULL) 
	{
	    *s = rep;
	    ++s;
	}
    }
    return src;
}


