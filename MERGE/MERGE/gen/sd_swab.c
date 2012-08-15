/*	This routine swaps the byte order for a block of data.

NOTE:  The calling sequence here is consistent with the usual POSIX
order.  The first address is the DESTINATION, while the second address
is the SOURCE.  This is different from the order given in the QNX WATCOM
library.

NOTE:  It is legal to use the same destination and source arguments (i.e.
	to swap the bytes in place 


The calling sequence is:

	void swab( char * dest, char *source, int num_bytes)
*/
/*
$Log: sd_swab.c,v $
 * Revision 1.1  1994/03/08  20:59:35  baker
 * Initial revision
 *
*/

char swab_rev[] = {"$Revision: 1.1 $"};

void sd_swab (char * dest, char *source, int num)
{
register int i;
register char temp;

i = 0;
while (i < num) {
	temp = source[i+1];
	dest[i+1] = source[i];
	dest[i] = temp;
	i = i+2;
	}
return;
}

