/* This routine handles the byte swapping for the ACF and XCF records in the FIT data files.
   The ACF and XCF records are the same for all versions of FITACF, including the old DG
   files. */
/*
$Log:	swap_rrn1.c,v $
 * Revision 1.4  94/09/06  11:20:57  11:20:57  baker (Kile Baker S1G)
 * elliminated paths
 * 
 * Revision 1.2  93/10/18  15:41:34  15:41:34  bristow (Bill Bristow S1G)
 * Revised to change call to swab() to sd_swab().
 * 
 * Revision 1.1  1993/08/23  14:25:10  baker
 * Initial revision
 *
*/

#include <stdlib.h>
#include "fitfile.h"
#include "endian.h"

char swap_rrn1_rev[] = {"$Revision: 1.4 $"};

void swap_rrn1(struct FIT_R1 *dd, struct FIT_R1 *sd)
{
swab_dword(&dd->rrn, &sd->rrn);
swab_dword(&dd->rtime, &sd->rtime);
swab_dword(&dd->r_xflag, &sd->r_xflag);
memcpy(&(dd->r_range_list[0]), &(sd->r_range_list[0]), 50);
sd_swab(&dd->r_pwr_l, &sd->r_pwr_l, 950);
return;
}


