/*  This routine fixes the byte order for the parameter records (reletive record 0)
    for the old Data General FIT files.  The byte re-oredering must be done on gib endian
    machines */
/*
$Log:	swap_dg0.c,v $
 * Revision 1.4  94/09/06  11:21:45  11:21:45  baker (Kile Baker S1G)
 * elimnated paths
 * 
 * Revision 1.2  93/10/18  15:38:46  15:38:46  bristow (Bill Bristow S1G)
 * Revised to change call to swab() to sd_swab().
 * 
 * Revision 1.1  1993/08/23  14:12:53  baker
 * Initial revision
 *
*/

#include <stdlib.h>
#include "fitfile.h"
#include "endian.h"

char swap_dg0_rev[] = {"$Revision: 1.4 $"};

void swap_dg0(struct FIT_DG *dd, struct FIT_DG *sd)
{
swab_dword(&(dd->rrn), &(sd->rrn));
swab_dword(&(dd->rtime), &(sd->rtime));
sd_swab(&dd->p, &sd->p, 22);
dd->p.TX.TXST.TXDB = sd->p.TX.TXST.TXDB;
dd->p.TX.TXST.ST_ID = sd->p.TX.TXST.ST_ID;
sd_swab(&dd->p.TXPL, &sd->p.TXPL, 30);
dd->p.NREQ = sd->p.NREQ;
dd->p.NSEL = sd->p.NSEL;
sd_swab(&dd->p.XCF, &sd->p.XCF, 4);
memcpy(&dd->combf, &sd->combf, 80);
swab_dword(&(dd->r_noise), &(sd->r_noise));
swab_dword(&(dd->r_noise_lag0), &(sd->r_noise_lag0));
swab_dword(&(dd->r_noise_vel), &(sd->r_noise_vel));
sd_swab(&dd->r_pwr_0, &sd->r_pwr_0, 200);
return; 
}
