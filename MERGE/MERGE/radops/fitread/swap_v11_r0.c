/*	This routine is used to swap the byte order for FIT file data
	relative record 0 (i.e. the parameter record).  It is for use
	with version 1.10 and 1.2 of QNX fitacf.

	NOTE:  This routine will only be used on big endian machines
	such as the HP-9000s.
*/
/*
$Log:	swap_v11_r0.c,v $
 * Revision 1.4  94/09/06  11:20:15  11:20:15  baker (Kile Baker S1G)
 * elilminated paths
 * 
 * Revision 1.2  93/10/18  15:43:40  15:43:40  bristow (Bill Bristow S1G)
 *  Revised to change call to swab() to sd_swab().
 * 
 * Revision 1.1  1993/08/23  14:21:28  baker
 * Initial revision
 *
*/

#include <stdlib.h>
#include "endian.h"
#include "radops.h"
#include "fitfile.h"

char swap_v11_r0_rev[] = {"$Revision: 1.4 $"};

void swap_v11_r0( struct FIT_V11_R0 *dd, struct FIT_V11_R0 *sd)
{
swab_dword(&dd->rrn, &sd->rrn);
swab_dword(&dd->rtime, &sd->rtime);
dd->r_parm.REV.MAJOR = sd->r_parm.REV.MAJOR;
dd->r_parm.REV.MINOR = sd->r_parm.REV.MINOR;
sd_swab(&dd->r_parm.NPARM, &sd->r_parm.NPARM, 34);
swab_dword(&dd->r_parm.NOISE, &sd->r_parm.NOISE);
swab_dword(&dd->r_parm.radops_sys_resL[0], &sd->r_parm.radops_sys_resL[0]);
swab_dword(&dd->r_parm.radops_sys_resL[1], &sd->r_parm.radops_sys_resL[1]);
sd_swab(&dd->r_parm.INTT, &sd->r_parm.INTT, 24);
swab_dword(&dd->r_parm.MXPWR, &sd->r_parm.MXPWR);
swab_dword(&dd->r_parm.LVMAX, &sd->r_parm.LVMAX);
swab_dword(&dd->r_parm.usr_resL1, &sd->r_parm.usr_resL1);
swab_dword(&dd->r_parm.usr_resL2, &sd->r_parm.usr_resL2);
sd_swab(&dd->r_parm.CP, &sd->r_parm.CP, 8);
sd_swab(&dd->r_ppat, &sd->r_ppat, V11_PULSE_PAT_LEN*2 + V11_LAG_TAB_LEN*4);
memcpy(&dd->r_combf, &sd->r_combf, COMBF_SIZE);
swab_dword(&dd->r_noise, &sd->r_noise);
swab_dword(&dd->r_noise_lag0, &sd->r_noise_lag0);
swab_dword(&dd->r_noise_vel, &sd->r_noise_vel);
sd_swab(&dd->r_pwr_0, &sd->r_pwr_0, 300);
return;
}




