#include "radops.h"
#include <stdio.h>
#include "endian.h"

void swap_rawparms(dst, src)
	struct radops_parms *dst, *src;
{
dst->REV.MAJOR = src->REV.MAJOR;
dst->REV.MINOR = src->REV.MINOR;
swab_word(&dst->NPARM,&src->NPARM);
swab_word(&dst->ST_ID,&src->ST_ID);
swab_word(&dst->YEAR,&src->YEAR);
swab_word(&dst->MONTH,&src->MONTH);
swab_word(&dst->DAY,&src->DAY);
swab_word(&dst->HOUR,&src->HOUR);
swab_word(&dst->MINUT,&src->MINUT);
swab_word(&dst->SEC, &src->SEC);
swab_word(&dst->TXPOW, &src->TXPOW);
swab_word(&dst->NAVE, &src->NAVE);
swab_word(&dst->ATTEN, &src->ATTEN);
swab_word(&dst->LAGFR, &src->LAGFR);
swab_word(&dst->SMSEP, &src->SMSEP);
swab_word(&dst->ERCOD, &src->ERCOD);
swab_word(&dst->AGC_STAT, &src->AGC_STAT);
swab_word(&dst->LOPWR_STAT, &src->LOPWR_STAT);
swab_word(&dst->radops_sys_resS, &src->radops_sys_resS);
swab_dword(&dst->NOISE, &src->NOISE);
swab_dword(&dst->radops_sys_resL[0], &src->radops_sys_resL[0]);
swab_dword(&dst->radops_sys_resL[1], &src->radops_sys_resL[1]);
swab_word(&dst->INTT, &src->INTT);
swab_word(&dst->TXPL, &src->TXPL);
swab_word(&dst->MPINC, &src->MPINC);
swab_word(&dst->MPPUL, &src->MPPUL);
swab_word(&dst->MPLGS, &src->MPLGS);
swab_word(&dst->NRANG, &src->NRANG);
swab_word(&dst->FRANG, &src->FRANG);
swab_word(&dst->RSEP, &src->RSEP);
swab_word(&dst->BMNUM, &src->BMNUM);
swab_word(&dst->XCF, &src->XCF);
swab_word(&dst->TFREQ, &src->TFREQ);
swab_word(&dst->SCAN, &src->SCAN);
swab_dword(&dst->MXPWR, &src->MXPWR);
swab_dword(&dst->LVMAX, &src->LVMAX);
swab_dword(&dst->usr_resL1, &src->usr_resL1);
swab_dword(&dst->usr_resL2, &src->usr_resL2);
swab_word(&dst->CP, &src->CP);
swab_word(&dst->usr_resS1, &src->usr_resS1);
swab_word(&dst->usr_resS2, &src->usr_resS2);
swab_word(&dst->usr_resS3, &src->usr_resS3);
return;
}
