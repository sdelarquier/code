/*	This file reads an old Data General style FIT data file */

/*
$Log:	read_fit_dg.c,v $
 * Revision 1.2  94/09/06  10:31:41  10:31:41  baker (Kile Baker S1G)
 * removed path portion of includes
 * 
 * Revision 1.1  1993/08/20  20:44:20  baker
 * Initial revision
 *
*/

#include <stdio.h> 
#include <stdlib.h>
#include <errno.h>
#include "fitfile.h"
#include "endian.h"
#include "fit_errno.h"

static short int dg_pulse_pat[PULSE_PAT_LEN] = {0, 2, 3, 7, 13, 15, 16, 0,
	0, 0, 0, 0, 0, 0, 0, 0};
static short int dg_lag_tab[2][LAG_TAB_LEN] = { {7, 2, 0, 0, 3, 2, 7, 0, 7,
	7, 3, 2, 3, 0, 2, 0, 0}, {7, 3, 2, 3, 7, 7, 13, 7, 15, 16, 13, 13,
	15, 13, 16, 15, 16} };

extern void swap_dg0();
extern void swap_rrn1();

char read_fit_dg_rev[] = {"$Revision: 1.2 $"};

int read_fit_dg( FIT_FILE *fp, struct FIT_DATA *fd  )
{
union INX_REC inx_rec, inx_rec1;
union FIT_REC fit_rec, fit_rec1;

int i, j, k, np, nd, nsel, etst, iostat;
long int *ip1, *ip2;


/* first read the next index record */

iostat = read( fp->inxdes, &inx_rec1, sizeof(inx_rec));
if (iostat == -1) {
  perror("I/O error while reading index file");
  return (iostat);
  }
else if (iostat == 0) return (-1);
else if (iostat != sizeof(inx_rec)) {
	printf("I/O error reading index file. Bytes read = %d\n", iostat);
	return (-1);
	}

fp->inx_offset = fp->inx_offset + sizeof(inx_rec);

/* see if we are using a big endian machine.  If so, we have to
	swap bytes and words */

if (endian(&etst) == 0) 
	for (i=0; i< 4; ++i) {
                ip1 = &(inx_rec.iv[i]);
		ip2 = &(inx_rec1.iv[i]);
		swab_dword(ip1,ip2);
		}
else for (i=0; i<4; ++i) inx_rec.iv[i] = inx_rec1.iv[i];


if ((inx_rec.index.recno -1)* fp->fit_recl  != fp->fit_offset) {
	printf("Inconsistent record numbers\n");
	printf("record number from index file will be used\n");
	fp->fit_offset = (inx_rec.index.recno - 1) * fp->fit_recl;
	lseek(fp->fitdes, fp->fit_offset, SEEK_SET);
	}

/* ok, we've got the index record, and we're at the beginning of the FIT
	data */

/* read the first record of the FIT file */

iostat = read( fp->fitdes, &fit_rec1, sizeof(fit_rec));
if (iostat != sizeof(fit_rec)) {
	perror("I/O error reading FIT file ");
	return errno;
	}

fp->fit_offset = fp->fit_offset + sizeof(fit_rec);

/* now check the type of machine again.  If it is a big endian machine
	we have to do some byte swapping */

if (endian(&etst) == 0) {
	swap_dg0(&fit_rec, &fit_rec1);
	}
else memcpy(&fit_rec, &fit_rec1, sizeof(fit_rec));

/*  OK, relative record 0 should be ready to process */
/*  move the data into the FIT_DATA structure */

if (fit_rec.dg.rrn != 0) {
	printf("Could not find relative record 0 in FIT file\n");
	return RDFIT_INV_PARM_RECORD;
	}

fp->ctime = fit_rec.dg.rtime;
fd->rec_time = fit_rec.dg.rtime;


/* now get the noise values */

fd->noise_lev = fit_rec.dg.r_noise;
fd->noise_lag0 = fit_rec.dg.r_noise_lag0;
fd->noise_vel = fit_rec.dg.r_noise_vel;

/* now get the lag0 powers */

for (i=0; i<50; ++i) {
	fd->pwr_lag0[i] = fit_rec.dg.r_pwr_0[i]/100.0;
	fd->slist[i] = fit_rec.dg.r_slist[i];
	}

/* now create the pulse pattern table and the lag_table */

for (i=0; i < PULSE_PAT_LEN; ++i)
	fd->PULSE_PATTERN[i] = dg_pulse_pat[i];

for (j=0; j<2; ++j)
  for (i=0; i < LAG_TAB_LEN; ++i)
	fd->LAG_TABLE[j][i] = dg_lag_tab[j][i];

/* new generate the radops 386 parameter structure from the Data General
	parameter list */


/* the old DG data was often messed up when it comes to the station id
	the following lines fix this */

if (fit_rec.dg.p.TX.TXST.ST_ID == 3) fd->p.ST_ID = 1;
else if (fit_rec.dg.p.TX.TXST.ST_ID == 5) fd->p.ST_ID = 1;
else if (fit_rec.dg.p.TX.TXST.ST_ID == 6) fd->p.ST_ID = 1;
else fd->p.ST_ID = fit_rec.dg.p.TX.TXST.ST_ID;

fd->p.NPARM = fit_rec.dg.p.NPARM;
fd->p.YEAR = fit_rec.dg.p.YEAR;
fd->p.MONTH = fit_rec.dg.p.MONTH;
fd->p.DAY = fit_rec.dg.p.DAY;
fd->p.HOUR = fit_rec.dg.p.HOUR;
fd->p.MINUT = fit_rec.dg.p.MINUT;
fd->p.SEC = fit_rec.dg.p.SEC;
fd->nsel = fit_rec.dg.p.NSEL;
fd->p.NAVE = fit_rec.dg.p.NAVE;
fd->p.ATTEN = fit_rec.dg.p.ATTEN;
fd->p.LAGFR = fit_rec.dg.p.LAGFR;
fd->p.SMSEP = fit_rec.dg.p.SMSEP;
fd->p.ERCOD = fit_rec.dg.p.ERCOD;
fd->p.NOISE = fit_rec.dg.p.NOISE;
fd->p.INTT = fit_rec.dg.p.INTT;
fd->p.TXPL = fit_rec.dg.p.TXPL;
fd->p.MPINC = fit_rec.dg.p.MPINC;
fd->p.MPLGS = fit_rec.dg.p.MPLGS;
fd->p.NRANG = fit_rec.dg.p.NRANG;
fd->p.BMNUM = fit_rec.dg.p.BMNUM;
fd->p.XCF = fit_rec.dg.p.XCF;
fd->p.TFREQ = fit_rec.dg.p.FMhz *1000 + fit_rec.dg.p.FKhz;
fd->p.MXPWR = fit_rec.dg.p.MXPWR * 65536L;
fd->p.LVMAX = fit_rec.dg.p.LVMAX;
fd->p.RSEP = (fit_rec.dg.p.SMSEP * 3) / 20;
fd->p.FRANG = (fit_rec.dg.p.LAGFR * 3) / 20;

/*	set the common program to 0 and set the scan variable based on
	the beam number */

fd->p.CP = 0;
fd->p.SCAN = 0;
if (fd->p.BMNUM == 0) fd->p.SCAN = 1;

/* now set the number of lags used in each fit to 0 to indicate that
	this value is invalid for DG type fit files */

for (i=0; i < 50; ++i) fd->num_lags[i] = 0;

/* -------------  done with relative record 0 ------------------------- */

/* 	Now start reading the regular data records   */

for (i=0; i<50; ++i) fd->qflag[i] = fd->x_qflag[i] = 0;

nd = 0;
nsel = fit_rec.dg.p.NSEL;

while (nd < nsel) {
	np = 0;
	iostat = read(fp->fitdes, &fit_rec1, sizeof(fit_rec));
	if (iostat != sizeof(fit_rec)) {
		perror ("Unable to read FIT record\n");
		printf("time = %4d/%2d/%2d %2d:%2d:%2d\n",
		  fd->p.YEAR, fd->p.MONTH, fd->p.DAY,
		  fd->p.HOUR, fd->p.MINUT, fd->p.SEC);
		}
	fp->fit_offset = fp->fit_offset + sizeof(fit_rec);

	/* again, check to see if we are on a big endian machine */

	if (endian(&etst) == 0) {
		swap_rrn1(&fit_rec, &fit_rec1);
		}
	else memcpy(&fit_rec, &fit_rec1, sizeof(fit_rec));

	/* now check the relative record number */
	if (fit_rec.r1.rrn == 0 || fit_rec.r1.r_xflag) 
		return RDFIT_INV_RECORD;

	/* check the record time */
	if (fit_rec.r1.rtime != fd->rec_time) return RDFIT_INV_TIME;

	/* now transfer the data */

	while ((np < 25) && (nd < nsel)) {
		k = fit_rec.r1.r_range_list[np];
		if (k != fd->slist[nd]) return RDFIT_INV_RANGE;
		--k;
		fd->qflag[k] = fit_rec.r1.r_qflag[np];
		fd->pwr_l[k] = fit_rec.r1.r_pwr_l[np]/100.0;
		fd->pwr_l_err[k] = fit_rec.r1.r_pwr_l_err[np]/100.0;
		fd->pwr_s[k] = fit_rec.r1.r_pwr_s[np]/100.0;
		fd->pwr_s_err[k] = fit_rec.r1.r_pwr_s_err[np]/100.0;
		fd->vel[k] = fit_rec.r1.r_vel[np]/10.0;
		fd->vel_err[k] = fit_rec.r1.r_vel_err[np]/10.0;
		fd->width_l[k] = fit_rec.r1.r_w_l[np]/10.0;
		fd->width_l_err[k] = fit_rec.r1.r_w_l_err[np]/10.0;
		fd->width_s[k] = fit_rec.r1.r_w_s[np]/10.0;
		fd->width_s_err[k] = fit_rec.r1.r_w_s_err[np]/10.0;
		fd->stnd_dev_l[k] = fit_rec.r1.r_sdev_l[np]/1000.0;
		fd->stnd_dev_s[k] = fit_rec.r1.r_sdev_s[np]/1000.0;
		fd->stnd_dev_phi[k] = fit_rec.r1.r_sdev_phi[np]/100.0;
		if (fit_rec.r1.r_sdev_phi[np] == 32767) 
					fd->stnd_dev_phi[k] =32767.0;
		fd->gscat[k] = fit_rec.r1.r_gscat[np];
		++np;
		++nd;
		}
	}

/*  we've gotten all the ACF data.  Now get the XCF data if there is any */

nd = 0;

while ((fd->p.XCF != 0) && (nd < nsel)) {
	np = 0;
	iostat = read(fp->fitdes, &fit_rec1, sizeof(fit_rec));
	if (iostat != sizeof(fit_rec)) {
		perror ("Unable to read FIT record\n");
		printf("time = %4d/%2d/%2d %2d:%2d:%2d\n",
		  fd->p.YEAR, fd->p.MONTH, fd->p.DAY,
		  fd->p.HOUR, fd->p.MINUT, fd->p.SEC);
		}
	fp->fit_offset = fp->fit_offset + sizeof(fit_rec);

	/* again, check to see if we are on a big endian machine */

	if (endian(&etst) == 0) {
		swap_rrn1(&fit_rec, &fit_rec1);
		}
	else memcpy(&fit_rec, &fit_rec1, sizeof(fit_rec));

	/* now check the relative record number */
	if (fit_rec.r1.rrn == 0 || fit_rec.r1.r_xflag == 0) {
		return RDFIT_INV_RECORD;
		}
	/* check the record time */
	if (fit_rec.r1.rtime != fd->rec_time) return RDFIT_INV_TIME;

	/* now transfer the data */

	while ((np < 25) && (nd < nsel)) {
		k = fit_rec.r1.r_range_list[np];
		if (k != fd->slist[nd]) return RDFIT_INV_RANGE;
		--k;
		fd->x_qflag[k] = fit_rec.r1.r_qflag[np];
		fd->x_pwr_l[k] = fit_rec.r1.r_pwr_l[np]/100.0;
		fd->x_pwr_l_err[k] = fit_rec.r1.r_pwr_l_err[np]/100.0;
		fd->x_pwr_s[k] = fit_rec.r1.r_pwr_s[np]/100.0;
		fd->x_pwr_s_err[k] = fit_rec.r1.r_pwr_s_err[np]/100.0;
		fd->x_vel[k] = fit_rec.r1.r_vel[np]/10.0;
		fd->x_vel_err[k] = fit_rec.r1.r_vel_err[np]/10.0;
		fd->x_width_l[k] = fit_rec.r1.r_w_l[np]/10.0;
		fd->x_width_l_err[k] = fit_rec.r1.r_w_l_err[np]/10.0;
		fd->x_width_s[k] = fit_rec.r1.r_w_s[np]/10.0;
		fd->x_width_s_err[k] = fit_rec.r1.r_w_s_err[np]/10.0;
		fd->phi0[k] = fit_rec.r1.r_phi0[np]/100.0;
		fd->phi0_err[k] = fit_rec.r1.r_phi0_err[np]/100.0;
		fd->elev[k] = fit_rec.r1.r_elev[np]/100.0;
		fd->elev_low[k] = fit_rec.r1.r_el_low[np]/100.0;
		fd->elev_high[k] = fit_rec.r1.r_el_high[np]/100.0;

		fd->x_stnd_dev_l[k] = fit_rec.r1.r_sdev_l[np]/1000.0;
		fd->x_stnd_dev_s[k] = fit_rec.r1.r_sdev_s[np]/1000.0;
		fd->x_stnd_dev_phi[k] = fit_rec.r1.r_sdev_phi[np]/100.0;
		if (fit_rec.r1.r_sdev_phi[np] == 32767) 
					fd->x_stnd_dev_phi[k] =32767.0;
		++np;
		++nd;
		}
	}

/*  ALL DONE */

return EOK;
}

