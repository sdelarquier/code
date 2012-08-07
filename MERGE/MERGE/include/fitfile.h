/*	This header file defines the FIT_FILE file structure as well
as the structure of the data in a fit file. */
/*
$Log: fitfile.h,v $
 * Revision 1.3  1996/03/07  20:06:32  baker
 * Added an additional element to the FITFILE structure.  The
 * new item, "inx_yr_offset" normally contains a zero.  If
 * the file spans a year boundary however, it will contain
 * the number of seconds in a year.  This is used by
 * find_fit_rec.
 *
 * Revision 1.2  1995/05/12  10:41:31  baker
 * removed an explicit reference to /project/radar/include
 *
 * Revision 1.1  95/02/21  10:36:54  10:36:54  baker (Kile Baker S1G)
 * Initial revision
 * 
*/

#ifndef FITFILE_H_INCLUDED

#define FIT_RECL 1024	/* record length for FIT files */
#define INX_RECL 16	/* record length for INX files */
#define RNG_PER_REC 25  /* number of ranges per 1024 byte data record */

/* definitions for the 386 v1.1
*/
#define V11_PULSE_PAT_LEN	7
#define V11_LAG_TAB_LEN		17

#include "radops.h"

/* define the fitfile structure */

typedef struct FIT_FILE_INFO	 {
	int fitdes, inxdes;	/* unix file descriptors for the FIT and
					INX files */
	int fit_recl, inx_recl;	/* record lengths */

	char fname[127];	/* full path name of the FIT file */

	char source[6];	/* computer system that created the file
			    legal values include: "DG", "QNX", "VAX"
				"UNIX", as well as special programs
				that edit or manufacture FIT files */
	char version[24];  /* the string containing the version number
				of the program that created the file */
	int (*fit_read)();	  /* pointer to a subroutine that knows how to
				read this data file */
	long fit_offset, inx_offset;  /* current byte position in the files */
	long inx_srec, inx_erec;  /* first and last records of index file */
	long ctime;	/* time of the last record read */
	long stime, etime; /* start time and end time of the file */
	long inx_yr_offset; /* time offset if spanning a year boundary */ 
	} FIT_FILE;

/*  Now define the structure of a single 1024 byte FIT record */

struct DG_PARMS {
	short int NPARM;
	short int YEAR;
	short int MONTH;
	short int DAY;
	short int HOUR;
	short int MINUT;
	short int SEC;
	short int INTT;
	short int NAVE;
	short int FMhz;
	short int FKhz;
	union {
	  short int TWPOW;
	  struct {
	    char TXDB;
	    char ST_ID;
	    } TXST;
	  } TX;
	short int TXPL;
	short int MPINC;
	short int MPPUL;
	short int MPLGS;
	short int NRANG;
	short int LAGFR;
	short int SMSEP;
	short int BMNUM;
	short int MXPWR;
	short int ATTEN;
	short int MODE;
	short int DPARM;
	short int LVMAX;
	short int ERCOD;
	short int NOISE;
	char NREQ;
	char NSEL;
	short int XCF;
	short int NCHAR;
	};

struct FIT_HEADER {
	short int fit_len;
	short int inx_len;
	char info[FIT_RECL - 4];
	};

struct FIT_DG {
	long rrn;
	long rtime;
	struct DG_PARMS p;
	char combf[80];
	long r_noise;
	long r_noise_lag0;
	long r_noise_vel;
	short int r_pwr_0[50];
	short int r_slist[50];
	};

struct FIT_V13_R0 {
	long rrn;
	long rtime;
	struct radops_parms r_parm;
	short int r_ppat[PULSE_PAT_LEN];
	short int r_lagtable[2][LAG_TAB_LEN];
	char r_combf[COMBF_SIZE];
	long r_noise, r_noise_lag0, r_noise_vel;
	short int r_pwr_0[75];
	short int r_slist[75];
	char r_numlags[75];
	};

struct FIT_V11_R0 {
	long rrn;
	long rtime;
	struct radops_parms r_parm;
	short int r_ppat[V11_PULSE_PAT_LEN];
	short int r_lagtable[2][V11_LAG_TAB_LEN];
	char r_combf[COMBF_SIZE];
	long r_noise, r_noise_lag0, r_noise_vel;
	short int r_pwr_0[75];
	short int r_slist[75];
	};

struct FIT_R1 {
	long rrn;
	long rtime;
	long r_xflag;
	char r_range_list[25];
	char r_qflag[25];
	short int r_pwr_l[25];
	short int r_pwr_l_err[25];
	short int r_pwr_s[25];
	short int r_pwr_s_err[25];
	short int r_vel[25];
	short int r_vel_err[25];
	short int r_w_l[25];
	short int r_w_l_err[25];
	short int r_w_s[25];
	short int r_w_s_err[25];
	short int r_phi0[25];
	short int r_phi0_err[25];
	short int r_elev[25];
	short int r_el_low[25];
	short int r_el_high[25];
	short int r_sdev_l[25];
	short int r_sdev_s[25];
	short int r_sdev_phi[25];
	short int r_gscat[25];
	};

union FIT_REC {
	char frcrd0[FIT_RECL];
	struct FIT_DG dg;
	struct FIT_HEADER hdr;
	struct FIT_V13_R0 v13_r0;
	struct FIT_V11_R0 v11_r0;
	struct FIT_R1 r1;
	};

/* define the structure of the global (common) variable "procdat" that is
used by FORTRAN routines */

struct PROCDAT {
	long rec_time;
	short int parm_list[70];
	float noise_lev;
	float noise_lag0;
	float noise_vel;
	float pwr_lag0[75];
	short int slist[75];
	long qflag[75];
	float pwr_l[75];
	float pwr_l_err[75];
	float pwr_s[75];
	float pwr_s_err[75];
	float vel[75];
	float vel_err[75];
	float width_l[75];
	float width_l_err[75];
	float width_s[75];
	float width_s_err[75];
	float stnd_dev_l[75];
	float stnd_dev_s[75];
	float stnd_dev_phi[75];
	short int gscat[75];
	long x_qflag[75];
	float x_pwr_l[75];
	float x_pwr_l_err[75];
	float x_pwr_s[75];
	float x_pwr_s_err[75];
	float x_vel[75];
	float x_vel_err[75];
	float x_width_l[75];
	float x_width_l_err[75];
	float x_width_s[75];
	float x_width_s_err[75];
	float phi0[75];
	float phi0_err[75];
	float elev[75];
	float elev_low[75];
	float elev_high[75];
	float x_stnd_dev_l[75];
	float x_stnd_dev_s[75];
	float x_stnd_dev_phi[75];
	struct radops_parms parms386;
	short int num_lags[75];
	};

/* the following definition defines the structure that should be used by
C programs and new FORTRAN programs for reading the FIT data */

struct FIT_DATA {
	long rec_time;
	struct radops_parms p;
        short int PULSE_PATTERN[PULSE_PAT_LEN];
        short int LAG_TABLE[2][LAG_TAB_LEN];
        char COMBF[COMBF_SIZE];
	long PAD1;
	double noise_lev;
	double noise_lag0;
	double noise_vel;
	double pwr_lag0[MAX_RANGE];
	short int slist[MAX_RANGE];
        short int nsel;
   	long qflag[MAX_RANGE];
	long PAD2;
	double pwr_l[MAX_RANGE]; 
	double pwr_l_err[MAX_RANGE]; 
	double pwr_s[MAX_RANGE]; 
	double pwr_s_err[MAX_RANGE]; 
	double vel[MAX_RANGE]; 
	double vel_err[MAX_RANGE]; 
	double width_l[MAX_RANGE]; 
	double width_l_err[MAX_RANGE]; 
	double width_s[MAX_RANGE]; 
	double width_s_err[MAX_RANGE]; 
	double stnd_dev_l[MAX_RANGE]; 
	double stnd_dev_s[MAX_RANGE]; 
	double stnd_dev_phi[MAX_RANGE]; 
	short int gscat[MAX_RANGE];
	short int PAD3;
	long x_qflag[MAX_RANGE];
	long PAD4;
	double x_pwr_l[MAX_RANGE]; 
	double x_pwr_l_err[MAX_RANGE]; 
	double x_pwr_s[MAX_RANGE]; 
	double x_pwr_s_err[MAX_RANGE]; 
	double x_vel[MAX_RANGE]; 
	double x_vel_err[MAX_RANGE]; 
	double x_width_l[MAX_RANGE]; 
	double x_width_l_err[MAX_RANGE]; 
	double x_width_s[MAX_RANGE]; 
	double x_width_s_err[MAX_RANGE]; 
	double phi0[MAX_RANGE]; 
	double phi0_err[MAX_RANGE]; 
	double elev[MAX_RANGE]; 
	double elev_low[MAX_RANGE]; 
	double elev_high[MAX_RANGE]; 
	double x_stnd_dev_l[MAX_RANGE]; 
	double x_stnd_dev_s[MAX_RANGE]; 
	double x_stnd_dev_phi[MAX_RANGE];
	short int num_lags[MAX_RANGE]; 
	};

/* Now define the structure of the INX file */

struct INX_HDR {
	long stime;
	long etime;
	long srec;
	long erec;
	};

struct INX_INDEX {
	long rtime;
	long recno;
	long nrec;
	long xflag;
	};

union INX_REC {
	long int iv[4];
	struct INX_HDR inx_hdr;
	struct INX_INDEX index;
	};

#define FITFILE_H_INCLUDED

#endif

	



