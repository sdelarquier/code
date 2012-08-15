/* this file contains the definitions for radops parameters.
   the pulse pattern table, lag definition table, and hardware
	paramter structure, and the rawdata structure */

#ifndef _RADOPS_H_INCLUDED

#define CLOCK_PERIOD	10		/* clock period in microseconds */

#define PULSE_PAT_LEN	16		/* max length of pulse pattern table */
#define LAG_TAB_LEN		48		/* max length of lag table */


#define COMBF_SIZE		80		/* comment buffer size */
#define MAX_RANGE		75		/* max number of ranges */
/* ----------------------------------------------------------------------- */
/* The following structure defines the RADOPS parameter block
	which is written at the beginning of each data record */

struct radops_parms
	{
	/* The first set of parameters are set by the RADOPS system */
	/* There are a total of 24 words (16 bit) in the system parameter list */

	struct	
		{
		char MAJOR,MINOR;
		} REV;/* REV.MAJOR is the major revision #, REV.MINOR is the minor # */

	short int 
		NPARM,	/* the total number of 16-bit words in the parameter block */
		ST_ID,	/* station ID code: 1=Goose Bay, 2=Schefferville,
					4=Halley Station, 8=Syowa */
		YEAR,	/* date and start time of the record.  Year=19xx */
		MONTH,
		DAY,
		HOUR,
		MINUT,
		SEC,
		TXPOW,	/* transmitted power (kW) */
		NAVE,	/* number of times the pulse sequence was transmitted */
		ATTEN,	/* attenuation setting of the receiver (0-3) */
		LAGFR,	/* the lag to the first range (microsecs) (see note 2 below) */
		SMSEP,	/* the sample separation (microsecs) (see note 2) */
		ERCOD,	/* error flag (see error definitions) */
		AGC_STAT,	/* AGC status word */
		LOPWR_STAT,	/* Low power status word */
		radops_sys_resS; /* reserved for future use */
	long
		NOISE,	/* the noise level determined during the clear freq. search */
		radops_sys_resL[2];  /* reserved for future use */

	/* The second set of parameters are set by the user */	
	/* These parameters can either be set manually or by a RADLANG program */
	/* There are a total of 24 words in this parameter list */
	short int
		INTT,	/* the integration period */
		TXPL,	/* the pulse length (in micro seconds) */
		MPINC,	/* the basic lag separation (in microseconds) */
		MPPUL,	/* the number of pulses in the pulse pattern */
		MPLGS,	/* the number of lags in the pulse pattern (see note 1 below) */
		NRANG,	/* the number of range gates */
		FRANG,	/* the distance to the first range (see note 2 below) */
		RSEP,	/* the range separation (in km) */
		BMNUM,	/* beam number */				
		XCF,	/* flag indicating if cross-correlations were being done */
		TFREQ,	/* the transmitted frequency (in KHz) */
		SCAN;	/* flag indicating the scan mode  */
	long
		MXPWR,	/*	Maximum power allowed (see note 3) */
		LVMAX;	/*  Maximum noise level allowed (see note 3) */
	/* user defined variables */
	long
		usr_resL1,
		usr_resL2;
	short int
		CP,
		usr_resS1,
		usr_resS2,
		usr_resS3;
	};
/*	The total length of the parameter block is 48 words */
/*  This does not include the length of the comment buffer,
	the pulse table and the lag table */

/* ---------------------- NOTES ------------------------------ */
/*
	1.	The number of lags in the pulse pattern is the true number of
		lags which are present in the table LAG_TABLE.  It is NOT the
		value of the maximum lag.  If the maximum lag is 33 but only
		22 out of the 33 lags are actually calculated then MPLGS is 22.

	2.	The user set the first range gate by specifying FRANG in km.
		The system uses this value to set the lag to the first range
		in microseconds.
    	
		Similarly, the user sets the range separation by specifying RSEP
		in km.  The system uses this value to set SMSEP in terms of microsecs.

	3.	During the gain setting routine, the system will attempt to add
		enough attenuation so that the maximum reflected power is less than
		MXPWR.  If this is not possible, the system will set the error code
		(ERCOD) to indicate the receiver overload condition.
	
		During the clear frequency search, the system will find the clearest
		frequency in the range specified.  The Noise level determined for
		that frequency will be stored in the parameter NOISE.  If NOISE is
		greater than LVMAX, the error code will be set to indicate the
		no clear frequency condition.
*/

struct rawdata
	{
	struct radops_parms PARMS;
	short int PULSE_PATTERN[PULSE_PAT_LEN];
	short int LAG_TABLE[2][LAG_TAB_LEN];
	char COMBF[COMBF_SIZE];
	long pwr0[MAX_RANGE];
	long acfd[MAX_RANGE][LAG_TAB_LEN][2];
	long xcfd[MAX_RANGE][LAG_TAB_LEN][2];
	};

/* -------------------------------------------------------------------- */
/*		Definition of Error code bits */
#define NO_CLEAR_FREQ 4
#define RECEIVER_OVERLOAD 8


/* define a tsg structure */
struct tsg_parms
	{
	short int n_times, 	/* number of times this sequence has been used */
	nrang,          /* number of ranges */
	frang,          /* distance to first range */
	range_sep,      /* range gate separation */
	npulse,         /* number of pulses in the sequence */
	mpinc,			/* multi-pulse increment */
	mlag,           /* maximum lag in the sequence */
	pulse_len,      /* length of a pulse */
	samples,        /* number of samples */
	*pattern;       /* pointer to the pulse pattern */
	};

/* ---------------------------------------------------------------------- */

/* The following structure defines the hardware parameters that are
	required to define the location and operation of the radar.  these
	parameters are actually read in from a file */

struct SD_HARDWARE {
	double geo_lat, geo_long, altitude; /* the location of the radar */
	double boresite, beam_sep; /* the beam pattern of the radar */
	double vdir;	/* the definition of velocity direction (+1.0 is the
						normal case, -1.0 indicates the I & Q inputs to
						the receiver were reversed). */
	double atten;	/* the attenuation step in dB */
	double tdiff;	/* the time delay in microsec of the signal from
						the interferometer array and the main array */
	double phidiff;	/* the sign of the phase shift of the interferometer
						signal.  A +1 indicates the interferometer array
						is in front of the main array */
	double interfer_pos[3]; /* the x, y, z position of the interferometer
								array with respect to the main array.
								the values are given in meters.  x is
								in the direction parallel to the main
								array, with +x being to the right, as you
								look along the boresite.  y is in the 
								boresite direction, so a + value indicates
								that the interferometer array is in front
								of the main array.  z completes the right
								handed coordinate system and is positive
								upwards */
	double rec_rise;	/* the rise time of the receiver in microsec */
	};

/* ------------------------------------------------------------------- */
 
#define _RADOPS_H_INCLUDED
#endif
