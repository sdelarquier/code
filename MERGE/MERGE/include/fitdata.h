/*	This header file defines the data structure that is passed to the
	advanced display routines, such as QLTP */

#define FIT_DATA 'f'	/* message type for sending fitdata */

#ifndef MAX_RANGE
#include "/radops/include/radops.h"
#endif

struct range_data		/* this is the data structure for a single range gate */
	{
	int
		qflg, gsct;		/* quality and groundscatter flags */
	double
		p_0,	/* lag 0 power */
		p_l,	/* lamda power */
		p_s,	/* sigma power */
		w_l,	/* lamda width */
		w_s,	/* sigma width */
		v,		/* velocity */
	    v_err,	/* velocity error */
		sdev_l,	/* stnd dev. of lamda fit */
		sdev_s,	/* stnd dev of sigma fit */
		sdev_phi; /* stnd dev of phase fit */
	};

struct fitdata		/* this is the data structure passed to the other routines */
	{
	struct radops_parms prms;	/* radops parameter block */
	struct range_data rng[MAX_RANGE];
	};

