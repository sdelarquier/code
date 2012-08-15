/* File: include/aacgmlib.h */
/* Last Modification: 03-Jun-2002 */
/* Author: Dieter Andre */


void aacgm_init( char filename[80], logical *ascii_flag);


void altitude_to_cgm( float *R_HEIGHT_IN, *R_LAT_ALT, *R_LAT_ADJ);


void cgm_to_altitude( float *R_HEIGHT_IN, *R_LAT_ALT, *R_LAT_ADJ, logical *I_ERR64);


long cnv_mdhms_sec( short *yr, *mo, *day, *hr, *mn, *sec);

void cnv_mlt_long( short int *iyr, long int *yrsec, mlt, float *pt_mlat, *pt_glat, *pt_glong);

void cnv_sec_mdhms( short *iyr,*mo,*dy,*hr,*min,*sec, long *seconds);

void cnv$coord( float *in_lat, *in_long, *height, long int *order,
                float *out_lat, *out_long, *out_r, short int *mgflag, *err);

double eqn_of_time( float *mean_long, short int *iyr);

float *mlt( short int *iyr, long int *t0, float *mlong, *mslong);

float *mlt1( double *T0, float *solar_dec, *mlong1, *mslong);

void rylm( double *D_COLAT, *D_LON, long int *I_ORDER, double *D_YLMVAL);

void sfc$$convert_geo_coord( float *R_LAT_IN, *R_LON_IN, *R_HEIGHT_IN, *R_LAT_OUT, *R_LON_OUT,
				long int *I_FLAG, *I_ERROR );

void solar_loc( short int *yr, long int *t1, float *mean_long, *declination);


