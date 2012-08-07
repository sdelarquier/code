/* File: include/genlib.h */
/* Last Modification: 31-May-2002 */
/* Author: Dieter Andre */

/* array dimension: 3 */
void cartosph( double *car, double *sph);

long cnv_mdhms_sec( short *yr, *mo, *day, *hr, *mn, *sec);

void cnv_sec_mdhms(short *iyr,*mo,*dy,*hr,*min,*sec, long *seconds);

void fldpnt( float *rrho, *rlat, *rlon, *ral, *rel, *r, *frho, 8flat, *flon);

void fldpnth( float *gdlat, *gdlon, *psi, *bore, *fh, *r, *frho, *flat, *flon);

void fldpntn( float *rrho, *rlat, *rlon, *ral, *rel, *r, *frho, *flat, *flon);

/* lat,lon,rho,elv,azc arrays [75,16] in C */
void geochrt( short int *st_id, year, long int *yr_sec, float *frang, *rsep, *height, *lat, *lon, *rho, *elv, *azc);

void geocnvrt( float *gdlat, *gdlon, *xal, *xel, *ral, *rel);

void geocnvrtn( float *gdlat, *gdlon, *xal, *xel, *ral, *rel);

void geodtgc( long int *iopt, float *gdlat, *gdlon, *grho, *glat, *glon, *del);

void glbthor( long int *iopt, float *lat, *lon, *rx, *ry, *rz, *tx, *ty, *tz);

void gs_fldpnth( float *gdlat, *gdlon, *psi, *bore, *fh, *r, *frho, *flat, *flon);

void gsptcar( long int *iopt, float *rho, *lat, *lon, *rx, *ry, *rz);

int hardware_setup( int year, int yr_sec, int station, struct SD_HARDWARE *hd);

void hortmag( long int *iopt, float *bx, *by, *bz, *tx, *ty, *tz, *vx, *vy, *vz);

void hsptcar( long int *iopt, float *r, *az, *el, *tx, *ty, *tz);

void locbg( short int *st_id, short int *year, long int *yr_sec, float *frang, *rsep, *flat, *flon, *height, short int *beam, *gate);

/* what is logical in C ??? */
/* vel, kbad, gscat arrays [dim_2,dim_1] in C */
void median_filter( logical *kbad, float *vel, logical *gscat, short int *ifilt, *dim_1, *dim_2);

/* give it year and day of year, it returns the month and day of month */
void monday( int *year, *doy, *imon, *iday);

void norm_vec( float *rx, *ry, *rz);

void quicksort( float *array, short int *n);

void radar_pos( short int* st_id, short int* year, int* yr_sec, float pos[5]);

/* lat,lon,rho arrays [75,16] in C */
void radcoor( short int *st_id, *year, long int *yr_sec, float *frang, *rsep, *height, *lat, *lon, *rho);

short int *range_gate( float *frang, *rsep, *rxris_km, *range_edge, *slant_range);

void rbpos_gen(	short int *range,		/* Index of range gate, first one is 1	*/
		dhort int *bmnum,		/* beam number [0-15]			*/
		float *first_range,		/* Range to first sample [km]		*/
		float *range_separation,	/* Distance between range gates [km]	*/
		float *height,			/* <90 [deg]; >=90 [km]			*/
		float *pos,			/* see doc				*/
		short int *st_id,		/* station id				*/
		short int *year,		/* of operation				*/
		long int *yr_sec,		/* second of year			*/
		short int *mgflag,
		logical *center_flag,
		logical *gs_flag,
		short int *err);

/* lat,lon,rho,elv,azc arrays [75,16] in C */
void rdkelaz( short int *st_id, *year, long int *yr_sec, float *lat, *lon, *rho, *height, *elv, *azc);

void sd_swab (char * dest, char *source, int num_bytes);

float *slant_range( float *frang, *rsep, *rxris_km, *range_edge, short int *range_gate);

/* vel, kbad arrays [dim_2,dim_1] in C */
void smooth( logical *kbad, float *vel, short int *dim_1, short int *dim_2);

/* array dimension: 3 */
void sphtocar( float *sph, float *car);

/* array dimension: 3 */
void sphtocarvect( float *sph, *sphvect, *carvect);

char *strcrep( char *src, *rlist, rep);

long int * trim( char *ss);

