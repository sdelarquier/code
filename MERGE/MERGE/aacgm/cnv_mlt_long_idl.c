/* File: ~/pgm/cnv_mlt_long_idl.c */
/* Last Modification: 22-NOV-2000 */
/* Author: Dieter Andre'          */

/* idl interface to cnv_mlt_long.f */

int cnv_mlt_long_idl( int argc, void *argv[] )

{
  extern void cnv_mlt_long( short int* iyr, int* yrsec, float* mlt, float* pt_mlat, float* pt_glat, float* pt_glong);

  short int* iyr;
  int* yrsec;
  float* mlt;
  float* pt_mlat;
  float* pt_glat;
  float* pt_glong;
    
  iyr=  (short int *) argv[0];
  yrsec=      (int *) argv[1];
  mlt=      (float *) argv[2];
  pt_mlat=  (float *) argv[3];
  pt_glat=  (float *) argv[4];
  pt_glong= (float *) argv[5];

  cnv_mlt_long( iyr, yrsec, mlt, pt_mlat, pt_glat, pt_glong);
  return 0;
} /* cnv_mlt_long_idl */
