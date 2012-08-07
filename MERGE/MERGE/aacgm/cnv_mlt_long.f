C File: ~/pgm/cnv_mlt_long.f
C Last Modification: 22-NOV-2000
C Author: Dieter Andre

C Given time, magnetic loacl time and magnetic latitude of a point,
C calculate the geographic position of the point

	SUBROUTINE cnv_mlt_long( iyr, yrsec, mlt, pt_mlat, pt_glat, pt_glong)

C	Inputs:
	INTEGER*2 iyr		! Year
	INTEGER*4 yrsec		! Second of the year [UT]
	REAL*4 mlt		! Magnetic Local Time [h]
	REAL*4 pt_mlat		! Geomagnetic Latitude [deg]

C	Outputs:
	REAL*4 pt_glat		! Geographic Latitude [deg]
	REAL*4 pt_glong		! Geographic Longitude [deg]

	INTEGER*2 err, mgflag
	REAL*4 h, r 
	REAL*4 sol_glong, sol_glat
	REAL*4 sol_mlong, sol_mlat

	h= 300.0
C	Geographic Position of the sun
	CALL solar_loc( iyr, yrsec, sol_glong, sol_glat)
C	Convert to geomagnetic
	mgflag= 1
	CALL cnv$coord( sol_glat, sol_glong, h, 4, sol_mlat, sol_mlong, r, mgflag, err)
C	Geomagnetic Longitude of Point
	pt_mlong= (mlt - 12)* 15.0 + sol_mlong
	IF (pt_mlong .GT. 360.0) pt_mlong= pt_mlong - 360.0
	IF (pt_mlong .LT. -360.0) pt_mlong= pt_mlong + 360.0
C	Convert to geographic
	mgflag= 2
	CALL cnv$coord( pt_mlat, pt_mlong, h, 4, pt_glat, pt_glong, r, mgflag, err)
	END