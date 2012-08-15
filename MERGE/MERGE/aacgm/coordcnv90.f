	program coordcnv
c
c       This program is a short driver program for the Altitude Adjusted
c       Corrected Geomagnetic Coordinates routines.  It demonstrates how 
c       the routine "cnv$coord" is to be called.
c
c       NOTE:  the UNIX and VMS versions of this program are slightly
c       different because of the way end-of-file is signaled in the
c       two operating systems.  The code for both versions is included
c       here, but one set should be commented out.
c
c       $Revision: 1.1 $
c**********************************************************************
c       $Log:	coordcnv.f,v $
c Revision 1.1  94/10/14  10:49:09  10:49:09  baker (Kile Baker S1G)
c Initial revision
c 
c

	real*4 glat,glong,mlat,mlong,h,r,mlt,t
	integer*4 t0,cnv_mdhms_sec
	integer*2 yr,mn,day,hr,min,sec
	character*1 chr
	integer*2 err,mgflag
	logical*1 ascii_flag/.TRUE./
	type *,'Coordinate Conversion Program using Spherical Harmonics'
	type *,'This is the 1990 era version!'
	call aacgm_init('/project/radar/pgm/aacgm_coeffs1990.asc',ascii_flag)
	type *,' '
5	type *,'enter 1 for geographic to geomagnetic and 2 for'
	type *,'geomagnetic to geographic'
	accept *,mgflag
	type *,'enter height (km)'
	accept *,h
c	type *,'enter time(yr,month,day,hr,min,sec)'
c	accept *,yr,mn,day,hr,min,sec
c	t0 = cnv_mdhms_sec(yr,mn,day,hr,min,sec)

10	type *,'enter input latitude and longitude (cntrl-d to exit)'
c10	type *,'enter input latitude and longitude (cntrl-z to exit)'
	read(5,*,end=99)glat,glong
	call cnv$coord(glat,glong,h,4,mlat,mlong,r,mgflag,err)
c	if (mgflag.eq.1)then
c		t=mlt(yr,t0,mlong,mslong)
c	else
c		t=mlt(yr,t0,glong,mslong)
c	end if
	type *,'converted lat and long:',mlat,mlong,'  err=',err
c       type *,'MLT = ',t
	
	goto 10
c
c       UNIX systems use control-d to signal end-of-file
c
99	type *,'enter another cntrl-d to quit or any other character'
	type *,'to change parameters.'
	read(5,1,end=98)chr
1	format(a)
c
c       VMS systems use control-z to signal end-of-file
c
c99	type *,'enter another cntrl-z to quit or any other character'
c	type *,'to change parameters.'
c	read(5,1,end=98)chr
c1	format(a)

	goto 5
98	stop 'normal exit'
	end

