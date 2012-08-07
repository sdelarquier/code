c****************************************************************************
	subroutine GEOCNVRT(gdlat,gdlon,xal,xel,ral,rel)

c Converts from pointing azimuth and elevation angles (xal,xel) measured
c with respect to the local horizon to azimuth and elevation angles (ral,rel)
c appropriate to a horizon defined by the plane perpendicular to the earth-
c centred radial vector drawn through the point of origin.
c (This is an adjustment to the measured azimuth and elevation for the
c oblateness of the earth.)

c The conversion is effected by means of a cartesian coordinate transformation 
c that rotates the local vertical onto the direction of the earth-centred
c radial vector. The angle of rotation about the x axis is the deviation of
c the vertical.
c (x: eastward y:northward z: upward)

c       $Log:	geocnvrt.f,v $
c Revision 1.1  94/08/03  16:12:21  16:12:21  miker (Michael Ruohoniemi S1G)
c Initial revision
c 

	real*4 kxg,kyg,kzg,kxr,kyr,kzr
	kxg	 =  cos( 0.017453292* xel)* sin( 0.017453292* xal)
	kyg	 =  cos(  0.017453292* xel)*cos( 0.017453292* xal)
	kzg	 =  sin( 0.017453292* xel)
        call geodtgc(1,gdlat,gdlon,rrad,rlat,rlon,del)
	kxr	 =  kxg
	kyr	 =  kyg*cos( 0.017453292* del) + kzg*sin( 0.017453292* del)
	kzr	 = -kyg*sin( 0.017453292* del) + kzg*cos( 0.017453292* del)
	ral	 =  57.29577951* atan2(kxr,kyr)
	rel      =  57.29577951* atan(kzr/sqrt(kxr**2+kyr**2))
	return
	end 
