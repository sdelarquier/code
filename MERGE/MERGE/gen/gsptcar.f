c****************************************************************************
	subroutine GSPTCAR(iopt,rho,lat,lon,rx,ry,rz)

c Performs the conversion from global spherical coordinates to global cartesian
c coordinates (iopt >= 0) and the inverse transformation (iopt < 0).

c For the global spherical coordinates, the components are:
c   	rho: distance from the centre of the earth (km)
c   	lat: geocentric latitude (deg)
c   	lon: east longitude      (deg)

c For the global cartesian coordinates, the origin is at the centre of the
c earth, the z axis is in the direction of the rotational axis and through
c the north pole, and the x axis is in the plane of the equator and through
c the prime meridian. All distances are in kilometres.
c
c       $Log:	gsptcar.f,v $
c Revision 1.1  94/08/03  16:20:24  16:20:24  miker (Michael Ruohoniemi S1G)
c Initial revision
c 
c
c****************************************************************************
	implicit none
	integer*4 iopt
	real*4  rho,lat,lon,rx,ry,rz
	real*8 sph(3), car(3)
c
c	convert from real*4 to real*8 so we can use some previously
c	defined routines
c
	sph(1) = rho
	sph(2) = 90.0 - lat
	sph(3) = lon
	car(1) = rx
	car(2) = ry
	car(3) = rz

	if (iopt.ge.0) then
	  call sphtocar(sph, car)
	  rx  =  car(1)
	  ry  =  car(2)
	  rz  =  car(3)
	end if

	if (iopt.lt.0) then
	  call cartosph(car, sph)
	  rho = sph(1)
	  lat = 90.0 - sph(2)
	  lon = sph(3)
	end if	
	return
	end
