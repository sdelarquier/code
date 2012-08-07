c **************************************************************************c
c
c	This routine converts cartesian coordinates to spherical coordinates
c
c	The spherical coordinates are specified in the form  r, theta, phi
c	with theta being the angle from the north pole (i.e. colatitude) and
c	the angles are specified in degrees
c
c	Note that the arguments are REAL*8
c
c       $Log:	cartosph.f,v $
c Revision 1.1  94/08/03  15:42:00  15:42:00  baker (Kile Baker S1G)
c Initial revision
c 
c

	subroutine cartosph(car,sph)
	implicit none
	real*8 car(3),sph(3)
	
	sph(1) = sqrt(car(1)*car(1)+car(2)*car(2)+car(3)*car(3))
	sph(2) = acos( car(3)/ sph(1))* 57.29577951
	if((car(2).eq.0.).and.(car(1).eq.0.)) then
	   sph(3) = 0.
	else
	   sph(3) = atan2( car(2), car(1))* 57.29577951
	end if
	return
	end
