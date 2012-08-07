c*****************************************************************************
c	This subroutine converts spherical coordinates (r,theta,phi)  to
c       cartesian coordinates.  Note that theta is the angle from the north
c       pole (i.e. co-latitude).  The angle arguments are given in DEGREES.
c       Note also that the arguments are real*8.
c
c	Calling sequence:
c	   real *8 sph(3), car(3)
c	   call sphtocar(sph, car)
c
c
c       $Log:	sphtocar.f,v $
c Revision 1.1  94/08/03  17:01:14  17:01:14  miker (Michael Ruohoniemi S1G)
c Initial revision
c 
c
	subroutine sphtocar(sph,car)
	implicit none
	real*8 sph(3),car(3),sinteta

	sinteta = sin(sph(2)* 0.017453292)
	car(1)  = sph(1)*sinteta*cos(sph(3)* 0.017453292)
	car(2)  = sph(1)*sinteta*sin(sph(3)* 0.017453292)
	car(3)  = sph(1)*cos(sph(2)* 0.017453292)
	return
	end
