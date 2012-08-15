c****************************************************************************
c	This subroutine converts a vector specified in spherical coordinates
c	to a vector specified in cartesian coordinates.
c
c	The arguments are all REAL*8
c
c	The calling sequence is:
c	  call sphtocarvect(sph, sphvect, carvect)
c	where sph is the origin of the vector in spherical coordinates
c	sphvect is the r, theta, phi components of the vector
c	carvect are the returned values of the cartesian vector
c
c	NOTE:  the anglular values are given in degrees and the angle
c	theta is measured with respect to the north pole (i.e. colatitude)
c
c
c       $Log:	sphtocarvect.f,v $
c Revision 1.1  94/08/03  17:02:59  17:02:59  miker (Michael Ruohoniemi S1G)
c Initial revision
c 
c
	subroutine sphtocarvect(sph,sphvect,carvect)
	implicit none
	real*8 sph(3),sphvect(3),carvect(3)
	real*8 sinteta,costeta,sinalph,cosalph,el(3,3)
	integer*2 i,j

	do i = 1,3
	   carvect(i) = 0.0
	end do
	sinteta = sin(sph(2)* 0.017453292)
	costeta = cos(sph(2)* 0.017453292)
	sinalph = sin(sph(3)* 0.017453292)
	cosalph = cos(sph(3)* 0.017453292)
	el(1,1) = sinteta*cosalph
	el(2,1) = sinteta*sinalph
	el(3,1) = costeta
	el(1,2) = costeta*cosalph
	el(2,2) = costeta*sinalph
	el(3,2) =  -sinteta
	el(1,3) = -sinalph
	el(2,3) = cosalph
	el(3,3) = 0.
	do i = 1,3
	   do j = 1,3
	      carvect(i) = carvect(i)+ el(i,j)*sphvect(j)
	   end do
        end do	  

	return
	end

