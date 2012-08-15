c*****************************************************************************
	subroutine NORM_VEC(rx,ry,rz)
c
c This subroutine renormalizes a vector defined by the components
c rx, ry, rz.  The values are divided by the magnitude of the vector, to
c return a vector of magnitude 1.
c
c       $Log:	norm_vec.f,v $
c Revision 1.1  94/08/03  16:44:12  16:44:12  miker (Michael Ruohoniemi S1G)
c Initial revision
c 
c
c*****************************************************************************
	real*4 rx,ry,rz,rnorm
	rnorm =  sqrt(rx**2+ry**2+rz**2)
	rx    =  rx/rnorm
	ry    =  ry/rnorm
	rz    =  rz/rnorm
	return
	end
