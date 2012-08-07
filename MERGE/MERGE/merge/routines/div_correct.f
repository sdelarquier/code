	subroutine DIV_CORRECT(vx,vy,dvx,dvy,glat,glon,az,div_v,vrad)
c	*************************************************************

c********************************************************************
c This routine reduces the divergence to zero. The velocity
c components are corrected in proportion to their inhomegeneity,
c as computed by the routine DVCAL, called by the subroutine
c DIV_FREE.
c 
c Inputs: - vx,vy,dvx,dvy,glat and glon are the 3x3 and 5x5 matrices 
c	    for velocity components and positions at points surroun-
c	    ding the point where the velocity has to be corrected
c	  - div_v is the velocity divergence at the point where the
c	    velocity has to be corrected
c Outputs:- vx and vy, the corrected values of the velocity components
c	    which replace the input values; vrad, the los velocities
c	    consistent with the new velocity components
c
c Last written: Oct. 93 (JCC)
c
c	$Log:	div_correct.f,v $
c Revision 1.1  94/09/01  16:13:42  16:13:42  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c********************************************************************

	real*4  glat(0:4,0:4),glon(0:4,0:4)        
	real*4  vx(3,3),vy(3,3),dvx(3,3),dvy(3,3)
	real*4  ddiv_dvx(3,3),ddiv_dvy(3,3)
	real*4  az(2,3,3),vrad(2,3,3)
c__________________

c... Initializations

	do i=1,3
	  do j=1,3
	    ddiv_dvx(i,j) = 0.
	    ddiv_dvy(i,j) = 0.
	  enddo
	enddo

c... Calculates the volume sqg of the unitary volume ...

	i = 2
	j = 2
	dlat1 = ( glat(i+1,j) - glat(i-1,j) )/2.        
	dlat2 = ( glat(i,j+1) - glat(i,j-1) )/2.
	dlon1 = ( glon(i+1,j) - glon(i-1,j) )/2.        
	dlon2 = ( glon(i,j+1) - glon(i,j-1) )/2.
	c2_lat = cosd( glat(2,2))*cosd(glat(2,2) )
	g11 = dlat1*dlat1 + dlon1*dlon1*c2_lat        
	g22 = dlat2*dlat2 + dlon2*dlon2*c2_lat
	g12 = dlat1*dlat2 + dlon1*dlon2*c2_lat
	sqg = sqrt( g11*g22 - g12*g12 )        
	coef = 1./(2.*sqg)

c... Now calculates the derivatives of the divergence ...

	i = 3
	j = 2
	dlat2 = ( glat(i,j+1) - glat(i,j-1) )/2.
	dlon2 = ( glon(i,j+1) - glon(i,j-1) )/2.
	ddiv_dvx(i,j) = coef*dlon2*cosd(glat(i,j))
	ddiv_dvy(i,j) = coef*dlat2

	i = 1
	j = 2
	dlat2 = ( glat(i,j+1) - glat(i,j-1) )/2.
	dlon2 = ( glon(i,j+1) - glon(i,j-1) )/2.
	ddiv_dvx(i,j) = -coef*dlon2*cosd(glat(i,j))
	ddiv_dvy(i,j) = -coef*dlat2

	i = 2
	j = 3
	dlat1 = ( glat(i+1,j) - glat(i-1,j) )/2.        
	dlon1 = ( glon(i+1,j) - glon(i-1,j) )/2.        
	ddiv_dvx(i,j) = -coef*dlon1*cosd(glat(i,j))
	ddiv_dvy(i,j) = -coef*dlat1

	i = 2
	j = 1
	dlat1 = ( glat(i+1,j) - glat(i-1,j) )/2.        
	dlon1 = ( glon(i+1,j) - glon(i-1,j) )/2.        
	ddiv_dvx(i,j) = coef*dlon1*cosd(glat(i,j))
	ddiv_dvy(i,j) = coef*dlat1


c... Corrections to the cartesian velocity components ...

	sum = 0.
	  do i=1,3
	    do j=1,3
	      if(((i+j).eq.3).or.((i+j).eq.5)) then
	        sum = sum + abs(ddiv_dvx(i,j)*dvx(i,j)) +
     &			    abs(ddiv_dvy(i,j)*dvy(i,j))
	      endif
	    enddo
	  enddo

	do i=1,3  
	  do j=1,3
	    if(((i+j).eq.3).or.((i+j).eq.5)) then
	        vx(i,j) = vx(i,j) - div_v*sign(1.,ddiv_dvx(i,j)) *
     &			       	    dvx(i,j)/sum
                vy(i,j) = vy(i,j) - div_v*sign(1.,ddiv_dvy(i,j)) *
     &				    dvy(i,j)/sum
	    endif
	  enddo
	enddo

c... Calculates the new radial velocity components

	do i=1,3
	  do j=1,3
	    if(((i+j).eq.3).or.((i+j).eq.5)) then
	      do k=1,2
		vrad(k,i,j) = -cosd(az(k,i,j))*vx(i,j) +
     &	        	       sind(az(k,i,j))*vy(i,j)
              enddo
            endif
          enddo
	enddo

	return
	end
