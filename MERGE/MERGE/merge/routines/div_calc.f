	subroutine DIV_CALC(vx,vy,glat,glon,div)
c	****************************************

c**********************************************************************
c Computes the divergence (div) of the velocity 
c field in curvilinear (radar) coordinates at the center (i=2, j=2) of
c the 3x3 and 5x5 arrays. Positions (glat, glon) and velocities
c (x, southward; y, eastward) are the inputs, and div is the output.
c
c div is computed in units of [Re*pi/180]**2,
c	where Re is the earth radius
c
c Last written: Oct. 93 (JCC)
c
c	$Log:	div_calc.f,v $
c Revision 1.1  94/09/01  16:13:41  16:13:41  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c**********************************************************************

	real*4  glat(0:4,0:4),glon(0:4,0:4)        
	real*4  vx(3,3),vy(3,3)
c________________
      
c... Computes the volume sqg of the unitary volume ...
        
	i = 2
	j = 2
	dlat1 = ( glat(i+1,j) - glat(i-1,j) )/2.        
	dlat2 = ( glat(i,j+1) - glat(i,j-1) )/2.
	dlon1 = ( glon(i+1,j) - glon(i-1,j) )/2.        
	dlon2 = ( glon(i,j+1) - glon(i,j-1) )/2.
	c2_lat = cosd(glat(2,2))*cosd(glat(2,2))
	g11 = dlat1*dlat1 + dlon1*dlon1*c2_lat        
	g22 = dlat2*dlat2 + dlon2*dlon2*c2_lat
	g12 = dlat1*dlat2 + dlon1*dlon2*c2_lat
	sqg = sqrt(g11*g22-g12*g12)

c... Now calculates the divergence div_v ...
        
	i = 3
	j = 2
	dlat1 = ( glat(i+1,j) - glat(i-1,j) )/2.        
	dlat2 = ( glat(i,j+1) - glat(i,j-1) )/2.
	dlon1 = ( glon(i+1,j) - glon(i-1,j) )/2.        
	dlon2 = ( glon(i,j+1) - glon(i,j-1) )/2.
	call C_CONTRA(vx(i,j),vy(i,j),glat(i,j),comp1p,dum,
     &        	  	dlat1,dlat2,dlon1,dlon2)

	i = 1
	j = 2
	dlat1 = ( glat(i+1,j) - glat(i-1,j) )/2.        
	dlat2 = ( glat(i,j+1) - glat(i,j-1) )/2.
	dlon1 = ( glon(i+1,j) - glon(i-1,j) )/2.        
	dlon2 = ( glon(i,j+1) - glon(i,j-1) )/2.
	call C_CONTRA(vx(i,j),vy(i,j),glat(i,j),comp1m,dum,
     &        	  	dlat1,dlat2,dlon1,dlon2)
	df1du1  = (comp1p-comp1m)/2.

	i = 2
	j = 3
	dlat1 = ( glat(i+1,j) - glat(i-1,j) )/2.        
	dlat2 = ( glat(i,j+1) - glat(i,j-1) )/2.
	dlon1 = ( glon(i+1,j) - glon(i-1,j) )/2.        
	dlon2 = ( glon(i,j+1) - glon(i,j-1) )/2.
	call C_CONTRA(vx(i,j),vy(i,j),glat(i,j),dum,comp2p,
     &        	  	dlat1,dlat2,dlon1,dlon2)

	i = 2
	j = 1
	dlat1 = ( glat(i+1,j) - glat(i-1,j) )/2.        
	dlat2 = ( glat(i,j+1) - glat(i,j-1) )/2.
	dlon1 = ( glon(i+1,j) - glon(i-1,j) )/2.        
	dlon2 = ( glon(i,j+1) - glon(i,j-1) )/2.
	call C_CONTRA(vx(i,j),vy(i,j),glat(i,j),dum,comp2m,
     &        	  	dlat1,dlat2,dlon1,dlon2)
	df2du2  = (comp2p-comp2m)/2.

	div = (df1du1+df2du2)/sqg

	return
	end
