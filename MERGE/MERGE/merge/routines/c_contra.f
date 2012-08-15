	subroutine C_CONTRA(vx,vy,glat,comp1,comp2,
     &          		dlat1,dlat2,dlon1,dlon2)
c	************************************************

c**********************************************************************
c Calculates the product fi*sqrt(g) of the contravariant components 
c of the velocity field by the unitary volume (comp1,comp2)        
c at the point of latitude glat, where the velocity is (vx,vy).
c vx (southward) and vy (eastward) are the horizontal components
c of the velocity.
c -dlat1/2, dlon1/2 are the horizontal components of the unitary
c vector in curvilinear (radar) coordinates.
c
c Last written: Oct. 93 (JCC)
c
c	$Log:	c_contra.f,v $
c Revision 1.1  94/09/01  16:13:39  16:13:39  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c**********************************************************************

	cos_lat = cosd(glat)
	comp1 =  vy*dlat2 + vx*dlon2*cos_lat
	comp2 = -vy*dlat1 - vx*dlon1*cos_lat
	return
	end
