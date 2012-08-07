c****************************************************************************
	subroutine HSPTCAR(iopt,r,az,el,tx,ty,tz)

c Performs the conversion from local spherical coordinates to local cartesian
c coordinates (iopt >= 0) and the inverse transformation (iopt < 0).

c For the local horizon coordinates, the components are:
c   r  :  distance from origin (km)
c   az :  azimuth              (deg N)
c   el :  elevation            (deg)

c For the local cartesian coordinates, the components are:
c   tx :  southwards	       (km)
c   ty :  eastwards            (km)
c   tz :  vertical	       (km)

c       $Log:	hsptcar.f,v $
c Revision 1.1  94/08/03  16:41:07  16:41:07  miker (Michael Ruohoniemi S1G)
c Initial revision
c 
c
c****************************************************************************
	implicit none
	integer*4 iopt
	real*4 r, az, el, tx, ty, tz
        real*4 az_rad, el_rad

	if (iopt.ge.0) then
          az_rad= az* 0.017453292
          el_rad= el* 0.017453292
	  tx  = -r*cos(el_rad)*cos(az_rad)
	  ty  =  r*cos(el_rad)*sin(az_rad)
	  tz  =  r*sin(el_rad)
	end if
	if (iopt.lt.0) then
	  r   =  sqrt(tx**2+ty**2+tz**2)
	  el  =  asin(tz/r)* 57.29577951
	  az  =  atan2(ty,-tx)* 57.29577951
	end if
	return
	end
