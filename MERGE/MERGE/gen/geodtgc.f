c*************************************************************************
	subroutine GEODTGC(iopt,gdlat,gdlon,grho,glat,glon,del)

c Converts from geodetic coordinates (gdlat,gdlon) to geocentric spherical
c coordinates (glat,glon) for iopt >= 0.
c The reverse transformation is effected for iopt < 0.
c In either case, the radius of the earth (grho) and the deviation of the
c vertical (del) are calculated.

c The IAU 1964 oblate spheriod model of the earth is adopted.

c       $Log:	geodtgc.f,v $
c Revision 1.1  94/08/03  16:15:01  16:15:01  miker (Michael Ruohoniemi S1G)
c Initial revision
c 
c
c********************************************
	implicit none
	integer*4 iopt
        real*4 gdlat,gdlon,grho,glat,glon,del
        real*4 a,f,b,e2
        a    =  6378.16
        f    =  1./298.25
        b    =  a*(1.-f)
        e2   =  a**2/b**2-1
	if (iopt.ge.0) then
          glat =  atan( b**2/a**2*tan( 0.017453292* gdlat))* 57.29577951
          glon =  gdlon
	  if (glon.gt.180) glon = glon - 360
	end if
	if (iopt.lt.0) then
	  gdlat =  atan(a**2/b**2*tan( 0.017453292* glat))* 57.29577951
	  gdlon =  glon
	end if  
        grho =  a/sqrt(1.+e2*sin( 0.017453292* glat)**2)
        del  =  gdlat - glat
	return
        end
