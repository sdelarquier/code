c****************************************************************************
	subroutine GLBTHOR(iopt,lat,lon,rx,ry,rz,tx,ty,tz)

c Converts a vector from the global cartesian components (rx,ry,rz) defined
c in s/r GSPTCAR to cartesian components (tx,ty,tz) defined with respect to
c the local horizon (x:southwards  y:eastwards  z:vertical).  (iopt >= 0)
c Also performs the inverse transformation (iopt < 0).

c
c       $Log:	glbthor.f,v $
c Revision 1.1  94/08/03  16:17:49  16:17:49  miker (Michael Ruohoniemi S1G)
c Initial revision
c 
c
c****************************************************************************
	implicit none
	integer*4 iopt
	real*4 lat,lon,rx, ry, rz, tx, ty, tz, lax, lon_rad
	real*4 sx, sy, sz

        lon_rad= lon* 0.017453292
	if (iopt.ge.0) then
	  sx  =  cos(lon_rad)*rx + sin(lon_rad)*ry
	  sy  = -sin(lon_rad)*rx + cos(lon_rad)*ry
	  sz  =  rz
	  lax =  (90 - lat)* 0.017453292
	  tx  =  cos(lax)*sx - sin(lax)*sz
	  ty  =  sy
	  tz  =  sin(lax)*sx + cos(lax)*sz
	end if
	if (iopt.lt.0) then
	  lax =  (90 - lat)* 0.017453292
	  sx  =  cos(lax)*tx + sin(lax)*tz
	  sy  =  ty
	  sz  = -sin(lax)*tx + cos(lax)*tz
	  rx  =  cos(lon_rad)*sx - sin(lon_rad)*sy
	  ry  =  sin(lon_rad)*sx + cos(lon_rad)*sy
	  rz  =  sz
	end if
	return
	end
