c****************************************************************************
      SUBROUTINE FLDPNT(rrho,rlat,rlon,ral,rel,r,frho,flat,flon)

c This subroutine calculates the geocentric coordinates (frho,flat,flon)
c of a field point given the angular geocentric coordinates (rrho,rlat,rlon)
c of the point of origin, the azimuth (ral), the elevation (rel), and 
c the slant range (r).
c Note that the azimuth and elevation angles are reckoned from the horizon
c that is defined by the plane perpendicular to the radial vector from the
c centre of the earth through the point of origin. For conversion from radar
c pointing coordinates to these coordinates, call s/r ceocnvrt.
c Last written: Aug 17/89
c Obtain the global cartesian coordinates of the field point by vector addition
c
c     $Log:	fldpnt.f,v $
c Revision 1.1  94/08/03  16:04:28  16:04:28  miker (Michael Ruohoniemi S1G)
c Initial revision
c 
c
	implicit none
	real*4 rrho, rlat, rlon, ral, rel, r, frho, flat, flon
	real*4 rx, ry, rz, hx, hy, hz, sx, sy, sz, tx, ty,tz
	call gsptcar(+1,rrho,rlat,rlon,rx,ry,rz)
	call hsptcar(+1,r,ral,rel,hx,hy,hz)
	call glbthor(-1,rlat,rlon,sx,sy,sz,hx,hy,hz)
	tx  =  rx + sx
	ty  =  ry + sy
	tz  =  rz + sz
	call gsptcar(-1,frho,flat,flon,tx,ty,tz)
	return
	end
