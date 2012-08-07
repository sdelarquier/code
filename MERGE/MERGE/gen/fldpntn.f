c SD_BEGIN>
c****************************************************************************
      SUBROUTINE FLDPNTn(rrho,rlat,rlon,ral,rel,r,frho,flat,flon)
c INVERSE OPERATION.
c This subroutine calculates the geocentric coordinates (frho,flat,flon)
c of a field point given the angular geocentric coordinates (rrho,rlat,rlon)
c of the point of origin, the azimuth (ral), the elevation (rel), and
c the slant range (r).
c Note that the azimuth and elevation angles are reckoned from the horizon
c that is defined by the plane perpendicular to the radial vector from the
c centre of the earth through the point of origin. For conversion from radar
c pointing coordinates to these coordinates, call s/r ceocnvrt.
c Last written: Aug 17/89, Aug 7/91
c Obtain the global cartesian coordinates of the field point by vector
c addition.
c <SD_END
        implicit none
        real*4 rrho,rlat,rlon,ral,rel,r,frho,flat,flon
        real*4 tx,ty,tz,rx,ry,rz,sx,sy,sz,hx,hy,hz

        call gsptcar(+1,frho,flat,flon,tx,ty,tz)
        call gsptcar(+1,rrho,rlat,rlon,rx,ry,rz)
        sx  =  tx - rx
        sy  =  ty - ry
        sz  =  tz - rz
        call glbthor(+1,rlat,rlon,sx,sy,sz,hx,hy,hz)
        call hsptcar(-1,r,ral,rel,hx,hy,hz)
        return
        end
