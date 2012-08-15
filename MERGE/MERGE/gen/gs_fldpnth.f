c****************************************************************************
      subroutine GS_FLDPNTH(gdlat,gdlon,psi,bore,fh,r,frho,flat,flon)

c This subroutine calculates the geocentric coordinates (frho,flat,flon)
c of a radar field point given the angular geodetic coordinates (gdlat,gdlon)
c of the radar site, the pointing azimuth (determined by psi and bore), the
c field point height (fh), and the slant range (r).
c The solution is an iterative one, with a tolerance of .5 km on the height.

c The height is automatically adjusted (lowered) if the range is deemed
c insufficient to support scatter at the stated altitude and the value of
c height is reassigned to the adjusted value.

c Last written: March 23/92   - changes to height algorithm
c
c     $Log:	fldpnth.f,v $
c Revision 1.1  94/08/03  16:10:37  16:10:37  miker (Michael Ruohoniemi S1G)
c Initial revision
c 
c
c****************************************************************************
        real*4  psi,bore,kxg,kyg,kzg,kxr,kyr,kzr
        fh = 250.
	if (fh.le.150) then
	  xh = fh
	else
	  if (r.le.300) then
	    xh = 115
          else if (r.gt.300.and.r.lt.500) then
	    xh = (r-300)/200 * (fh-115) + 115
	  else
	    xh = fh
	  end if
	end if
	if (r.lt.150) then
	  xh = (r/150) * 115
	end if
        call geodtgc(+1,gdlat,gdlon,rrad,rlat,rlon,del)
	rrho =  rrad
c Approximate the radius of the earth beneath the field point.
	frad =  rrad
1       frho =  frad + xh
c Estimate the off-array-normal azimuth (azi) from the cone angle (psi) and the
c pointing elevation (xel) approximated by its spherical earth value (rel).
	rel      =  asin( ((frho)**2-rrad**2-r**2) / (2*rrad*r) )* 57.29577951
        xel      =  rel
	tan_azi  =  sqrt(sin(psi* 0.017453292)**2/(cos(psi* 0.017453292)**2 - sin(xel* 0.017453292)**2))
	azi      =  57.29577951* atan(tan_azi)*sign(1.0,psi)
c Obtain the corresponding value of pointing azimuth (xal).
	xal      =  azi + bore
c Adjust the azimuth angle for the oblateness of the earth.
	call geocnvrt(gdlat,gdlon,xal,xel,ral,dum)
c Obtain the global spherical coordinates of the field point.
	call fldpnt(rrho,rlat,rlon,ral,rel,r,frho,flat,flon)
c Recompute the radius of the earth beneath the field point and check altitude.
	call geodtgc(-1,dum1,dum2,frad,flat,flon,dum3)
        fhx  = frho - frad
        if (abs(fhx-xh).gt.0.5) goto 1
        return
        end




















