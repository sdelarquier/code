c *************************************************************************
C File: /home/darn/gen/rdkelaz.f
C last Modification: 13-MAR-2002
C Modified by: Dieter Andre
C
	subroutine RDKELAZ(st_id,year,yr_sec,lat,lon,rho,height,elv,azc)
c
C Revision 2.0; 20020313; Dieter Andre
C Time dependence of radar parameters
C Removed duplicate yr parameter
C
c Solves for all radar field points the pointing elevation (elv) and azimuth
c (azc) angles of the radar wave k vector at the field point measured with
c respect to the local horizon.
c
c Last written - Aug 8/95
c *************************************************************************
	implicit    none
	integer*2   st_id
	integer*2   year
	integer*4   yr_sec
	real*4      lat(0:15,75),lon(0:15,75),rho(0:15,75)
	real*4      height
	real*4      elv(0:15,75),azc(0:15,75)
	real*4	    pos(5),tm
	integer*2   ja,ia,ifrst
	real*4      gdlat,gdlon,grho,glat,glon,dum
	real*4      flat,flon,frho,fx,fy,fz
	real*4      gbx,gby,gbz,gx,gy,gz,ghx,ghy,ghz
	real*4      bx,by,bz,b
	
	call radar_pos(st_id, year, yr_sec,pos)
	gdlat  =  pos(1)
	gdlon  =  pos(2)
	tm     =  year
	ifrst  =  1

	do ja = 0,15
	  do ia = 1,75
	    flat = lat(ja,ia)
	    flon = lon(ja,ia)
	    frho = rho(ja,ia)
c solve for the pointing direction of the radar wave k vector (assuming 
c assuming line propagation)
	    call gsptcar(+1,frho,flat,flon,fx,fy,fz)
	    call geodtgc(+1,gdlat,gdlon,grho,glat,glon,dum)
	    call gsptcar(+1,grho,glat,glon,gbx,gby,gbz)
	    gx = fx - gbx
	    gy = fy - gby
	    gz = fz - gbz
	    call norm_vec(gx,gy,gz)
c compute the look direction of the k vector in horizon coordinates
	    call glbthor(+1,flat,flon,gx,gy,gz,ghx,ghy,ghz)
	    call norm_vec(ghx,ghy,ghz)
c determine the pointing direction of the radar k vector after refraction:
c - obtain the magnetic field components in the local horizon coordinate system
c - rotate the k vector in the vertical plane to orthogonality to B
	    call magcmp(tm,frho,flat,flon,bx,by,bz,b,ifrst)
	    call norm_vec(bx,by,bz)
	    ghz = -(bx*ghx+by*ghy)/bz
	    call norm_vec(ghx,ghy,ghz)
c calculate the pointing elevation and azimuth angles of the radar k vector
	    elv(ja,ia) = atan2(ghz,sqrt(ghx**2+ghy**2))* 57.29577951
	    azc(ja,ia) = atan2(ghy,-ghx)* 57.29577951
	  end do
	end do
	return
	end


