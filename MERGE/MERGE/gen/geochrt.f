c **************************************************************************
C File: /home/darn/gen/geochrt.f
C Last Modification: 13-MAR-2002
C Modified by: Dieter Andre
C Time dependence of radar hardware parameters adapted from Sessai Yukimato
C
	subroutine GEOCHRT(st_id,year,yr_sec, frang, rsep,height,lat,lon,rho,elv,azc)
c
C Revision 2.0; 20020313; Dieter Andre
C Time dependence of radar hardware parameters adapted from Sessai Yukimato
C Use of frang and rsep
C Remove storage of data
C
c Calculates for all radar field points:
c	      i) the geographic position coordinates (lon,lat,rho)
c	     ii) the pointing elevation and azimuth angles (elv,azc) of the
c		 radar k vector
c
c Note that a k vector pointing direction does not, in general, line up
c exactly with the distribution of field points along the corresponding
c beam.  (One result, discussed in s/r RADPLT, is that line-of-sight 
c vectors do not line up exactly stem to stern.)  This is a consequence
c of the varying azimuth of the field points on a beam, which in turn is a
c consequence of cone-angle factors.
cc
c **************************************************************************
	integer*2    st_id, year
	integer*4    yr_sec
	real*4       frang, rsep
	real*4	     height
	real*4	     lat(0:15,75),lon(0:15,75),rho(0:15,75)
	real*4	     elv(0:15,75),azc(0:15,75)


	call radcoor(st_id,year,yr_sec, frang, rsep,height,lat,lon,rho)
	call rdkelaz(st_id,year,yr_sec,lat,lon,rho,height,elv,azc)
	return
	end


