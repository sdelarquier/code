	subroutine CORR_AZIM(ind,irad,ibeam,rho,beta,azb,heit)
c	******************************************************
c
c*******************************************************************
c This routine corrects the azimuth for the cone angle effect. This
c correction is small enough to be calculated in the spherical
c Earth approximation. The Earth radius is then taken at the radar
c site.
c 
c Inputs:
c	ind: 	>0 if correction is for the beam center
c	     	<0 if correction is for the beam edge
c	irad: 	radar index (1 or 2)
c	ibeam: 	beam center number if ind>0
c		beam_edge   number if ind<0
c	rho:	Earth radius at radar site
c	beta:	Ground distance (in degrees) from the radar site
c	azb:	Non-corrected azimuth
c
c Output:
c	azb: 	Corrected azimuth
c	heit:	The virtual height
c
c The elevation angle is calculated in an iterative way, with a
c tolerance of 5 km on the virtual height.
c
c Last written: Oct. 93 (CS)
c
c	$Log:	corr_azim.f,v $
c Revision 1.1  94/09/01  16:13:40  16:13:40  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c********************************************************************


	include   'info.inc'
	parameter (ng = 75)
	integer*2 mhz(2),khz(2),lagfr(2),smsep(2),intt(2),nrang(2)
	integer*2 ir, RANGE_GATE
	real*4    pos(2,5)
	real*4    v_height(2,0:15,ng),vv_height(0:16,ng)
	common/radars/pos
	common/hght/v_height
	common/param/mhz,khz,lagfr,smsep,intt,nrang
c__________________________

	k = irad
	boraz = pos(irad,3)
	azstp = pos(irad,4)
	rxris = pos(irad,5)

	do i=0,16
	  do j=1,ng
	    vv_height(i,j) = 0.
	  enddo
	enddo

	do i=0,15
	  do j=1,ng
	    vv_height(i,j) = v_height(irad,i,j)
	  enddo
	enddo

	heit = height
   1	h = heit
	rh = rho*sind(beta)
	sh = rh*tand(beta/2.)
	ph = sh+h
	range = sqrt(rh*rh+ph*ph)
	ir = RANGE_GATE(lagfr(irad),smsep(irad),rxris,0.,range)
	if((ir.gt.nrang(k)).or.(ir.lt.1)) return
	if(ind.gt.0) then
	  if((ibeam.ge.0).and.(ibeam.le.15)) then
	    heit = vv_height(ibeam,ir)
	  else
	    heit = height
	  endif
	else
	  if((ibeam.ne.0).and.(ibeam.ne.16)) then
	    heit = (vv_height(ibeam-1,ir)+vv_height(ibeam,ir))/2.
	  else if(ibeam.eq.0) then
	    heit = vv_height(ibeam,ir)
	  else if(ibeam.eq.16) then
	    heit = vv_height(ibeam-1,ir)
	  endif
	endif
	if(abs(heit-h).gt.0.5) go to 1

c... Computes the elevation angle ...

	elev = atan2d(ph,rh)-beta
	if(elev.le.0.) elev = 0.
	cos_el = cosd(elev)

c... Corrects the azimuth angle ...

	if(ind.gt.0) then
	  psi0 = (float(ibeam)-7.5)*azstp
	else
	  psi0 = (float(ibeam)-8.0)*azstp
	endif
	sin_psi0 = sind(psi0)
	sin_psi = sin_psi0 / cos_el
	psi = asind(sin_psi)
	corr = psi - psi0
	azb = xmod(azb + corr)
	return
	end
