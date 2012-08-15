	subroutine VIRTUAL
c	******************

c******************************************************************
c Makes the virtual height array
c Inputs:
c	- ialt: index of the method for virtual height setting
c	- height: virtual height choice (from MERGE_INFO)
c Output(in the common hght):
c	- v_height: array of virtual height, kept in common/hght/
c
c Last written: Oct. 93 (CS)
c
c	$Log:	virtual.f,v $
c Revision 1.1  94/09/01  16:14:05  16:14:05  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c******************************************************************

	include   'info.inc'
	parameter (ng = 75)
	integer*2 gsc(2,0:15,ng),n_avg(2,0:15,ng)
	real*4    v_height(2,0:15,ng),virt_hght(ng)
	real*4    range(2,ng), pos(2,5)
	real*4    p(2,0:15,ng),v(2,0:15,ng)
	real*4	  verr_v(2,0:15,ng),verr_w(2,0:15,ng)
	real*4    width(2,0:15,ng),werr(2,0:15,ng)
	real*4    el(2,0:15,ng),el_low(2,0:15,ng),el_high(2,0:15,ng)
	logical*1 bad(2,0:15,ng)
	common/hght/v_height
	common/gate/range
	common/radars/pos
	common/data/p,v,verr_v,verr_w,width,werr,el,el_low,el_high,
     &			bad,gsc,n_avg
	data re/6356.779/
c___________________________


c... Initialisations ...

	do k=1,2
	  do ib=0,15
	    do ir=1,ng
	      v_height(k,ib,ir) = height
	    enddo
	  enddo
	enddo

c... Virtual height ...

	if(ialt.eq.1) return				!ialt = 1

	if(ialt.eq.2) then				!ialt = 2
	  call MODEL(virt_hght)
	  do k=1,2
	    do ib=0,15
	      do ir=1,ng
		v_height(k,ib,ir) = virt_hght(ir)
	      enddo
	    enddo
	  enddo
	endif

	if(ialt.eq.3) then
	  do k=1,2					!ialt = 3
	    gdlat = pos(k,1)
	    gdlon = pos(k,2)
	    boraz = pos(k,3)
	    azstp = pos(k,4)
	    do ib=0,15
	      psi = (float(ib)-7.5)*azstp
	      do ir=1,ng
c**** ATTENTION: Needs tests on elevation: if the elevation
c****            has an unconsistent value, v_height should be
c****		 set to height.
	        d = range(k,ir)
	        reh2 = re*re + d*d + 2.*d*re*sind(el(k,ib,ir))
	        temp_height = sqrt(reh2) - re
	        call FLDPNTH(gdlat,gdlon,psi,boraz,temp_height,d,
     &				brho,blat,blon)
	        v_height(k,ib,ir) = temp_height
	      enddo
	    enddo
	  enddo
	endif

	return
	end
