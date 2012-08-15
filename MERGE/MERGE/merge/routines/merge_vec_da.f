	subroutine MERGE_VEC_DA(ngood,fgood,vx,vy,vz,
     &			     vx_erv,vy_erv,vz_erv,
     &			     vx_erw,vy_erw,vz_erw,pnt_qlty)
c	***************************************************

c*********************************************************************
c This routine merges the radial velocities from the two radars.
c Inputs are found in the commons 'data', and 'table'
c Outputs are:
c	 ngood:			number of valid vectors in the map
c	 fgood(0:15,0:15):	true if valid vector
c	 vx,vy,vz(0:15,0:15):	velocity components
c	 vx,vy,vz_erv(0:15,0:15): error on velocity components
c					(from vel_err)
c	 vx,vy,vz_erw(0:15,0:15): error on velocity components
c					(from width)
c	 pnt_qlty(0:15,0:15):	quality index for each data point
c
c Last written: Oct. 93 (CS)
c
c	$Log:	merge_vec.f,v $
c Revision 1.1  94/09/01  16:13:52  16:13:52  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c	Revision: 2.0; 16-JUN-1998 Dieter Andre'
c	Calculate errors for vlos and put into common vradial
c*********************************************************************

	include   'info.inc'
	parameter (ng = 75, npts = 25)
	integer*2 mhz(2),khz(2),lagfr(2),smsep(2),intt(2),nrang(2)
	integer*2 n(2,0:15,0:15),rng(2,npts,0:15,0:15)
	integer*2 gsc(2,0:15,ng),n_avg(2,0:15,ng)
	integer*2 nb(2),ngood,num(2)
	integer*2 pnt_qlty(0:15,0:15),quality_pnt
	real*4	  az(2,-1:16,-1:16),alt(-1:16,-1:16),abb(0:15,0:15)
	real*4    latg(-1:16,-1:16),long(-1:16,-1:16)
	real*4    latm(0:15,0:15),lonm(0:15,0:15)
	real*4    bx(0:15,0:15),by(0:15,0:15),bz(0:15,0:15)
	real*4    kx(2,0:15,0:15),ky(2,0:15,0:15),kz(2,0:15,0:15)
	real*4	  vlos(2,0:15,0:15)
	real*4	  vlos_erv(2,0:15,0:15)		! DA
	real*4	  vlos_erw(2,0:15,0:15)		! DA
	real*4    p(2,0:15,ng),v(2,0:15,ng)
	real*4	  verr_v(2,0:15,ng),verr_w(2,0:15,ng)
	real*4    width(2,0:15,ng),werr(2,0:15,ng)
	real*4    el(2,0:15,ng),el_low(2,0:15,ng),el_high(2,0:15,ng)
	real*4    vrad(2),vrad_erv(2),vrad_erw(2)
	real*4	  number,nbscans(2)
	real*4    vx(0:15,0:15),vy(0:15,0:15),vz(0:15,0:15)
	real*4    vx_erv(0:15,0:15),vy_erv(0:15,0:15),vz_erv(0:15,0:15)
	real*4    vx_erw(0:15,0:15),vy_erw(0:15,0:15),vz_erw(0:15,0:15)
	dimension aa(3,3),bb(3,1)
	logical*1 bad(2,0:15,ng),fgood(0:15,0:15),fvlos(2,0:15,0:15)
	common/param/mhz,khz,lagfr,smsep,intt,nrang
	common/vradial/vlos,vlos_erv,vlos_erw,fvlos
	common/table/latg,long,alt,latm,lonm,az,abb,n,rng,
     &			bx,by,bz,kx,ky,kz
	common/data/p,v,verr_v,verr_w,width,werr,el,el_low,el_high,
     &			bad,gsc,n_avg
c_____________________________


c... Initializations ...

	ngood = 0
	do ib=0,15
	  do jb=0,15
	    vx(ib,jb) = 0.
	    vy(ib,jb) = 0.
	    vz(ib,jb) = 0.
	    vx_erv(ib,jb) = 0.
	    vy_erv(ib,jb) = 0.
	    vz_erv(ib,jb) = 0.
	    vx_erw(ib,jb) = 0.
	    vy_erw(ib,jb) = 0.
	    vz_erw(ib,jb) = 0.
	    fgood(ib,jb) = .false.
	    pnt_qlty(ib,jb) = 0
	    do ir=1,2
	      vlos(ir,ib,jb) = 0.
	      vlos_erv(ir,ib,jb) = 0.		! DA
	      vlos_erw(ir,ib,jb) = 0.		! DA
	      fvlos(ir,ib,jb) = .false.
	    enddo
	  enddo
	enddo

c... Computes the number of scans averaged during time 'tperiod'

c ATTENTION: This is valid only for 16-beam scans, and if intt does
c *********  not change during time 'tperiod'. This has no effect
c *********  if QUALITY_PNT is called with 'nb' rather than with 'num'

	do k=1,2
	  if(tperiod.eq.0.) then
	    nbscans(k) = 1.
	  else
	    nbscans(k) = tperiod/(intt(k)*16)
	  endif
	enddo

	do ib=0,15
	  do jb=0,15

c... Computes the mean radial velocities at the center of the diamonds

	    angle = abb(ib,jb)
	    wid = 0.
	    do k=1,2
	      vrad(k) = 0.
	      vrad_erv(k) = 0.
	      vrad_erw(k) = 0.
	      nb(k) = 0
	      number = 0.
	      nmax = n(k,ib,jb)
	      if(nmax.ne.0) then
	        irmin = rng(k,1,ib,jb)
		if(irmin.gt.nrang(k)) irmin = nrang(k)
	        irmax = rng(k,nmax,ib,jb)
		if(irmax.gt.nrang(k)) irmax = nrang(k)
	        if(k.eq.1) ibeam = ib
	        if(k.eq.2) ibeam = jb
	        do ir=irmin,irmax
	          if(.not.bad(k,ibeam,ir)) then
	            vrad(k) = vrad(k) + v(k,ibeam,ir)
	            vrad_erv(k) = vrad_erv(k) + verr_v(k,ibeam,ir)**2
	            vrad_erw(k) = vrad_erw(k) + verr_w(k,ibeam,ir)**2
		    wid = wid + width(k,ibeam,ir)
		    nb(k) = nb(k) + 1
		    number = number + n_avg(k,ibeam,ir)
	          endif
	        enddo
	      endif
	      if(nb(k).ne.0) then
		vrad(k) = vrad(k)/nb(k)
		vrad_erv(k) = sqrt(vrad_erv(k))/nb(k)
		vrad_erw(k) = sqrt(vrad_erw(k))/nb(k)
	      endif
	      number = number/nbscans(k)	!normalizes to its
	      num(k) = number + 0.5		!value for one scan
	    enddo
	    if((nb(1)+nb(2)).ne.0) wid = wid/(nb(1)+nb(2))

c... Computes the vector velocity (vx,vy,vz) ...

	    if((nb(1).ne.0).and.(nb(2).ne.0)) then

	      do k=1,2
	        aa(k,1) = kx(k,ib,jb)
	        aa(k,2) = ky(k,ib,jb)
	        aa(k,3) = kz(k,ib,jb)
	      enddo
	      aa(3,1) = bx(ib,jb)
	      aa(3,2) = by(ib,jb)
	      aa(3,3) = bz(ib,jb)
	      bb(1,1) = vrad(1)
	      bb(2,1) = vrad(2)
	      bb(3,1) = 0.
	      call GAUSSJ(aa,3,3,bb,1,1)

	      vx(ib,jb) = bb(1,1)		!Velocity
	      vy(ib,jb) = bb(2,1)
	      vz(ib,jb) = bb(3,1)
c						!Error
	      vx_erv(ib,jb) = sqrt( (aa(1,1)**2)*(vrad_erv(1)**2) +
     &				    (aa(1,2)**2)*(vrad_erv(2)**2) )
	      vy_erv(ib,jb) = sqrt( (aa(2,1)**2)*(vrad_erv(1)**2) +
     &				    (aa(2,2)**2)*(vrad_erv(2)**2) )
	      vz_erv(ib,jb) = sqrt( (aa(3,1)**2)*(vrad_erv(1)**2) +
     &				    (aa(3,2)**2)*(vrad_erv(2)**2) )
	      vx_erw(ib,jb) = sqrt( (aa(1,1)**2)*(vrad_erw(1)**2) +
     &				    (aa(1,2)**2)*(vrad_erw(2)**2) )
	      vy_erw(ib,jb) = sqrt( (aa(2,1)**2)*(vrad_erw(1)**2) +
     &				    (aa(2,2)**2)*(vrad_erw(2)**2) )
	      vz_erw(ib,jb) = sqrt( (aa(3,1)**2)*(vrad_erw(1)**2) +
     &				    (aa(3,2)**2)*(vrad_erw(2)**2) )
	      fgood(ib,jb) = .true.
	      ngood = ngood + 1

c... Projects the velocity to the standard altitude height_r along
c... the magnetic field (the divergence of the field lines is taken
c... into account ...

	      alpha = atan2d(bx(ib,jb),by(ib,jb))
	      v_merid = vx(ib,jb)*cosd(alpha) + vy(ib,jb)*sind(alpha)
	      v_lonm = -vx(ib,jb)*sind(alpha) + vy(ib,jb)*cosd(alpha)
	      rz_r = 1.+ (height_r-alt(ib,jb)) / (alt(ib,jb)+6370.)
	      v_merid = v_merid * rz_r * rz_r
	      v_lonm = v_lonm*  rz_r
	      vx(ib,jb) = v_merid*cosd(alpha) - v_lonm*sind(alpha)
	      vy(ib,jb) = v_merid*sind(alpha) + v_lonm*cosd(alpha)
	      vz(ib,jb) = -(vx(ib,jb)*bx(ib,jb) + vy(ib,jb)*by(ib,jb))
     &				/ bz(ib,jb)
	    endif

	    do k=1,2
	      if(nb(k).ne.0) then
		rz_r = 1.+ (height_r-alt(ib,jb))/(alt(ib,jb)+6370.)
		vlos(k,ib,jb) = vrad(k) * rz_r *sqrt(rz_r)	! DA
		vlos_erv(k,ib,jb) = vrad_erv(k) * rz_r *sqrt(rz_r)	! DA
		vlos_erw(k,ib,jb) = vrad_erw(k) * rz_r *sqrt(rz_r)	! DA
		fvlos(k,ib,jb) = .true.
	      endif
	    enddo

c... Computes the quality index for the vector

c	    pnt_qlty(ib,jb) = QUALITY_PNT(num,wid,angle)
	    pnt_qlty(ib,jb) = QUALITY_PNT(nb,wid,angle)

	  end do
	end do

	return
	end
