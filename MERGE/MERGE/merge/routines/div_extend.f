	subroutine DIV_EXTEND(vx,vy,latg,long,az,fgood,ngood)
c	*****************************************************

c*********************************************************************
c This routine extends the velocity map to points of the 16x16 grid
c where the velocity is unknown (fgood=.false.). Both cases (1 or
c 2 radial components missing) are considered. This extension is
c done by assuming that the divergence is zero at the four
c neighbouring points whose divergence depends upon the velocity
c at the current point.
c
c ATTENTION: This method does not work for radars whose field of view
c *********  extend over the geographic pole, i.e. for radars of the
c *********  present SuperDARN chain located in the southern
c *********  hemisphere.
c
c Last written: Oct. 93 (JCC)
c
c	$Log:	div_extend.f,v $
c Revision 1.1  94/09/01  16:13:44  16:13:44  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c*********************************************************************

	integer*2  ind(4),jnd(4),iflag(4),ngood
	real*4     vx(0:15,0:15),vy(0:15,0:15)
	real*4     latg(-1:16,-1:16),long(-1:16,-1:16)
	real*4     az(2,-1:16,-1:16)
	real*4     azz(2,3,3)
	real*4     vvx(3,3),vvy(3,3)
	real*4	   gglat(0:4,0:4),gglon(0:4,0:4)
	real*4     t(4,2),v(2),bb(4)
	real*4     vr(2,0:15,0:15)
	logical*1  fgood(0:15,0:15),lvr(2,0:15,0:15)
	common/vradial/vr,lvr
c________________


c... Starts the analysis of the velocity map ...

      do ib = 0,15
	do jb = 0,15

	  ncomp = 0			!Initializations
	  numc = 0
	  npts = 0
	  do n=1,4
	    ind(n) = 0
	    jnd(n) = 0
	    iflag(n) = 0
	  enddo

c... Tests if the velocity is known at point (ib,jb)   
	  if(fgood(ib,jb)) go to 10

c... Tests if the point is at the border of the fov
	  if((ib.eq.0).or.(ib.eq.15)) then
            if((jb.eq.0).or.(jb.eq.15)) goto 10
	  endif

c... determines how many (ncomp) and, and if ncomp=1, which (numc)
c... radial velocity component(s) is (are) missing ...

	  vra=0.
	  ncomp = 0
	  do icomp=1,2
	    if(.not.lvr(icomp,ib,jb)) then
	      ncomp = ncomp+1
	      numc = icomp
	    else
	      vra=vr(icomp,ib,jb)
	    endif
	  enddo

c... selects the neighbouring points where the divergence
c... has to be set to zero ...

	  if((ib.eq.0).and.((jb.gt.0).and.(jb.lt.15)))then
	    npts = 1
	    ind(1) = ib+1
	    jnd(1) = jb
	    iflag(1) = 1
	  endif
	  if((ib.eq.15).and.((jb.gt.0).and.(jb.lt.15)))then
	    npts = 1
	    ind(1) = ib-1
	    jnd(1) = jb
	    iflag(1) = 2
	  endif
	  if((jb.eq.0).and.((ib.gt.0).and.(ib.lt.15)))then
	    npts = 1
	    ind(1) = ib
	    jnd(1) = jb+1
	    iflag(1) = 4
	  endif
	  if((jb.eq.15).and.((ib.gt.0).and.(ib.lt.15))) then
	    npts = 1
	    ind(1) = ib
	    jnd(1) = jb-1
	    iflag(1) = 3
	  endif
	  if((ib.eq.1).and.(jb.eq.1)) then
	    npts = 2
	    ind(1) = ib
	    jnd(1) = jb+1
	    iflag(1) = 4
	    ind(2) = ib+1
	    jnd(2) = jb
	    iflag(2) = 1
	  endif
	  if((ib.eq.14).and.(jb.eq.1)) then
	    npts = 2
	    ind(1) = ib
	    jnd(1) = jb+1
	    iflag(1) = 4
	    ind(2) = ib-1
	    jnd(2) = jb
	    iflag(2) = 2
	  endif
	  if((ib.eq.1).and.(jb.eq.14)) then
	    npts = 2
	    ind(1) = ib
	    jnd(1) = jb-1
	    iflag(1) = 3
	    ind(2) = ib+1
	    jnd(2) = jb
	    iflag(2) = 1
	 endif
	 if((ib.eq.14).and.(jb.eq.14)) then
	    npts = 2
	    ind(1) = ib
	    jnd(1) = jb-1
	    iflag(1) = 3
	    ind(2) = ib-1
	    jnd(2) = jb
	    iflag(2) = 2
	  endif
	  if((ib.eq.1).and.((jb.gt.1).and.(jb.lt.14)))then
	    npts = 3
	    ind(1) = ib+1
	    jnd(1) = jb
	    iflag(1) = 1
	    ind(2) = ib
	    jnd(2) = jb-1
	    iflag(2) = 3
	    ind(3) = ib
	    jnd(3) = jb+1
	    iflag(3) = 4
	  endif
	  if((ib.eq.14).and.((jb.gt.1).and.(jb.lt.14))) then
	    npts = 3
	    ind(1) = ib-1
	    jnd(1) = jb
	    iflag(1) = 2
	    ind(2) = ib
	    jnd(2) = jb-1
	    iflag(2) = 3
	    ind(3) = ib
	    jnd(3) = jb+1
	    iflag(3) = 4
	  endif
	  if((jb.eq.1).and.((ib.gt.1).and.(ib.lt.14)))then
	    npts = 3
	    ind(1) = ib
	    jnd(1) = jb+1
	    iflag(1) = 4
	    ind(2) = ib-1
	    jnd(2) = jb
	    iflag(2) = 2
	    ind(3) = ib+1
	    jnd(3) = jb
	    iflag(3) = 1
	  endif
	  if((jb.eq.14).and.((ib.gt.1).and.(ib.lt.14))) then
	    npts = 3
	    ind(1) = ib
	    jnd(1) = jb-1
	    iflag(1) = 3
	    ind(2) = ib-1
	    jnd(2) = jb
	    iflag(2) = 2
	    ind(3) = ib+1
	    jnd(3) = jb
	    iflag(3) = 1
	  endif
	  if((ib.gt.1).and.(ib.lt.14).and.(jb.gt.1).and.
     &          (jb.lt.14)) then
	    npts = 4
	    ind(1) = ib
	    jnd(1) = jb-1
	    iflag(1) = 3
	    ind(2) = ib
	    jnd(2) = jb+1
	    iflag(2) = 4
	    ind(3) = ib-1
	    jnd(3) = jb
	    iflag(3) = 2
	    ind(4) = ib+1
	    jnd(4) = jb
	    iflag(4) = 1
	  endif

c... selects, among the npts (maximum 4) neighbouring points,
c... those (npt) where the divergence can be really calculated,
c... thus giving npt equations ...

	  npt = 0
	  do num=1,npts
	    np = 0
	    if(fgood(ind(num)+1,jnd(num))) np = np+1
	    if(fgood(ind(num)-1,jnd(num))) np = np+1
	    if(fgood(ind(num),jnd(num)-1)) np = np+1
	    if(fgood(ind(num),jnd(num)+1)) np = np+1
	    if(np.ge.3) then
	      npt = npt+1
	      ind(npt) = ind(num)
	      jnd(npt) = jnd(num)
	      iflag(npt) = iflag(num)
	    endif
	  enddo

	  if(npt.ge.ncomp) then

c... Establishing the system of npt equations with ncomp unknowns

	    do i=1,4		!initializations
              bb(i) = 0.
	      do j=1,2 
	        t(i,j) = 0.
              enddo
	    enddo		!end initializations

c... Selects the 3x3 matrix for velocity components and the
c... 5x5 matrix for geographic coordinates surrounding the point
c... where the divergence has to be set to zero ...

	    do ieq=1,npt
	      do ic=1,3
		do jc=1,3
		  vvx(ic,jc) = vx(ind(ieq)-2+ic,jnd(ieq)-2+jc)
		  vvy(ic,jc) = vy(ind(ieq)-2+ic,jnd(ieq)-2+jc)
		  azz(1,ic,jc) = az(1,ind(ieq)-2+ic,jnd(ieq)-2+jc)
		  azz(2,ic,jc) = az(2,ind(ieq)-2+ic,jnd(ieq)-2+jc)
		enddo
	      enddo
	      do ic=0,4
		do jc=0,4
		  gglat(ic,jc) = latg(ind(ieq)-2+ic,jnd(ieq)-2+jc)
		  gglon(ic,jc) = long(ind(ieq)-2+ic,jnd(ieq)-2+jc)
		enddo
	      enddo

c... Calculates the coefficients of the linear system of equations ...

	      call DIV_DER(vvx,vvy,gglat,gglon,azz,ncomp,numc,
     &	      			iflag(ieq),a1,a2,b,vra)
	      t(ieq,1) = a1
	      bb(ieq) = b
	      if (ncomp.eq.1) then
		t(ieq,2) = 0.
	      else
		t(ieq,2) = a2
	      endif
	    enddo

c... Solves the linear system for the missing velocity component(s) ...

	    call SOLVE(t,bb,npt,ncomp,v)
	    if (ncomp.eq.1) then
	      vr(numc,ib,jb) = v(1)
	      lvr(numc,ib,jb) = .true.
	    else
	      do k=1,2
		vr(k,ib,jb) = v(k)
		lvr(k,ib,jb) = .true.
	      enddo
	    endif
	    fgood(ib,jb) = .true.
	    ngood = ngood + 1
	    denom = cosd(az(1,ib,jb))*sind(az(2,ib,jb)) -
     &              cosd(az(2,ib,jb))*sind(az(1,ib,jb))
	    vx(ib,jb) = (-sind(az(2,ib,jb))*vr(1,ib,jb) +
     &                   sind(az(1,ib,jb))*vr(2,ib,jb))/denom
	    vy(ib,jb) = (-cosd(az(2,ib,jb))*vr(1,ib,jb) +
     &                   cosd(az(1,ib,jb))*vr(2,ib,jb))/denom

c	  checks the divergence at surrounding points

c	  write(4,50) ib,jb,ncomp,numc,npt
	  do k=1,npt
	    do ic=1,3
              do jc=1,3
	        vvx(ic,jc)=vx(ind(k)-2+ic,jnd(k)-2+jc)
	        vvy(ic,jc)=vy(ind(k)-2+ic,jnd(k)-2+jc)
	      enddo
	    enddo
	    do ic=0,4
	      do jc=0,4
	        gglat(ic,jc)=latg(ind(k)-2+ic,jnd(k)-2+jc)
	        gglon(ic,jc)=long(ind(k)-2+ic,jnd(k)-2+jc)
	      enddo
	    enddo
	    call div_calc(vvx,vvy,gglat,gglon,div)
c	    write(4,52) k,ind(k),jnd(k),iflag(k),div
c 50	    format(/,' ib,jb, ncomp, numc, npt = ',5i4)
c 52	    format(' k, ind, jnd, iflag, div = ',4i4,f10.3)
	  enddo

	  endif
10        continue

	enddo			!end of jb loop
      enddo			!end of ib loop

      return 
      end
