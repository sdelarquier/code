	subroutine DIV_FREE(vx,vy,latg,long,az,fgood,n_iter)
c	****************************************************

c**********************************************************************
c This routine makes the divergence free analysis for correcting the
c map. It is done in generalized radar coordinates. The principle is
c an iterative method consisting in two steps:
c - Determines the grid point where the absolute value of the diver-
c   gence is maximum (at the beam pair (imax,jmax));
c - Reduces the divergence (DIV_CORRECT) at that point by modifying
c   the velocity at the four neigbouring grid points.
c
c The relaxation analysis stops when the mean divergence of the map
c is below a predetermined value (divmin), or when the maximum number
c of steps (nmax) is reached. nmax is set to 20 times the number of
c valid data points in the map.
c The divergence at each step (at grid point (imax,jmax)) is reduced
c to zero.
c
c ATTENTION: This method is not valid if the field of view of the
c *********  radars covers the geographic pole. This is the case
c *********  for all the present radars of the southern hemisphere.
c
c Last written: Oct. 93 (JCC)
c
c	$Log:	div_free.f,v $
c Revision 1.1  94/09/01  16:13:45  16:13:45  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c*********************************************************************

	include 'info.inc'
	real*4  vx(0:15,0:15),vy(0:15,0:15),vlos(2,0:15,0:15)
	real*4  latg(-1:16,-1:16),long(-1:16,-1:16),az(2,-1:16,-1:16)
	real*4  vvx(3,3),vvy(3,3),dvvx(3,3),dvvy(3,3)
	real*4	gglat(0:4,0:4),gglon(0:4,0:4)
	real*4  azz(2,3,3),vrad(2,3,3)
	logical*1 fgood(0:15,0:15),flag(-1:16,-1:16)
	logical*1 fvlos(2,0:15,0:15)
	common/vradial/vlos,fvlos
	data divmin/10./
c_______________

c... Initializations ...

	n_iter = 0
	div = 0.

	do i=1,3
	  do j=1,3
	    vvx(i,j) = 0.
	    vvy(i,j) = 0.
	    dvvx(i,j) = 0.
	    dvvy(i,j) = 0.
	    do k=1,2
	      vrad(k,i,j) = 0.
	    enddo
	  enddo
	enddo

	do ib=-1,16
	  do jb=-1,16
	    flag(ib,jb) = .false.
	  enddo
	enddo

	do ib = 0,15
	  do jb = 0,15
	    flag(ib,jb) = fgood(ib,jb)
	  enddo
	enddo

c... Starts the calculation  ...

 10	divmax = 0.
	divmean = 0.
	nptdiv = 0


c... Tests if the divergence at point ib,jb can be calculated ...

	do ib=1,14
	  do jb=1,14
            if (fgood(ib,jb+1).and.fgood(ib,jb-1).
     &          and.fgood(ib+1,jb).and.fgood(ib-1,jb)) then

c... Selects the 3x3 matrix for velocity components and the
c... 5x5 matrix for geographic coordinates surrounding the
c... point where the divergence is to be calculated ...

	      do ic=1,3                
	        do jc=1,3
		  vvx(ic,jc) = vx(ib-2+ic,jb-2+jc)
		  vvy(ic,jc) = vy(ib-2+ic,jb-2+jc)
	        enddo
              enddo

	      do ic=0,4                
	        do jc=0,4
	          gglat(ic,jc) = latg(ib-2+ic,jb-2+jc)
	          gglon(ic,jc) = long(ib-2+ic,jb-2+jc)
	        enddo
	      enddo

c... calculates and eventually stores the divergence ...

	      call DIV_CALC(vvx,vvy,gglat,gglon,div)
	      adiv = abs(div)
	      divmean = divmean + adiv
	      nptdiv = nptdiv + 1
	      if(adiv.gt.abs(divmax)) then
	        divmax = div			!select the point
		imax = ib			!where the div. is max
	        jmax = jb
	      endif
            endif
	  enddo
        enddo

	if(nptdiv.eq.0) then
	  print*, 'The divergence cannot be computed for this map'
	  return
	endif

        divmean = divmean/nptdiv
        print 100, n_iter,divmean,divmax,imax,jmax
 100	format('n_iter = ',i4,' divmean = ',e10.3,' divmax = ',e10.3,
     &		' at i= ',i2,' j= ',i2)

	if(idiv.eq.0) return

	if(n_iter.eq.0) nmax = 20*nptdiv
        if (divmean.le.divmin) go to 30

c... Now reduces the divergence at the point where it is maximum

c... First, initializes the 3x3 and 5x5 matrices

        do ic=1,3                
          do jc=1,3

 					!velocities
	    vvx(ic,jc) = vx(imax-2+ic,jmax-2+jc)
	    vvy(ic,jc) = vy(imax-2+ic,jmax-2+jc)

 					!inhomogeneity of
					!the velocity field
	    if(flag(imax-2+ic,jmax-2+jc)) then
	      call DVCAL(imax-2+ic,jmax-2+jc,flag,vx,vdif)
	      dvvx(ic,jc) = vdif
	      call DVCAL(imax-2+ic,jmax-2+jc,flag,vy,vdif)
	      dvvy(ic,jc) = vdif
	    endif

	 				!azimuths
	    do k=1,2
	      azz(k,ic,jc) = az(k,imax-2+ic,jmax-2+jc)
	    enddo
       	  enddo
        enddo
					!coordinates
        do ic=0,4                
          do jc=0,4
	    gglat(ic,jc) = latg(imax-2+ic,jmax-2+jc)
	    gglon(ic,jc) = long(imax-2+ic,jmax-2+jc)
	  enddo
        enddo

c... Makes the correction to the divergence ...

	n_iter = n_iter + 1
        call DIV_CORRECT(vvx,vvy,dvvx,dvvy,gglat,gglon,azz,divmax,vrad)

c... Replaces the old velocities by the corrected values ...

        do ic=1,3                
          do jc=1,3
	    if(((ic+jc).eq.3).or.((ic+jc).eq.5)) then
              vx(imax-2+ic,jmax-2+jc) = vvx(ic,jc)
              vy(imax-2+ic,jmax-2+jc) = vvy(ic,jc)
	      do k=1,2
	        vlos(k,imax-2+ic,jmax-2+jc) = vrad(k,ic,jc)
	      enddo
	    endif
          enddo
        enddo

        if(n_iter.ge.nmax) go to 20

        go to 10	!computes again the whole map of divergence

 20     print*, 'The divergence cannot be reduced below',divmin,
     &    	' within ',n_iter,' iterations'
	write(4,120) divmin,n_iter
 30     print* ,'The divergence is reduced below ',divmean,' in ',
     &    	n_iter,' iterations'
	write(4,130) divmean,n_iter

 120     format(' The divergence cannot be reduced below',e10.3,
     &    	' within ',i4,' iterations')
 130     format(' The divergence is reduced below ',e10.3,' in ',
     &    	i4,' iterations')

        return
        end
