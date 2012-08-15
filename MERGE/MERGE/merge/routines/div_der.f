	subroutine DIV_DER(vx,vy,glat,glon,az,ncomp,numc,
     &				ide,a1,a2,sum,vra)
c	*************************************************

c*********************************************************************
c This routine calculates the coefficients of the linear equation
c (a1*v1+a2*v2 = sum) deduced from the zero divergence condition. 
c v1 and v2 are the radial velocity components at the point of the
c 3x3 matrix determined by ide (ide =1 to 4).
c
c Last written: Oct. 93 (JCC)
c
c	$Log:	div_der.f,v $
c Revision 1.1  94/09/01  16:13:43  16:13:43  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c*********************************************************************

	integer*2  ide
	real*4	vx(3,3),vy(3,3),az(2,3,3)
	real*4  glat(0:4,0:4),glon(0:4,0:4)        
	real*4  dvx_dv(2),dvy_dv(2)
	real*4  ddiv_dvx(3,3),ddiv_dvy(3,3)
	real*4  ddiv_dv(2,3,3),vrad(2)
c__________________

c... Initializations ...

	  a1 = 0.
	  a2 = 0.
	  sum = 0.

	  do k=1,2
	    dvx_dv(k) = 0.
	    dvy_dv(k) = 0.
	  enddo

	  do i=1,3
	    do j=1,3
	      ddiv_dvx(i,j) = 0.
	      ddiv_dvy(i,j) = 0.
              do ir=1,2
	        ddiv_dv(ir,i,j) = 0.   
              enddo
	    enddo
	  enddo

c... Now calculates the derivatives of the divergence  with
c... respect to cartesian velocity components ...

	i = 3
	j = 2
	dlat2 = ( glat(i,j+1) - glat(i,j-1) )/2.
	dlon2 = ( glon(i,j+1) - glon(i,j-1) )/2.
	ddiv_dvx(i,j) = dlon2 * cosd(glat(i,j))
	ddiv_dvy(i,j) = dlat2

	i = 1
	j = 2
	dlat2 = ( glat(i,j+1) - glat(i,j-1) )/2.
	dlon2 = ( glon(i,j+1) - glon(i,j-1) )/2.
	ddiv_dvx(i,j) = -dlon2 * cosd(glat(i,j))
	ddiv_dvy(i,j) = -dlat2

	i = 2
	j = 3
	dlat1 = ( glat(i+1,j) - glat(i-1,j) )/2.        
	dlon1 = ( glon(i+1,j) - glon(i-1,j) )/2.        
	ddiv_dvx(i,j) = -dlon1 * cosd(glat(i,j))
	ddiv_dvy(i,j) = -dlat1

	i = 2
	j = 1
	dlat1 = ( glat(i+1,j) - glat(i-1,j) )/2.        
	dlon1 = ( glon(i+1,j) - glon(i-1,j) )/2.        
	ddiv_dvx(i,j) = dlon1 * cosd(glat(i,j))
	ddiv_dvy(i,j) = dlat1


	do i=1,3
	  do j=1,3
	    if(((i+j).eq.3).or.((i+j).eq.5)) then

			!Derivatives of cartesian velocity
			!components with respect to radar
			!l.o.s. velocity components
	      denom = cosd(az(1,i,j))*sind(az(2,i,j)) -
     &                cosd(az(2,i,j))*sind(az(1,i,j))
	      dvx_dv(1) = -sind(az(2,i,j))/denom
	      dvx_dv(2) =  sind(az(1,i,j))/denom
	      dvy_dv(1) = -cosd(az(2,i,j))/denom
	      dvy_dv(2) =  cosd(az(1,i,j))/denom

			!Derivatives of the divergence
			!with respect to radar l.o.s. 
			!velocity components
	      do k=1,2
	        ddiv_dv(k,i,j) = ddiv_dvx(i,j)*dvx_dv(k) +
     &				 ddiv_dvy(i,j)*dvy_dv(k)
	      enddo
	    endif
	  enddo
	enddo

        
	sum=0.
	do i=1,3
	  do j=1,3
	    if(((i+j).eq.3).or.((i+j).eq.5)) then
	      if((i.eq.1).and.(j.eq.2).and.(ide.eq.1)) goto 10
	      if((i.eq.3).and.(j.eq.2).and.(ide.eq.2)) goto 10
	      if((i.eq.2).and.(j.eq.3).and.(ide.eq.3)) goto 10
	      if((i.eq.2).and.(j.eq.1).and.(ide.eq.4)) goto 10
	      do k=1,2
	        vrad(k) = -cosd(az(k,i,j))*vx(i,j) +
     &	      		   sind(az(k,i,j))*vy(i,j)
		sum = sum - ddiv_dv(k,i,j)*vrad(k)
	      enddo
	      goto 20
10            id = i
	      jd = j
20            continue
	    endif
	  enddo  
	enddo

	if (ncomp.eq.1) then
	  a1 = ddiv_dv(numc,id,jd)
	  if(numc.eq.1) then
	    sum = sum - ddiv_dv(2,id,jd)*vra
	  else
	    sum = sum - ddiv_dv(1,id,jd)*vra
	  endif
	else
	  a1 = ddiv_dv(1,id,jd)
	  a2 = ddiv_dv(2,id,jd)
	endif

	return
	end
