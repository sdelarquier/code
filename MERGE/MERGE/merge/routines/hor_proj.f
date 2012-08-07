	subroutine HOR_PROJ(ind,vx,vy,vz,bx,by,bz)
c	**************************************

c************************************************************************
c For ind = +1, this routine calculates the projection of the velocity
c onto the horizontal plane. This projection is along the magnetic field.
c For ind = -1, do the inverse transformation.
c
c Last written: Oct. 93 (JCC)
c
c	$Log:	hor_proj.f,v $
c Revision 1.1  94/09/01  16:13:48  16:13:48  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c************************************************************************

	real*4 vx(0:15,0:15),vy(0:15,0:15),vz(0:15,0:15)
	real*4 bx(0:15,0:15),by(0:15,0:15),bz(0:15,0:15)
c__________________

	do ib=0,15
	  do jb=0,15
	    if(ind.eq.+1) then
	      bxbz = bx(ib,jb)/bz(ib,jb)
	      vx(ib,jb) = vx(ib,jb) - vz(ib,jb)*bxbz
	      bybz = by(ib,jb)/bz(ib,jb)
	      vy(ib,jb) = vy(ib,jb) - vz(ib,jb)*bybz
	      vz(ib,jb) = 0.
	    else if(ind.eq.-1) then
	      den = bx(ib,jb)**2 + by(ib,jb)**2 + bz(ib,jb)**2
	      top = vx(ib,jb)*bx(ib,jb) + vy(ib,jb)*by(ib,jb)
	      alpha = -top/den
	      vx(ib,jb) = vx(ib,jb) + alpha*bx(ib,jb)
	      vy(ib,jb) = vy(ib,jb) + alpha*by(ib,jb)
	      vz(ib,jb) = alpha*bz(ib,jb)
	    endif
	  enddo
	enddo
	return
	end
