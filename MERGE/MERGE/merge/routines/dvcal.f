	subroutine DVCAL(ib,jb,flag,v,dv)
c	***********************************

c***************************************************************
c This routine returns in dv the deviation of the velocity at
c (ib,jb) from the mean velocity of the surrounding 9x9 points
c (including the point itself). It is a measure of the local
c non-uniformity of the flow.
c
c	$Log:	dvcal.f,v $
c Revision 1.1  94/09/01  16:13:46  16:13:46  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c***************************************************************

	real*4		v(0:15,0:15)
	logical*1	flag(-1:16,-1:16)
c______________

	nr = 0
	asumr = 0.
	do i=ib-1,ib+1
	  do j=jb-1,jb+1
	    if(flag(i,j)) then
	      nr = nr+1
	      asumr = asumr + v(i,j)
	    endif
	  enddo
	enddo

	if(nr.ge.2) then
	  vravg = asumr/nr
	  dv = abs( v(ib,jb) - vravg )
	else
	  dv = 50.
	endif
	return
	end
