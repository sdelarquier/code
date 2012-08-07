	subroutine SOLVE(a,b,mp,np,x)    
c	*****************************

c************************************************************
c solves the linear system a.x=b of mp (.le.4) equations
c with np (.le.2) unknowns.                
c
c Last written: Oct. 93 (JCC)
c
c	$Log:	solve.f,v $
c Revision 1.1  94/09/01  16:14:02  16:14:02  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c************************************************************

	real*4 a(4,2),b(4),x(2) 
	real*4 p(2,2),inv_p(2,2),atb(2)
c______________

c... Calculates the product p=at*a  (at=transpose of a) ...

	do i=1,np
	  do j=1,np
	    p(i,j) = 0.
	    do k=1,mp
	     p(i,j) = p(i,j) + a(k,i)*a(k,j)
	    end do
	  end do
	end do

c... Calculates the inverse of p ...

	if (np.eq.2) then
	  det = p(1,1)*p(2,2) - p(1,2)*p(2,1)
	  inv_p(1,1) =  p(2,2)/det
	  inv_p(2,2) =  p(1,1)/det
	  inv_p(1,2) = -p(1,2)/det
	  inv_p(2,1) = -p(2,1)/det
	else
	  inv_p(1,1) = 1./p(1,1)
	end if

c... Calculates the product at*b ...

	do i=1,np
	  atb(i) = 0.
	    do k=1,mp
	      atb(i) = atb(i) + a(k,i)*b(k)
	    end do
	  end do

c... Calculates the solution v ...

	do i=1,np
	  x(i) = 0.
	  do k=1,np
	    x(i) = x(i) + inv_p(i,k)*atb(k)
	  end do
	end do

	return
	end
