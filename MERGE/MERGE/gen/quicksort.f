	subroutine quicksort(array,n)
c
c	quicksort a real*4 array
c
c	order is from minimum to maximum
c
c       $Log:	quicksort.f,v $
c Revision 1.1  94/08/04  10:26:38  10:26:38  baker (Kile Baker S1G)
c Initial revision
c 
c

	integer*2 n
	real*4 array(n)
	integer*2 pivot,lv(20),uv(20),p
	lv(1)=1
	uv(1)=n
	p=1
	do while (p.gt.0)
	  if (lv(p).ge.uv(p))then
	    p = p-1		!pop the stack
	  else
	    i=lv(p)-1
	    j=uv(p)
	    pivot=j
	    do while (i.lt.j)
	      i=i+1
	      do while (array(i).lt.array(pivot))
		i=i+1
	      end do
	      j=j-1
	      do while (j.gt.i)
		if(array(j).le.array(pivot))goto 5
		j=j-1
	      end do
5	      if (i.lt.j)then
		temp=array(i)
		array(i)=array(j)
		array(j)=temp
	      end if
 	    end do
c
	    j=uv(p)		!move pivot
	    temp=array(i)
	    array(i)=array(j)
	    array(j)=temp
	    if (i-lv(p) .lt. uv(p)-i)then
	      lv(p+1)=lv(p)
	      uv(p+1)=i-1
	      lv(p)=i+1
	    else
	      lv(p+1)=i+1
	      uv(p+1)=uv(p)
	      uv(p)=i-1
	    end if
	    p=p+1
	  end if
	end do
	return
	end

