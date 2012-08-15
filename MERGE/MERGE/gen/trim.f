c
c       $Id$
c
c       $Log:	trim.f,v $
c Revision 1.1  94/07/08  11:25:57  11:25:57  baker (Kile Baker S1G)
c Initial revision
c 
c
	integer*4 function trim(ss)
	character*(*) ss
c
c	this function returns the length of a string
c	when trailing blanks are ignored.
c
	l1=len(ss)
	do while (l1.gt.0 .and. ss(l1:l1).eq.' ')
	  l1=l1-1
	end do
	trim=l1
	return
	end

