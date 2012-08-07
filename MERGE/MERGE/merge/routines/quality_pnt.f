	function QUALITY_PNT(num,wid,angle)
c	***********************************

c**********************************************************************
c This routine returns the quality index for individual vectors. This
c index is made of 3 digits such as
c	QUALITY_PNT = qlty(1)*100 + qlty(2)*10 + qlty(3)
c where:
c
c - qlty(1) depends on the number of gates num(1) and num(2), from each
c   radar, falling into a the diamond:
c	qlty(1) = 0:	0 x 0
c	qlty(1) = 1:	0 x n
c	qlty(1) = 2:	1 x 1
c	qlty(1) = 3:	1 x 2
c	qlty(1) = 4:	1 x 3 (or more than 3)
c	qlty(1) = 5:	2 x 2
c	qlty(1) = 6:	2 x 3 (or more than 3)
c	qlty(1) = 7:	3 (or more than 3) x 3 (or more than 3)
c
c - qlty(2) depends on the spectral width 'wid' averaged from all
c   valid points falling into the diamond (from the two radars):
c	qlty(2) = 0:	        wid >= 500.
c	qlty(2) = 1:	400. <= wid <  500.
c	qlty(2) = 2:	300. <= wid <  400.
c	qlty(2) = 3:	200. <= wid <  300.
c	qlty(2) = 4:	100. <= wid <  200.
c	qlty(2) = 5:	  0. <= wid <  100.
c
c - qlty(3) depends on the angle between the two radar beams at the
c   grid node:
c	qlty(3) = 0:	  0. <= angle < 15.
c	qlty(3) = 1:	 15. <= angle < 30.
c	qlty(3) = 2:	 30. <= angle
c
c Last written: Oct. 93 (CS)
c
c	$Log:	quality_pnt.f,v $
c Revision 1.1  94/09/01  16:13:57  16:13:57  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c**********************************************************************

	integer*2 qlty(3),quality_pnt
	integer*2 num(2),n1,n2
c_____________________

	if(num(1).lt.num(2)) then
	  n1 = num(1)
	  n2 = num(2)
	else
	  n1 = num(2)
	  n2 = num(1)
	endif
	if(n1.gt.3) n1 = 3
	if(n2.gt.3) n2 = 3
	if((n1.eq.0).and.(n2.eq.0)) qlty(1) = 0
	if((n1.eq.0).and.(n2.ne.0)) qlty(1) = 1
	if((n1.eq.1).and.(n2.eq.1)) qlty(1) = 2
	if((n1.eq.1).and.(n2.eq.2)) qlty(1) = 3
	if((n1.eq.1).and.(n2.ge.3)) qlty(1) = 4
	if((n1.eq.2).and.(n2.eq.2)) qlty(1) = 5
	if((n1.eq.2).and.(n2.ge.3)) qlty(1) = 6
	if((n1.ge.3).and.(n2.ge.3)) qlty(1) = 7

	widmin = 500.
	do i=1,6
	  widmax = widmin + 100.
	  if(i.eq.1) widmax = 9999.
	  if((wid.ge.widmin).and.(wid.lt.widmax)) qlty(2) = i-1
	  widmin = widmin - 100.
	enddo

	if((angle.ge.0.).and.(angle.lt.15.))  qlty(3) = 0
	if((angle.ge.15.).and.(angle.lt.30.)) qlty(3) = 1
	if(angle.ge.30.) qlty(3) = 2

	quality_pnt = qlty(1)*100 + qlty(2)*10 + qlty(3)

	return
	end
