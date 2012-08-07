	function QUALITY_MAP(ngood,fgood,pnt_qlty)
c	******************************************

c*******************************************************************
c This routine computes the map quality index. This index is made
c of 4 digits, such that
c
c   quality_map = qlty(1)*1000 + qlty(2)*100 + qlty(3)*10 + qlty(4)
c
c where the last 3 digits having the same meaning as for the point
c quality index.
c
c The first digit refers to the number of points in the map:
c		qlty(1) = 0:	  0 <= n < 20
c		qlty(1) = 1:	 20 <= n < 40
c		qlty(1) = 2:	 40 <= n < 60
c		qlty(1) = 3:	 60 <= n < 80
c		qlty(1) = 4:	 80 <= n < 100
c		qlty(1) = 5:	100 <= n < 120
c		qlty(1) = 6:	120 <= n < 140
c		qlty(1) = 7:	140 <= n < 160
c		qlty(1) = 8:	160 <= n < 180
c		qlty(1) = 9:	180 <= n < 16x16
c
c Last written: Nov. 1993 (CS)
c
c	$Log:	quality_map.f,v $
c Revision 1.1  94/09/01  16:13:56  16:13:56  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c*******************************************************************

	integer*2 ngood, pnt_qlty(0:15,0:15)
	integer*4 qlty(4), QUALITY_MAP
	logical*1 fgood(0:15,0:15)
c_______________

	do i=1,4
	  qlty(i) = 0
	enddo

	do ib=0,15
	  do jb=0,15
	    if(fgood(ib,jb)) then
	      m1 = pnt_qlty(ib,jb)/100
	      m2 = (pnt_qlty(ib,jb) - m1*100) / 10
	      m3 = pnt_qlty(ib,jb) - (m1*100 + m2*10)
	      qlty(2) = qlty(2) + m1
	      qlty(3) = qlty(3) + m2
	      qlty(4) = qlty(4) + m3
	    endif
	  enddo
	enddo

	do i=2,4
	  if(ngood.ne.0) q = qlty(i)/ngood
	  qlty(i) = nint(q)
	enddo

	min = 0
	do i=0,9
	  max = min + 20
	  if(i.eq.9) max = 256
	  if((ngood.ge.min).and.(ngood.lt.max)) qlty(1) = i
	  min = min + 20
	enddo

	QUALITY_MAP = qlty(1)*1000+qlty(2)*100+qlty(3)*10+qlty(4)

	return
	end
