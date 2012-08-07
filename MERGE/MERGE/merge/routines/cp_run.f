	subroutine CP_RUN(cp1,cp2)
c	**************************

c**********************************************************************
c This subroutine checks that the files being processed correspond
c to Common Programs. If this is not the case, the user is warned,
c and asked if he still wants to process the data. 
c This routine is called in READ_INIT, only from August 1995 when
c the SuperDARN schedule was set for the first time.
c
c First written: August 1995
c
c	$Log: cp_run.f,v $
c	Revision 1.3  1996/09/11 09:16:24  senior
c	Modified to take into account the new CP codes
c	defined by Kile Baker on August 29, 1995
c
c Revision 1.1  95/08/03  16:25:25  16:25:25  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.3 $
c*********************************************************************

	include		'info.inc'
	integer*2	cp1,cp2,cp(2)
	integer*2	st(2)
	character*20	r, RADAR_NAME
	character*20	pi,PI_NAME
	character*1	ans
c_____________________________

	if( (cp1.gt.0).and.(cp1.lt.1000).and.(cp2.gt.0).and.(cp2.lt.1000) ) 
     &			return		!This is a Common Program
	cp(1) = cp1
	cp(2) = cp2
	st(1) = st1
	st(2) = st2

	do k=1,2
	   r = RADAR_NAME(st(k))
	   print *,' '

	   if(cp(k).ge.1000) then		!Special Time
	      print *,'Special Program for ', r
	      print *,'****** CP=',cp(k)
	      pi = PI_NAME(cp(k))
	      print *,'For authorization to use these data, please contact: ',pi
	   endif

	   if(cp(k).lt.0) then			!Discretionary Time
	      if(abs(cp(k)).lt.1000) then
		 print *,'Common Mode run as Discretionary for ', r
	         print *,'****** CP=',cp(k)
		 if(st(k).eq.1.or.st(k).eq.3)  cp(k) = 1000
		 if(st(k).eq.4)  cp(k) = 2000
		 if(st(k).eq.5)  cp(k) = 3000
		 if(st(k).eq.8)  cp(k) = 4000
		 if(st(k).eq.12) cp(k) = 5000
		 if(st(k).eq.9.or.st(k).eq.10) cp(k) = 6000
		 if(st(k).eq.11) cp(k) = 7000
	      else
		 print *,'Discretionary Program for ', r 
	         print *,'****** CP=',cp(k)
		 cp(k) = -cp(k)
	      endif
	      pi = PI_NAME(cp(k))
	      print *,'For authorization to use these data, please contact: ',pi
	   endif
	enddo

C 15-MAY-1997 DA
C	print *,' '
C	print *,'Do you still want to process these data? (y or n)'
C	read '(a)', ans
C	if((ans.eq.'n').or.(ans.eq.'N')) stop

	return
	end
