	function STATION(letter)
c	************************

c***************************************************************
c Sets up the station ID index from the station letter
c
c	$Log:	station.f,v $
c Revision 1.1  94/09/01  16:14:03  16:14:03  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c***************************************************************

	character*1 letter, let(12)
	integer*2 station
	data let/'g','s','k','h','t','b','a','w','e','f','d','j'/
c_____________________

	station = 0
	do i=1,12
	  if(letter.eq.let(i)) station = i 
	enddo
	if(station.eq.0) then
	  print *,' Wrong file name'
	  stop
	endif
	return
	end
