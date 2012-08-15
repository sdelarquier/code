	function LETTER(st)
c	*******************
c
c*******************************************************
c	Set the identification letter of the station
c
c	$Log:	letter.f,v $
c Revision 1.1  94/09/01  16:13:48  16:13:48  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c*******************************************************
c
	character*1 letter, let(12)
	integer*2 st
	data let/'g','s','k','h','t','b','a','w','e','f','d','j'/
c________________________

	letter = let(st)
	return
	end
