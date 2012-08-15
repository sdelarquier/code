	character*20 function PI_NAME(cp)
c	**********************************

c***********************************************************************
c This routine finds the PI to be contacted from the cp code
c
c First written: August 95 (CS)
c
c	$Log:	pi_name.f,v $
c Revision 1.1  95/08/03  16:23:51  16:23:51  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c***********************************************************************

	integer*2	cp

	cp = cp/1000

	if(cp.eq.1) pi_name = 'Ray Greenwald'
	if(cp.eq.2) pi_name = 'Alan Rodger'
	if(cp.eq.3) pi_name = 'Georges Sofko'
	if(cp.eq.4) pi_name = 'Jean-Paul Villain'
	if(cp.eq.5) pi_name = 'Natsuo Sato'
	if(cp.eq.6) pi_name = 'Tudor Jones'
	if(cp.eq.7) pi_name = 'David Walker'
	if(cp.ge.8) pi_name = 'Unknown PI'

	return
	end
