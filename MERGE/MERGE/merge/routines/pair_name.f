	character*1 function PAIR_NAME(st1,st2)
c	***************************************

c***********************************************************************
c Attributes a letter to a pair of radars
c
c	$Log:	pair_name.f,v $
c Revision 1.1  95/01/11  09:43:25  09:43:25  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c***********************************************************************

	integer*2	st1,st2
c__________________

c... Northern Hemisphere Nominal Pairs

	if((st1.eq.7).and.(st2.eq.6)) PAIR_NAME = 'A'
	if((st1.eq.5).and.(st2.eq.3)) PAIR_NAME = 'B'
	if((st1.eq.1).and.(st2.eq.8)) PAIR_NAME = 'C'
	if((st1.eq.9).and.(st2.eq.10)) PAIR_NAME = 'D'

c... Northern Hemisphere Non-Nominal Pairs

	if((st1.eq.7).and.(st2.eq.5)) PAIR_NAME = 'E'
	if((st1.eq.7).and.(st2.eq.3)) PAIR_NAME = 'F'
	if((st1.eq.6).and.(st2.eq.3)) PAIR_NAME = 'G'
	if((st1.eq.5).and.(st2.eq.1)) PAIR_NAME = 'H'
	if((st1.eq.5).and.(st2.eq.8)) PAIR_NAME = 'I'
	if((st1.eq.3).and.(st2.eq.1)) PAIR_NAME = 'J'
	if((st1.eq.3).and.(st2.eq.8)) PAIR_NAME = 'K'

c... Southern Hemisphere Pairs

	if((st1.eq.4).and.(st2.eq.11)) PAIR_NAME = 'M'
	if((st1.eq.4).and.(st2.eq.12)) PAIR_NAME = 'N'
	if((st1.eq.11).and.(st2.eq.12)) PAIR_NAME = 'O'

	return
	end
