	subroutine PAIR(r1,r2,ier)
c	**************************
c
c**********************************************************************
c Determines if radar pair is allowed
c Inputs: radar IDs r1 and r2
c Outputs: error index ier
c	ier = 0		Nominal pair of radars
c	ier = 1		Radar1 is east of Radar2
c	ier = 2		Fields of view overlap, but not a nominal pair
c	ier = 3		Radars are not in the same hemisphere
c	ier = 4		Bad pair of radars: fields of view
c			do not overlap
c
c Last written: May 93 (CS)
c
c	$Log:	pair.f,v $
c Revision 1.1  94/09/05  12:58:49  12:58:49  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c**********************************************************************

	integer*2 r1, r2
	integer*2 radar_pos_year
	integer*4 radar_pos_sec
	real*4    pos1(5), pos2(5), pos
c__________________________

	ier = 4
	radar_pos_year = 1999
	radar_pos_sec  = 0
	call RADAR_POS(r1,radar_pos_year,radar_pos_sec,pos1)
	call RADAR_POS(r2,radar_pos_year,radar_pos_sec,pos2)

c... Radar1 must be west of radar2 ...

	if(pos1(2).gt.pos2(2)) then
	  ier = 1
	endif

c... Radars must be in the same hemisphere ...

	pos = pos1(1) * pos2(1)
	if(pos.lt.0.) then
	  ier = 3
	  return
	endif

c... Looks for compatible pairs ...

	if(r1.eq.7) then
	  if(r2.eq.6) ier = 0
	  if((r2.eq.5).or.(r2.eq.3)) ier = 2
	endif

	if(r1.eq.6) then
	  if(r2.eq.3) ier = 2
	endif

	if(r1.eq.5) then
	  if(r2.eq.3) ier = 0
	  if((r2.eq.1).or.(r2.eq.8)) ier = 2
	endif

	if(r1.eq.3) then
	  if((r2.eq.1).or.(r2.eq.8)) ier = 2
	endif

	if(r1.eq.2) then
	  if(r2.eq.1) ier = 0
	endif

	if(r1.eq.1) then
	  if(r2.eq.8) ier = 0
	endif

	if(r1.eq.9) then
	  if(r2.eq.10) ier = 0
	endif

	if(r1.eq.4) then
	  if((r2.eq.11).or.(r2.eq.12)) ier = 0
	endif

	if(r1.eq.11) then
	  if(r2.eq.12) ier = 0
	endif

	return
	end
