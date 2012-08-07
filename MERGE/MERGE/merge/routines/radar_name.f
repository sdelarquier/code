	character*20 function RADAR_NAME(st)
c	************************************

c************************************************************
c
c	$Log: radar_name.f,v $
c	Revision 1.2  1996/09/11 09:40:24  senior
c	Changed E and F names
c
c Revision 1.1  94/09/01  16:13:58  16:13:58  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.2 $
c***********************************************************

	integer*2 st

	if(st.eq.1 ) radar_name = 'GOOSE-BAY'
	if(st.eq.2 ) radar_name = 'SCHEFFERVILLE'
	if(st.eq.3 ) radar_name = 'KAPUSKASING'
	if(st.eq.4 ) radar_name = 'HALLEY-BAY'
	if(st.eq.5 ) radar_name = 'SASKATOON'
	if(st.eq.6 ) radar_name = 'BRITISH COLUMBIA'
	if(st.eq.7 ) radar_name = 'KING SALMON'
	if(st.eq.8 ) radar_name = 'STOKKSEYRI'
	if(st.eq.9 ) radar_name = 'THYKKVIBAER'
	if(st.eq.10) radar_name = 'HANKASALMI'
	if(st.eq.11) radar_name = 'SANAE'
	if(st.eq.12) radar_name = 'SYOWA'

	return
	end
