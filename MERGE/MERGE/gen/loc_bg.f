C File: /home/darn/gen/loc_bg.f
C Last Modification: 13-MAR-2002
C Modified by: Dieter Andre
C
	program loc_bg
c
C Revision 2.0; 20020313; Dieter Andre
C Use frang and rsep
c Test program
c Outputs the beam amd range gate for a given geographic location and SuperDARN radar

	implicit none
	integer h
	integer*2	st_id, beam, gate
	real*4		frang, rsep
	real*4		gdlat,gdlon,grho,glat,glon,height,del

	print *,'Input st_id, lat, long'
	read *,st_id, gdlat, gdlon
	frang= 180.0
	rsep= 45.0
	height=0.0

	call geodtgc(1,gdlat,gdlon,grho,glat,glon,del)
	do h=0,300,50
	   height=real(h)
	   CALL LOCBG(st_id, frang, rsep,glat,glon,height,beam,gate)	
	   write(*,*) height,beam,gate
	enddo
	end

