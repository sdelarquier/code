c**************************************************************************
C File: /home/darn/gen/radcoor.f
C Last Modification: 13-MAR-2002
C Modified by: Dieter Andre
C
 	subroutine RADCOOR(st_id,year,yr_sec, frang, rsep,height,lat,lon,rho)
c
C Revision 2.0; 20020313; Dieter Andre
C Use frang and rsep
C Replaced call to rbpos_center_coord with call to rbpos_gen
c
c Last written:  Aug 8/94
c *************************************************************************
	real*4      lat(0:15,75),lon(0:15,75),rho(0:15,75)
	integer*2   st_id, ja, ia
	real*4      frang, rsep
	real*4      height, pos(3,2,2),xho
	integer*2   mgflag,err
	logical*1   center_flag, gs_flag
	integer*2   year
	integer*4   yr_sec
	data        mgflag/0/

	center_flag= .true.
	gs_flag= .false.
	do ja = 0,15
	  do ia = 1,75
	    call rbpos_gen( ia, ja, frang, rsep, height, pos, st_id, year, yr_sec, mgflag, center_flag, gs_flag, err)
	    lat(ja,ia)  =  pos(1,1,1)
	    lon(ja,ia)  =  pos(2,1,1)
	    rho(ja,ia)  =  pos(3,1,1)
	  end do
	end do
	return
	end






