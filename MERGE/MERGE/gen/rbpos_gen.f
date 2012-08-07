c***************************************************************************
C File: /home/darn/gen/rbpos_gen.f
C Last Modification: 13-MAR-2002
C Modified by: Dieter Andre
C
	SUBROUTINE RBPOS_gen(	range,			! Index of range gate, first one is 1
     #				bmnum,			! beam number [0-15]
     #				first_range,		! Range to first sample [km]
     #				range_separation,	! Distance between range gates [km]
     #				height,			! <90 [deg]; >=90 [km]
     #				pos,			! see doc
     #				st_id,			! station id
     #				year,			! of operation
     #				yr_sec,			! second of year
     #				mgflag,			!
     #				center_flag,		!
     #				gs_flag,		!
     #				err)
c
c	This subroutine calculates the geographic or geomagnetic coordinates of radar field points.
c	If center_flag is .false. the coordinates of the vertices of the sampling
c 	cell centered on the selected beam and range are returned in parameter array pos(3,2,2).
c	If center_flag is .true. the coordinates are calculated that correspond to the center of
c	the sampling cell. These are found in
c	pos(1,1,1) = latitude
c       pos(2,1,1) = longitude.
c       pos(3,1,1) = distance from center of the earth (rho)
C
C	THE POSITION IS CALCULATED IN ONE OF 2 WAYS.
C	IF THE VALUE OF 'HEIGHT' IS LARGER THAN 90, IT IS INTERPRETED
C	AS THE HEIGHT IN THE IONOSPHERE FROM WHICH THE BACKSCATTER
C	OCCURS.  IN THAT CASE, THE PROPAGATION IS PRESUMMED TO BE 
C	IN A STRAIGHT LINE AT VELOCITY C TO THE HEIGHT SPECIFIED.
C	IF THE VALUE OF 'HEIGHT' IS LESS THAN 90, IT IS INTERPRETED AT
C	THE TAKE-OFF ANGLE FROM THE RADAR.  THE HEIGHT AT WHICH THE
C	BACKSCATTER OCCURS IS THEN AN UNKNOWN, BUT THE GEOGRAPHIC
C	LOCATION OF THE BACKSCATTER CAN BE DETERMINED.
C
c	When mgflag = 0 the 4 points returned in the array box are in geographical coordinates
c	and when mgflag = 1, they are in magnetic coordinates.
C	When gs_flag= .true. calculate ground scatter positions,
C	assuming ray is in the ionosphere at half the slant range
c	Err is returned with value zero when there is no error and non-zero otherwise.
c
C Revision 2.1 20020313 Dieter Andre
C Time dependent radar position
C Use frang and rsep
C Introduce gs_flag to be able to use rbpos_gen instead of gscat_pos
C
C Revision 2.0 20001211 Dieter Andre
C Took out the common block loc and calculate only the point that is requested.
c
C Revision 1.6 20000128 Dieter Andre
C To calculate position for radars not in the hardware.dat file:
C If st_id <= 0 do not call radar_pos but take the 5 parameters
C gdlat, gdlon, boresite, bmwidth, and rxris from the first 5 elements
C of pos.
c
c       $Log: rbpos_gen.f,v $
c       Revision 1.5  1995/06/20 12:47:03  bristow
c       *** empty log message ***
c
c Revision 1.4  1994/08/09  12:05:32  miker
c added a 'save' to preserve radar parameters values, eliminated the references to 'index'
c (changing array dimensions where needed) and cleaned and general clean-up
c
c Revision 1.3  94/08/08  13:40:06  13:40:06  miker (Michael Ruohoniemi S1G)
c eliminated declarations of unused variables
c 
c Revision 1.1  94/08/03  16:57:29  16:57:29  miker (Michael Ruohoniemi S1G)
c Initial revision
c 
c
c***************************************************************************
	implicit   none
	integer*2  range, bmnum, st_id, mgflag, err
	REAL*4     first_range, range_separation
	real*4     height, pos(3,2,2)
	logical*1  center_flag, gs_flag
	real*4     rho_B, lat_B, long_B
	integer*2  temp_err
	real*4     re/6356.779/
	real*4     maglat,maglong

	external   radar_pos
	integer*2  order/4/
	real*4     gdlat, gdlon, boresite, bmwidth, rxris
	real*4     r, psi
	real*4     position(5)
	real*4     bm_edge, range_edge
	real*4     d, slant_range, temp_height
	integer*2  bm_max, range_max
	integer*2  ib, ir, ibm_pos, ira_pos
c
c	added two variables for use with the new time dependent radar_pos
c
	integer*2  radar_pos_year
	integer*4  radar_pos_sec
	integer*2  year
	integer*4  yr_sec
	save

	err = 0
c
c	call radar_pos to set up the radar position and other things
	radar_pos_year  =  year
	radar_pos_sec   =  yr_sec
	IF (st_id .GT. 0) THEN
	  call radar_pos(st_id, radar_pos_year, radar_pos_sec, position)
	ELSE
	  position(1)= pos(1,1,1)
	  position(2)= pos(2,1,1)
	  position(3)= pos(3,1,1)
	  position(4)= pos(1,2,1)
	  position(5)= pos(2,2,1)
	ENDIF

	gdlat           =  position(1)
	gdlon           =  position(2)
	boresite        =  position(3)
	bmwidth         =  position(4)
	rxris           =  position(5)

c	check the value of iflag to determine whether we are
c	working in the center of a bm and range gate, or
c	using the edges.
c
	  if (center_flag) then
	    bm_max     =  bmnum
	    bm_edge    =   0.
	    range_max  =  range
	    range_edge =   0.0
	  else
	    bm_max     =  bmnum + 1
	    bm_edge    = -bmwidth*0.5
	    range_max  =  range + 1
	    range_edge = -0.5* range_separation
	  end if
c
c	now do the calculations 
c
	do ib = bmnum, bm_max
	  psi = bmwidth* ( real( ib) - 7.5) + bm_edge !phasing matrix angle
	  do ir = range, range_max
	    d= slant_range( first_range, range_separation, rxris* 0.15, range_edge, ir)
	    if (gs_flag) then
	      d= d/ 2.0
	    end if
	    if (height.lt.90.) then  !height is actually elevation angle
	      temp_height = -re + sqrt(re**2 + 2* d* re* sin( 0.017453292* height) + d**2)
	    else
	      temp_height = height
	    end if
	    if (gs_flag) then
      	      call gs_fldpnth( gdlat, gdlon, psi, boresite, temp_height, d, rho_B, lat_B, long_B)
	    else
      	      call fldpnth( gdlat, gdlon, psi, boresite, temp_height, d, rho_B, lat_B, long_B)
	    end if
      	    ibm_pos= ib - bmnum + 1
      	    ira_pos= ir - range + 1
	    if (mgflag.eq.0) then
	        pos(1, ibm_pos, ira_pos) =  lat_B
	        pos(2, ibm_pos, ira_pos) =  long_B
	        pos(3, ibm_pos, ira_pos) =  rho_B
 	    else
	        call cnv$coord( lat_B, long_B, temp_height, order, maglat, maglong, r, mgflag, temp_err)
	        if (temp_err.ne.0) err = temp_err
	        pos(1, ibm_pos, ira_pos) =  maglat
	        pos(2, ibm_pos, ira_pos) =  maglong
	        pos(3, ibm_pos, ira_pos) =  rho_B
	    end if
	  enddo
	enddo
c
c	Fix up table for Schefferville so that beams are wider
c
	if (st_id.eq.2) then
	  print *, 'This version of rbpos does NOT work for Schefferville!'
	  stop
	end if

	return
	end




