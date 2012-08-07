c	INFO.INC
c	********

c*********************************************************************
c 			Meaning of few variables:
c			-------------------------
c
c	idur = 1:  (*)	Map for each scan
c	     = 2:	Average scans (during time 'tperiod')
c
c	ialt = 1:	Constant virtual height
c	     = 2:  (*)	Model for virtual height
c	     = 3:  (*)	Use elevation angle information for calcula-
c			tion of virtual height
c
c	igs = 1:	Include all echoes identified as ground
c			scatter by FITACF
c	    = 2:	Exclude all echoes identified as ground
c			scatter by FITACF
c	    = 3:   (*)	Include echoes if identified as ground
c			scatter by FITACF for one of the radar
c			but not for the other one
c	    = 4:   (*)	Other criteria of elimination (to be defined)
c
c	iereg = 1:	Include E-region echoes
c	      = 2:	Exclude E-region echoes by setting a minimum
c			range for F-region echoes
c	      = 3: (*)	Exclude E-region echoes by using the eleva-
c			tion angle information
c
c	ifltr = 0:	Do not smooth the radial velocity maps
c	      = 1:	Smooth the radial velocity maps
c
c	inter = 0:	Do not interpolate radial velocities to 
c			isolated missing data points
c	      = 1: (*)	Do interpolate
c
c	idiv = 0:	Do not do the divergence-free analysis for
c			reducing the divergence of the map
c	     = 1:	Do the divergence-free analysis for reducing
c			the divergence of the map
c
c	iext = 0:	Do not do the divergence-free analysis for
c			extending the 16x16 points maps
c	     = 1:	Do the divergence-free analysis for extending
c			the 16x16 points maps where velocities cannot
c			be computed from data (one or two components
c			missing
c
c	ires = 0:	Compute the velocities at the 16x16 grid
c			points only
c	     = 1:  (*)	Use the radial velocities from all gates to
c			increase the 16x16 grid points space resolution
c
c (*): Options not ready
c*********************************************************************
	parameter (iiary = 27, irary = 8)
	integer*2 infi(iiary)
	integer*2 syr,smo,sday,shr,smin,ssec
	integer*2 eyr,emo,eday,ehr,emin,esec
	integer*2 st1,st2
	integer*2 f_low(2),f_high(2)
	integer*2 idur,ialt,igs,iereg,ifltr,inter,idiv,iext,ires
	integer*4 ifileptr(2)
	real*4    infr(irary)
	real*4	  tperiod,height,height_r,frng_min,tstep
	real*4	  pwr_min,vel_max,velerr_max
c
	EQUIVALENCE 
c		INTEGER VARIABLES
     &		(st1,		infi(1) ),	!radar1 index
     &		(st2,		infi(2) ),	!radar2 index
     &		(syr,		infi(3) ),	!start time and date
     &		(smo,		infi(4) ),
     &		(sday,		infi(5) ),
     &		(shr,		infi(6) ),
     &		(smin,		infi(7) ),
     &		(ssec,		infi(8) ),
     &		(eyr,		infi(9) ),	!end time and date
     &		(emo,		infi(10)),
     &		(eday,		infi(11)),
     &		(ehr,		infi(12)),
     &		(emin,		infi(13)),
     &		(esec,		infi(14)),
     &		(f_low(1),	infi(15)),	!min freq. for radar1
     &		(f_low(2),	infi(16)),	!min freq. for radar2
     &		(f_high(1),	infi(17)),	!max freq. for radar1
     &		(f_high(2),	infi(18))	!max freq. for radar2
	EQUIVALENCE 
     &		(idur,		infi(19)),	!average maps or not
     &		(ialt,		infi(20)),	!virtual height method
     &		(igs,		infi(21)),	!ground scatter method
     &		(iereg,		infi(22)),	!E_region echoes method
     &		(ifltr,		infi(23)),	!smoothed or not
     &		(inter,		infi(24)),	!interpolation or not
     &		(idiv,		infi(25)),	!div. free relax. or not
     &		(iext,		infi(26)),	!div. free exten. or not
     &		(ires,		infi(27)),	!increase space resol.
     &						!or not
c		REAL VARIABLES
     &		(tperiod,	infr(1) ),	!averaging time (sec.)
     &		(height,	infr(2) ),	!virtual stnd height (km)
     &		(height_r,	infr(3) ),	!real standard height (km)
     &		(frng_min,	infr(4) ),	!min range of F-region
     &		(pwr_min,	infr(5) ),	!min power
     &		(vel_max,	infr(6) ),	!max velocity
     &		(velerr_max,	infr(7) ),	!max error on velocity
     &          (tstep,         infr(8) )       !time step (sec.)
c
	common/info1/infi
	common/info2/infr
	common/files/ifileptr
