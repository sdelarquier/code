! Main interface for IGRF model
! 
! Versions:
! 	March 2011: uses IGRF-11 coefficients
! 
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! input:	ryear - decimal year (year+month/12.0-0.5 or 
!                  	year+day-of-year/365 or ../366 if leap year)
! 			ivar = 1,2 - latitude, longitude
! 				 = 3     - altitude
! 				 = 4     - magnetic equator (not yet implemented)
! 			vbeg - initial value of desired variable
! 			vend - final value of desired variable
! 			vstep - step value of desired variable (no more than 180 steps)
! 			lati - latitude (ignored if ivar = 1)
! 			longi - longitude (ignored if ivar = 2)
! 			alti - altitude (ignored if ivar = 3)
! 
! output:	igrfOUT.dat - binary file
! 					Number of elements is 1(I3) + 3 x 360(F8.3):
! 						* number of elements in each array (max 360)
! 						* magnetic field amplitude (in Gauss)
! 						* magnetic field dip angle
! 						* magnetic field declination angle (+East)
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! Written by S. de Larquier (March 2011)
	PROGRAM igrf_run
		
		implicit none
		integer(4)        :: ivar,nvar,nsteps,xlcode
		real(4)           :: ryear,vbeg,vend,vstep
		real(4)           :: lati,longi,alti
		real(4)           :: babs,dip,dipdec,xl,dipl
		real(4)           :: diparr(360),decarr(360),fabs(360)

		read(5,*),ryear,ivar,vbeg,vend,vstep,lati,longi,alti
! 		print*,ryear,ivar,vbeg,vend,vstep,lati,longi,alti
		
		if (vstep .ne. 0.) then
			nsteps = int((vend-vbeg)/vstep)
		else
			nsteps = 1
		endif
		if (nsteps .gt. 360) then
			nsteps = 360
			vstep = (vend-vbeg)/nsteps
		endif
		do nvar = 1,nsteps
			call igrf_sub(lati,longi,ryear,alti,xl,xlcode,
     &                dip,dipdec,dipl,babs)
			diparr(nvar) = dip
			decarr(nvar) = dipdec
			fabs(nvar) = babs
		!	  Update variable
			select case (ivar)
				case (1)
					lati = vbeg + nvar*vstep
				case (2)
					longi = vbeg + nvar*vstep
				case (3)
					alti = vbeg + nvar*vstep
			end select
		end do

		OPEN(66,FILE='/tmp/igrfOUT.dat',status='unknown')
		write(66,'(I3)')nsteps
		write(66,100)(fabs(nvar), nvar=1,360)
100	FORMAT(360F8.3)
		write(66,100)(diparr(nvar), nvar=1,360)
		write(66,100)(decarr(nvar), nvar=1,360)
		CLOSE(66)
		
		
	END