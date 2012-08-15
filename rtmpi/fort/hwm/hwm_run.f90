! Main interface for HWM07 model
! 
! Versions:
! 	March 2011: uses HWM07 from Douglas P. Drob
! 
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! input:	yid - year and day of year as YYDDD
! 			ivar = 1   - hourly variation
!                = 2,3 - latitude, longitude
! 				 = 4   - altitude
! 			vbeg - initial value of desired variable
! 			vend - final value of desired variable
! 			vstep - step value of desired variable (no more than 180 steps)
! 			sec - UT (sec) (ignored if ivar = 1)
! 			lati - latitude (ignored if ivar = 2)
! 			longi - longitude (ignored if ivar = 3)
! 			alti - altitude (ignored if ivar = 4)
! 			imod - model version (1 for HWM07 and 2 for HWM93)
! 
! output:	hwmOUT.dat - binary file.
! 					Number of elements is 1(I3) + 2 x 500(F8.3):
! 						* number of elements in each array (max 500)
! 						* Meridional wind (+ North)
! 						* Zonal wind (+ East)
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! Written by S. de Larquier (March 2011)
PROGRAM hwm_run

    implicit none
    integer(4)   :: iyd,ivar,i,id,imn,imod
    integer(4)   :: iyyd,iyyyy,iybeg,jy,jmn,jd, nyd
    integer(4)   :: iiap(9),lm(12),mend,dum
    integer(4)   :: istep,nsteps,sw(25)
    real(4)      :: sec,alt,glat,glon,stl,f107a,f107
    real(4)      :: vbeg,vend,vstep
    real(4)      :: tsec,tlon,tlat,talt
    real(4)      :: ap(2),w(2)
    real(4)      :: wout(2,500)
    real         :: sum, f
    
	DATA LM/31,28,31,30,31,30,31,31,30,31,30,31/
    
    wout = 0.0

    read(5,*),iyyd,imn,id,ivar,sec,alt,glat,glon,vbeg,vend,vstep,imod
!     print*,iyd,ivar,sec,alt,glat,glon,vbeg,vend,vstep
	iyd = iyyd - int(iyyd/100000)*100000
	
! --------------------------------------------------------------------------
! Get F10.7 from file
! --------------------------------------------------------------------------
	iyyyy = int(iyyd/1000)
	Open(13,FILE='/davit/lib/vt/fort/iri/apf107.dat', &
			ACCESS='DIRECT',RECL=55,FORM='FORMATTED',STATUS='OLD')
	READ(13,10,REC=1) JY,JMN,JD,iiap,dum,F
	IYBEG = JY + 1900
	CLOSE(13)

	if(iyyyy.lt.IYBEG) print*,'Date out of range'   ! AP.DAT starts at Jan 1, 1958

	Open(13,FILE='/davit/lib/vt/fort/iri/apf107.dat', &
			ACCESS='DIRECT',RECL=55,FORM='FORMATTED',STATUS='OLD')

	istep = 0
	do i = IYBEG, iyyyy-1
		nyd = 365
		if(i/4*4.eq.i) nyd=366	! leap year
		istep = istep + nyd
	enddo

	lm(2)=28
	if(iyyyy/4*4.eq.iyyyy) lm(2)=29	  ! leap year
	
	do i=1,IMN-1
		istep = istep + LM(i)
	enddo
	
	sum = 0.0
	mend = lm(imn)
	do i = 1, mend 
		istep = istep + 1
		READ(13,10,REC=istep) JY,JMN,JD,iiap,dum,f
		if(f.lt.-4.) print*,'Date out of range'
		if (i.eq.ID) F107 = f
		sum = sum + f
	enddo
	F107a = sum/mend

10      FORMAT(3I3,9I3,I3,F5.1)
	CLOSE(13)
	istep = 0

! --------------------------------------------------------------------------
!   Hourly variations
! --------------------------------------------------------------------------
    if (ivar .eq. 1) then
! 		print '(a5,i8, a8,f5.1, a8,f5.1, a7,f6.1, a7,f6.1, a6,f6.1)', &
! 				'DAY=',iyd, ', LTbeg=',vbeg/3600., ', LTend=',vend/3600., &
! 				', GLAT=',glat, ', GLON=',glon, ', alt=',alt
! 		print '(a6,a12,a10)', 'TIME', 'MER','ZON'
		ap(2) = -1.0
		if (vstep .ne. 0.) then
			if (vbeg .le. vend) then
				nsteps = int((24.*3600. - vbeg + vend)/vstep)
			else
				nsteps = int((vend-vbeg)/vstep)
			endif
		endif
		if (nsteps .gt. 500) then
			nsteps = 500
			vstep = (vend-vbeg)/nsteps
		endif
		tsec = vbeg
		do istep = 1, nsteps, 1
			stl = tsec/3600. + glon/15.
			if (stl .gt. 24.) stl = stl - 24.
			if (imod.eq.1) then
				CALL hwm07(iyd,tsec,alt,glat,glon,stl,f107a,f107,ap,w)
			else
				CALL gws5(iyd,tsec,alt,glat,glon,stl,f107a,f107,ap,w)
			endif
! 			print '(f6.2,f12.3,f10.3)', tsec/3600., w
			if (tsec + vstep .ge. 24.*3600.) then
				tsec = (tsec/3600. - int(tsec/3600.))*3600. - vstep
				iyd = iyd + 1
			endif
			tsec = tsec + vstep
			wout(1,istep) = w(1)
			wout(2,istep) = w(2)
		end do
    endif
    
! --------------------------------------------------------------------------
!   Latitude variations
! --------------------------------------------------------------------------
    if (ivar .eq. 2) then
! 		print '(a5,i8, a5,f5.1, a7,f6.1, 2(a10,f6.1), a6,f6.1)', &
! 				'DAY=',iyd, ', LT=',sec/3600., &
! 				', GLATbeg=',vbeg, ', GLATend=',vend, ', GLON=',glon, ', alt=',alt
! 		print '(a6,a6,a12,a10)', 'LAT', 'TIME', 'MER','ZON'
		ap(2) = -1.0
		if (vstep .ne. 0.) then
			nsteps = int((vend-vbeg)/vstep)
		else
			nsteps = 1
		endif
		if (nsteps .gt. 500) then
			nsteps = 500
			vstep = (vend - vbeg)/500.
		endif
		tlat = vbeg
		stl = sec/3600. + glon/15.
		if (stl .gt. 24.) stl = stl - 24.
		do istep = 1, nsteps+1, 1
			if (imod.eq.1) then
				CALL hwm07(iyd,sec,alt,tlat,glon,stl,f107a,f107,ap,w)
			else
				CALL gws5(iyd,sec,alt,tlat,glon,stl,f107a,f107,ap,w)
			endif
! 			print '(f6.2,f6.2,f12.3,f10.3)', tlat, sec/3600., w
			tlat = tlat + vstep
			wout(1,istep) = w(1)
			wout(2,istep) = w(2)
		end do
    endif
    
! --------------------------------------------------------------------------
!   Longitude variations
! --------------------------------------------------------------------------
    if (ivar .eq. 3) then
! 		print '(a5,i8, a5,f5.1, a7,f6.1, 2(a10,f6.1), a6,f6.1)', &
! 				'DAY=',iyd, ', LT=',sec/3600., &
! 				', GLAT=',glat, ', GLONbeg=',vbeg, ', GLONend=',vend, ', alt=',alt
! 		print '(a6,a6,a12,a10)', 'LON', 'TIME', 'MER','ZON'
		ap(2) = -1.0
		if (vstep .ne. 0.) then
			nsteps = int((vend-vbeg)/vstep)
		else
			nsteps = 1
		endif
		if (nsteps .gt. 500) then
			nsteps = 500
			vstep = (vend - vbeg)/500.
		endif
		tlon = vbeg
		do istep = 1, nsteps+1, 1
			if (sec + vstep*12./180. .ge. 24.*3600) then
				sec = (sec/3600. - int(sec/3600.))*3600. - vstep*12./180.
			endif
			stl = sec/3600. + glon/15.
			if (stl .gt. 24.) stl = stl - 24.
			sec = sec + vstep*12./180.*3600.
			if (imod.eq.1) then
				CALL hwm07(iyd,sec,alt,glat,tlon,stl,f107a,f107,ap,w)
			else
				CALL gws5(iyd,sec,alt,glat,tlon,stl,f107a,f107,ap,w)
			endif
! 			print '(f6.2,f6.2,f12.3,f10.3)', tlon, sec/3600., w
			tlon = tlon + vstep
			wout(1,istep) = w(1)
			wout(2,istep) = w(2)
		end do
    endif
    
! --------------------------------------------------------------------------
!   Altitude variations
! --------------------------------------------------------------------------
    if (ivar .eq. 4) then
! 		print '(a5,i8, a5,f5.1, a7,f6.1, 2(a10,f6.1), a6,f6.1)', &
! 				'DAY=',iyd, ', LT=',sec/3600., &
! 				', GLAT=',glat, ', GLON=',glon, ', alt=',alt
! 		print '(a6,a6,a12,a10)', 'ALT', 'TIME', 'MER','ZON'
		ap(2) = -1.0
		if (vstep .ne. 0.) then
			nsteps = int((vend-vbeg)/vstep)
		else
			nsteps = 1
		endif
		if (nsteps .gt. 500) then
			nsteps = 500
			vstep = (vend - vbeg)/500.
		endif
		talt = vbeg
		stl = sec/3600. + glon/15.
		if (stl .gt. 24.) stl = stl - 24.
		do istep = 1, nsteps+1, 1
			if (imod.eq.1) then
				CALL hwm07(iyd,sec,talt,glat,glon,stl,f107a,f107,ap,w)
			else
				CALL hwm07(iyd,sec,talt,glat,glon,stl,f107a,f107,ap,w)
			endif
! 			print '(f6.2,f6.2,f12.3,f10.3)', talt, sec/3600., w
			talt = talt + vstep
			wout(1,istep) = w(1)
			wout(2,istep) = w(2)
		end do
    endif
    
    OPEN(66,FILE='/tmp/hwmOUT.dat',status='unknown')
    write(66,'(I3)')nsteps
100	FORMAT(500F8.3)
    write(66,100)(wout(1,istep), istep=1,500)
    write(66,100)(wout(2,istep), istep=1,500)
    CLOSE(66)



END