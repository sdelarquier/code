! Compute ionospheric conductivities from IRI+NRLMSIS models
! Conductivity formulas from Intro. to Space Physics, MG Kivelson and CT Russel p.201,
! and from Rishbeth and Garriott, Intro to Ionospheric Physics, p.136
! Collision frequency formulas from Rishbeth and Garriott, Intro to Ionospheric Physics, p.130
!
! Written by S. de Larquier - Jan. 2012
program msise_run

    implicit none
    INTEGER               :: i
!     Constants
    REAL,PARAMETER        :: amu=1.66e-27,qe=1.60e-19,me=9.11e-31
    REAL,PARAMETER        :: Ahe=4.,Ao=16.,An2=28.,Ao2=32.,Aar=40.,An=14., Ah=1. ! molar masses in g/mol
!     Inputs
    INTEGER               :: year,doy,hrut
    REAL                  :: glat,glon
!     Models inputs
    INTEGER               :: yyyy,iyd,dhour,yyddd
    REAL                  :: sec,stl,yeardec
    REAL                  :: f107,f107a,xl,ap(7)
    REAL                  :: hbeg,hend,hstep,alt
!     Models outputs
    REAL                  :: msisDout(9),msisTout(2)
! 	  Misc.
		INTEGER								:: IYBEG,nyd,JY,JMN,JD,IS,ihour,IR,lhour
		INTEGER								:: tiap(8),tiapd,iap(32),iapd(4)
		REAL									:: tF,tF81,tF365,F(4),F81(4),F365(4),M
    REAL(8),DIMENSION(500):: no,nn2,no2,nar,nhe,nn,nh,rho,T,P

! **************************************************************************************
! ************************** Input
! **************************************************************************************
! Read in year, day-of-year, hour UT, geographic latitude and longitude
!     print *,'Provide year(yyyy), day-of-year, hour(UT), latitude and longitude'
    read(*,*)yyyy,doy,hrut,glat,glon

    hbeg = 0.
    hend = 500.
    hstep = 1.


! Reads apf107.dat for f10.7 values
    IYBEG = 1958

		do i=1,8
			iap(i)=-1
		enddo

		Open(13,FILe='/davit/lib/vt/fort/iri/apf107.dat', &
			ACCESS='DIRECT',RECL=55,FORM='FORMATTED',STATUS='OLD')

		is = 0
		if(yyyy.gt.IYBEG) then
				do i=IYBEG,yyyy-1
						nyd = 365
						if (i/4*4.eq.i) nyd = 366
						IS = IS + nyd
				enddo
		endif
		IS = IS + doy

		ihour = int(hrut/3.) + 1
		if(ihour.gt.8) ihour = 8

		if(is*8+ihour.lt.13) then   ! at least 13 indices available
			print*, 'Too few past indices.. what do you need from 1958 anyway?!!!'
		endif
		READ(13,10,REC=IS-0) JY,JMN,JD,tiap,tiapd,IR,tF,tF81,tF365
		iap(25:32) = tiap
		iapd(4) = tiapd
		F(4) = tF
		F81(4) = tF81
		F365(4) = tF365
		READ(13,10,REC=IS-1) JY,JMN,JD,tiap,tiapd,IR,tF,tF81,tF365
		iap(17:24) = tiap
		iapd(3) = tiapd
		F(3) = tF
		F81(3) = tF81
		F365(3) = tF365
		READ(13,10,REC=IS-2) JY,JMN,JD,tiap,tiapd,IR,tF,tF81,tF365
		iap(9:16) = tiap
		iapd(2) = tiapd
		F(2) = tF
		F81(2) = tF81
		F365(2) = tF365
		READ(13,10,REC=IS-3) JY,JMN,JD,tiap,tiapd,IR,tF,tF81,tF365
		iap(1:8) = tiap
		iapd(1) = tiapd
		F(1) = tF
		F81(1) = tF81
		F365(1) = tF365
10  FORMAT(3I3,9I3,I3,3F5.1)

		CLOSE(13)

		ap(1) = iapd(4)
		lhour = 8*3 + ihour
		ap(2) = iap(lhour)
		ap(3) = iap(lhour-1)
		ap(4) = iap(lhour-2)
		ap(5) = iap(lhour-3)
		ap(6) = SUM(iap(lhour-11:lhour-4))/7
		ap(7) = SUM(iap(lhour-19:lhour-12))/7

		f107 = F(3)
		f107a = F81(4)

! **************************************************************************************
! ************************** NRL-MSIS
! **************************************************************************************
! Prepare parameters for call to NRLMSIS
    yyddd = yyyy*1000 + doy
    sec = hrut*3600.
    stl = sec/3600. + glon/15.
! Call NRL-MSIS
    call METERS(.TRUE.)
    alt = hbeg
    OPEN(66,FILE='AtmProfile.dat',status='unknown')
    do i=1,500
			call GTD7(yyddd,sec,alt,glat,glon,stl,f107a,f107,ap,48,msisDout,msisTout)
			nhe(i) = msisDout(1)
			no(i) = msisDout(2)
			nn2(i) = msisDout(3)
			no2(i) = msisDout(4)
			nar(i) = msisDout(5)
			nh(i) = msisDout(7)
			nn(i) = msisDout(8)
			rho(i) = msisDout(6)
			T(i) = msisTout(2)
			M = (Ahe*nhe(i) + Ao*no(i) + An2*nn2(i) + Ao2*no2(i) + Aar*nar(i) + An*nn(i) + Ah*nh(i)) / &
						(nhe(i) + no(i) + nn2(i) + no2(i) + nar(i) + nh(i) + nn(i)) * 1e-3
			P(i) = rho(i)*8.314/M*T(i)

    WRITE(66,400)hbeg+(i-1)*hstep,P(i),rho(i),T(i)
400 FORMAT(F7.2,3E13.5)

			alt = alt + hstep
    enddo
!     print *,'------------NRL-MSIS profile complete!'
    CLOSE(66)





end program msise_run
