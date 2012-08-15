	PROGRAM RAYJONES
	
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
 
	CHARACTER*8 MFLD(2), MSPITZ(3)
	CHARACTER*10 NB(2, 300), MODSAV
	CHARACTER*1 IRTYP,TAB
	CHARACTER*4 KOLL
	CHARACTER*10 MODRIN(3)
c KSH 25.1.91 sun compile warning: local variabes IIP, IIP2 not used
c	DIMENSION IIP(8), IIP2(2)
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /CONST / PI, PIT2, PID2, DEGS, RAD, K, C, LOGTEN
	COMMON /FLG   / NTYP, NEWWR, NEWWP, PENET, LINES, IHOP, HPUNCH
	COMMON /RIN   / COLL, FIELD, SPITZ, N2, N2I, PNP(8), GAM, PGP(6),
     &			POLAR, LPOLAR, FACTOR, SPACE
	COMMON /RINC  / MODRIN
	COMMON /RK    / STEP, E1MAX, E1MIN, E2MAX, E2MIN, FACT, 
     &			RSTART, N, MODE
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /YY    / Y(16)/ZZ/Z(4)
	COMMON /ZZC   / MODZ
	CHARACTER*10 MODZ
	COMMON /YYC   / MODY
	CHARACTER*10 MODY
	COMMON R(20), T, STP, DRDT(20)/WW/W0, W(400)
	COMMON /WWC   / ID
	CHARACTER*10 ID(10)
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	CHARACTER*9 NDATE
	COMMON /CTEST / EL, RANGE, ELB, ELS, RX, HH, RR, AZD, RGM, IEND, JX
* RCLOSE stores whether current ray went close to receiver position
* NCLOSE stores the number of rays in this run which are close to rx
* DSTANC is the ground range from tx to ray
* STOPSE is the phase path
* STOGRP is the group path (added by sto, 16/4/02)
* STOAMP is the absorption (in dB)
* STOELL is the elevation angle at the rx
* STOAZI is the azimuthal angle at the rx
c group path all along ray now collected in this common block.  tky
	common /tim/ grppth(500000)
	COMMON /lasse/ haswritten
	COMMON /STOHOM/ RCLOSE,NCLOSE,DSTANC,STOPSE,STOGRP,STOAMP,
     &	STOELL,STOAZI
* gndran - is the ground range of each ray which falls to the ground
*          (or more properly, the receiver height)
* grpran - is the group range ditto
* numran - is the number of such ranges)
* ranelv - is the launch-elevation of these rays
* ranalt - is the reflection altitude of these rays
* tmpelv - is the current ray elevation (passed to printr)
* doaell and doaazi - are the direction-of-arival info. at the
* receiver point, these have NOT been fully implemented.
	COMMON /STOPOW/ GNDRAN(10000), GRPRAN(10000), RANELV(10000),
     & TMPELV, DOAELL(10000), DOAAZI(10000), NUMRAN, RANALT(10000)
* GNDTHT - is the theta coordinate of rays hitting ground
* GNDPHI - is the phi coord
* NUMGND - is the number of rays hitting ground
	COMMON /STOGND/ GNDTHT(400000), GNDPHI(400000), GNDKTH(400000), 
     & GNDKPH(400000), GNDKRA(400000), NUMGND
c stores ray information for writing to idl-optimised file (rays.dat)
	COMMON /STORAY/ RADPOS(500000), THTPOS(500000), PHIPOS(500000), NUMPOS
* stores k-vector information for writing to kvect.dat
	COMMON /STOKVE/ RADVEC(500000), THTVEC(500000), PHIVEC(500000), NUMVEC
* stores upper-hybrid information for writing to uhybrid.dat
	COMMON /STOHYB/ UPPHYB(500000), NUMHYB
	COMMON /STOSPD/ STSEC, NOOFRY, NOTRANS
c add COMMON/PLT to pass RESET to plotting routines.  This enables
c a new graph to be plotted for each frequency (KSH 14.11.90)
        COMMON /PLT   /XL,XR,YB,YT,RESET
* allows perturbation to be turned off for plotting ambient profile
* iswitch=0 no perturbation (sto, 4/3/94)
	COMMON /PERTOF/ISWITCH
* keep electron density so that files do not have to be read for
* every altitude latitude and longitude
* edensFLAG=1 mean that densities have been read 
	COMMON /IRI_BLK/ edensARR(500,500),edensFLAG,edensPOS(2,500),dayNe(24), 
     &		    dip(2,500)
	double precision edensARR,edensPOS,dip,dayNe
	integer edensFLAG
c       Date for iri calculations
	COMMON /RUNDATE/ iyyyy,imd,hour
	integer iyyyy,imd
	real hour
	EQUIVALENCE(RAY, W(1)), (EARTHR, W(2)), (XMTRH, W(3)), 
     &		    (TLAT, W(4)), (TLON, W(5)), (F, W(6)), (FBEG, W(7)), 
     &		    (FEND, W(8)), (FSTEP, W(9)), (AZ1, W(10)), 
     &		    (AZBEG, W(11)), (AZEND, W(12)), (AZSTEP, W(13)), 
     &		    (BETA, W(14)), (ELBEG, W(15)), (ELEND, W(16)), 
     &		    (ELSTEP, W(17)), (ONLY, W(21)), (HOP, W(22)), 
     &		    (MAXSTP, W(23)), (PLAT, W(24)), (PLON, W(25)), 
     &		    (INTYP, W(41)), (MAXERR, W(42)), (ERATIO, W(43)), 
     &		    (STEP1, W(44)), (STPMAX, W(45)), (STPMIN, W(46)), 
     &		    (FACTR, W(47)), (SKIP, W(71)), (RAYSET, W(72)), 
     &		    (PLT, W(81)), (PERT, W(150)),(NEPLT,W(89))
	EQUIVALENCE (RLAT, W(90)),(RLON, W(91)),(DLAT,W(92)),
     &              (DLON,W(93)),(RAYNRX, W(94))
	LOGICAL SPACE, NEWWR, NEWWP, PENET, RCLOSE, FIRTRA
	REAL*8 N2, N2I, LOGTEN, K, MAXSTP, INTYP, MAXERR, MU
	REAL*8 frqinp(6000),elvinp(6000),aziinp(6000)
	COMPLEX*16 PNP, GAM, PGP, POLAR, LPOLAR
	character*80 fname
	TAB = CHAR(9)
C
C     REPLACEMENT DATA STATEMENT
C
	MODX(1) = ' '
	MODX(2) = ' '
	MODY = ' '
	MODZ = ' '
	MODRIN(1) = ' '
	MODRIN(2) = ' '
	MODRIN(3) = ' '
	haswritten=0

* set el. density flag to 0
	edensFLAG=0

* initialise variable indicating first ray-trace (used in outfile.dat)
	FIRTRA=.TRUE.
	
* Initialise variables used in sto signal strength algorithm
	RCLOSE=.FALSE.
	NCLOSE=0
	ELMIN=10000.0		! min and max elevations in sigstr.dat
	ELMAX=-10000.0
	AZMIN=10000.0		! min and max elevations in summary.dat only
	AZMAX=-10000.0
* and in perturbation plot
	ISWITCH=1
* running total for signal strength (in linear units)
	RUNAMP=0.0
* running total for phase path and for phase path squared
	RUNPSE=0.0
	RUNSQR=0.0
* total number of ranges - i.e. rays which hit the ground
	NUMRAN=0
* and ground positions
	NUMGND=0
	
*     open the various input and output files...
 	write(*,'(A,$)') 'Input file: '
	read (*,'(A)') fname
	
	OPEN(15, FILE=fname, STATUS='OLD')
	OPEN(16, FILE='out/outfile.dat', STATUS='UNKNOWN')
	OPEN(55,FILE='out/sigstr.dat',status='unknown')
	OPEN(56,FILE='out/summary.dat',status='unknown')
	OPEN(57,FILE='out/rays.dat', status='unknown', FORM='UNFORMATTED')
	OPEN(58,FILE='out/ranges.dat', status='unknown', FORM='UNFORMATTED')
	OPEN(59,FILE='out/rayinp.dat', status='unknown') ! creates if not exist
	OPEN(60,FILE='out/kvect.dat', status='unknown', FORM='UNFORMATTED')
	OPEN(61,FILE='out/groundpos.dat', status='unknown', FORM='UNFORMATTED')
	OPEN(62,FILE='out/uhybrid.dat', status='unknown', FORM='UNFORMATTED')
	OPEN(63,FILE='out/groundkv.dat', status='unknown', FORM='UNFORMATTED')
			
* work out time taken (why not) why? tky
c	CP = 1.0
! 	CALL DATT(NDATE)
c	CALL SECC(SECS)
	secs=0.0
	STSEC=SECS
C
C  ********* CONSTANTS
C
	PI = 3.1415926536
	PIT2 = 2.*PI
	PID2 = PI/2.
	DEGS = 180./PI
	RAD = PI/180.
	C = 2.997925E5
	K = 2.81785E-15*C**2/PI
	LOGTEN = ALOG(10.0)
C
C  ********* INITIALIZE SOME VARIABLES IN THE W ARRAY
C
	DO NW = 1, 400
	    W(NW) = 0.
	END DO
	PLON = 0.
	PLAT = PID2
	EARTHR = 6370.
	INTYP = 3.
	MAXERR = 1.E-4
	ERATIO = 50.
	STEP1 = 1.
	STPMAX = 10.
	STPMIN = 1.E-4
	FACTR = 0.5
	MAXSTP = 1000.
	HOP = 1.
	w(48)=EARTHR	! defined where grazing rays are processed
C
C  ********* READ W ARRAY AND PRINT NON-ZERO VALUES
C
100	CONTINUE
	CALL READW
* correct catchment area for latitude - valid for small d(long) only
	IF(W(94).EQ.1.0) W(93)=W(93)/COS(W(90))
* ensure that max. plot ray height is at least receiver height
	IF(W(28).LT.W(20)) W(28)=W(20)
C
	IF(SPITZ.EQ.0.)THEN
	    MSPITZ(1) = '        '
	    MSPITZ(2) = '        '
	    MSPITZ(3) = '     '
	ELSE
	    MSPITZ(1) = 'SPITZE V'
	    MSPITZ(2) = 'ERSION O'
	    MSPITZ(3) = 'F THE   '
	END IF
	KOLL = '  NO'
C
C
	IF(COLL.NE.0.)KOLL = 'WITH'
	AZ1 = 0.0
	BETA = 0.0
	F = 0.0
	IF(SKIP.EQ.0.)SKIP = MAXSTP
	IF(SPITZ.NE.0.)THEN
	    IF(W(60).EQ.0.)W(60) = 1.
	    IF(COLL.EQ.0. .AND. W(61).NE.0.)W(61) = 0.
	    IF(COLL.NE.0. .AND. W(61).EQ.0.)W(61) = 1.
	END IF
	RAY = SIGN(1.D0, RAY)
	NTYP = 2. + FIELD*RAY
	IF(NTYP.EQ.1)THEN
	ELSE IF(NTYP.EQ.2)THEN
	    MFLD(1) = 'NO FIELD'
	    MFLD(2) = ' '
	    IRTYP = 'N'
	    GO TO 200
	ELSE IF(NTYP.EQ.3)THEN
	    MFLD(1) = 'ORDINARY'
	    MFLD(2) = '        '
	    IRTYP = 'O'
	    GO TO 200
	ELSE
	    CALL GOTOER
	END IF
	MFLD(1) = 'EXTRAORD'
	MFLD(2) = 'INARY   '
	IRTYP = 'X'
200	CONTINUE
	MODSAV = MODX(2)
	IF(PERT.EQ.0.)THEN
	    MODX(2) = '      '
	    CALL NOPERT
	END IF
	IF(RAYSET.NE.0.)WRITE(1, 99002)ID, MODX(1), (W(NW), NW=101, 107)
     &				       , MODX(2), (W(NW), NW=151, 157), 
     &				       MODY, (W(NW), NW=201, 207), MODZ, 
     &				       (W(NW), NW=251, 257)
99002	FORMAT(10A8, 4(/A8,2X,7E10.3))

	IF(FIRTRA)THEN
	    WRITE(16, 99003)ID, NDATE, MODX, MODY, MODZ, MSPITZ, MODRIN, 
     &			    MFLD, KOLL
     	FIRTRA=.FALSE.
	ELSE
	    WRITE(16, 99014)CHAR(12),ID, NDATE, MODX, MODY, MODZ, MSPITZ, MODRIN, 
     &			    MFLD, KOLL
	ENDIF	
     
99003	    FORMAT(1X, 10A8, 25X, A9/4(1X,A8)/2X, 2A8, A5, 1X, 2A10, 
     &		   A4, 1X, A8, A5, 1X, A4, ' COLLISIONS '/)
	    WRITE(16, 99004)
99004	    FORMAT(
     &	       ' INITIAL VALUES FOR THE W ARRAY -- ALL ANGLES IN RADIAN'
     &	       , 'S, ONLY NONZERO VALUES PRINTED'/)
	    CALL NOTE(NB)
	    DO NW = 1, 400
		IF(W(NW).NE.0.)WRITE(16, 99005)NW, W(NW), 
     &				     (NB(IA,NW), IA=1, 2)
99005		FORMAT(I4, F19.11, 6X, 2A10)
	    END DO
C
C  ********* LET SUBROUTINES PRINTR AND RAYPLT KNOW THERE IS A NEW W ARRAY
C
	NEWWP = .TRUE.
	NEWWR = .TRUE.
C
C  ********* INITIALIZE PARAMETERS FOR INTEGRATION SUBROUTINE RKAM
C
	N = 6
	DO NR = 7, 20
	    IF(W(50+NR).NE.0.)N = N + 1
	END DO
	MODE = INTYP
	STEP = STEP1
	E1MAX = MAXERR
	E1MIN = MAXERR/ERATIO
	E2MAX = STPMAX
	E2MIN = STPMIN
	FACT = FACTR
C
C  ********* DETERMINE TRANSMITTER LOCATION IN COMPUTATIONAL COORDINATE
C  ********* SYSTEM (GEOMAGNETIC COORDINATES IF DIPOLE FIELD IS USED)
C
	IF(w(305).NE.1)THEN
		R0 = EARTHR + XMTRH
		SP = SIN(PLAT)
		CP = SIN(PID2-PLAT)
		SDPH = SIN(TLON-PLON)
		CDPH = SIN(PID2-(TLON-PLON))
		SL = SIN(TLAT)
		CL = SIN(PID2-TLAT)
		ALPHA = ATAN2(-SDPH*CP, -CDPH*CP*SL+SP*CL)
		TH0 = ACOS(CDPH*CP*CL+SP*SL)
		PH0 = ATAN2(SDPH*CL, CDPH*SP*CL-CP*SL)
		NOTRANS=1
	ELSE
		READ(61)NUMGND
		READ(61)(GNDTHT(ii), ii=1,NUMGND)
		READ(61)(GNDPHI(ii), ii=1,NUMGND)
		R0=EARTHR
		NOTRANS=NUMGND
		IF(w(306).EQ.1 .OR. w(306).EQ.2)THEN
			READ(63)NUMGKV
			IF(NUMGKV .NE. NUMGND)STOP
     &			'ERROR - Number of k-vectors at ground not equal to positions'
			READ(63)(GNDKTH(ii), ii=1,NUMGND)
			READ(63)(GNDKPH(ii), ii=1,NUMGND)
			READ(63)(GNDKRA(ii), ii=1,NUMGND)
		ENDIF
	ENDIF

* write header to intermediate file for IDL ray-plotting...
	WRITE(57)FBEG,FEND,FTEP,AZBEG,AZEND,AZSTEP,ELBEG,ELEND,ELSTEP
c	write(57,'(9e14.7)')FBEG,FEND,FTEP,AZBEG,AZEND,AZSTEP,ELBEG,ELEND,ELSTEP
* and k-vectors for orthogonality studies
	WRITE(60)FBEG,FEND,FTEP,AZBEG,AZEND,AZSTEP,ELBEG,ELEND,ELSTEP
* and upper-hybrid information
	WRITE(62)FBEG,FEND,FTEP,AZBEG,AZEND,AZSTEP,ELBEG,ELEND,ELSTEP

	NUMPOS=0
	NUMVEC=0
	NUMHYB=0
	
	NFREQ = 1
	IF(FSTEP.NE.0.)NFREQ = (FEND-FBEG)/FSTEP + 1.5
	NAZ = 1
	IF(AZSTEP.NE.0.)NAZ = (AZEND-AZBEG)/AZSTEP + 1.5
	NBETA = 1
	IF(ELSTEP.NE.0.)NBETA = (ELEND-ELBEG)/ELSTEP + 1.5
	noofry=nfreq*naz*nbeta
* output to summary file
	CALL SUMHEAD(fname, fbeg, nfreq, fstep, azbeg, naz, azstep, elbeg, 
     &	nbeta, elstep, degs)

* mod for reading in arbitrary freq, azimuth and elevation (sto,27/8/96)
	IF(azbeg.EQ.0.0 .AND. azend.EQ.0.0 .AND. azstep.EQ.0.0)THEN
		CALL RRAYIP(numinp,frqinp,elvinp,aziinp)
		nfreq=numinp
		naz=1
		nbeta=1
	ENDIF

* warn if array bounds in groundpos may not be OK...
	itempno=nfreq*naz*nbeta*hop
	IF(itempno .GT. 400000)THEN
		WRITE(*,*)'WARNING - May be too many rays to collect all information'
		WRITE(*,*)'          required for groundpos file'
	ENDIF

* also loop in trasnmitters (this is only relevant if w(305) has been
* set to 1
	DO NTX=1,NOTRANS
		IF(w(305).EQ.1.0)THEN
			R0=EARTHR
			TH0=GNDTHT(NTX)
			PH0=GNDPHI(NTX)
			CALL C2GEOG(PID2-TH0,PH0,TLAT,TLON) ! reset tx position
			TLAT=PID2-TLAT
			NEWWP=.TRUE.
			SP = SIN(PLAT)			! need to calculate alpha for
			CP = SIN(PID2-PLAT)		! new tx position
			SDPH = SIN(TLON-PLON)
			CDPH = SIN(PID2-(TLON-PLON))
			SL = SIN(TLAT)
			CL = SIN(PID2-TLAT)
			ALPHA = ATAN2(-SDPH*CP, -CDPH*CP*SL+SP*CL)
			write(*,'(a,I6)')'Location ',NTX
		ENDIF	
c loop in frequency, azimuth and elevation
		DO NF = 1, NFREQ
			F = FBEG + (NF-1)*FSTEP
			IF(azbeg.EQ.0.0 .AND. azend.EQ.0.0 .AND. azstep.EQ.0.0)
     &		F=frqinp(nf)
			DO J = 1, NAZ
				RESET=1.0
				AZ1 = AZBEG + (J-1)*AZSTEP
				IF(azbeg.EQ.0.0 .AND. azend.EQ.0.0 .AND. azstep.EQ.0.0)THEN
					AZ1=aziinp(nf)*RAD
					RESET=0.0
				ENDIF
				AZA = AZ1*DEGS
				GAMMA = PI - AZ1 + ALPHA
				SGAMMA = SIN(GAMMA)
				CGAMMA = SIN(PID2-GAMMA)
				DO I = 1, NBETA
					BETA = ELBEG + (I-1)*ELSTEP
					IF(azbeg.EQ.0.0 .AND. azend.EQ.0.0 .AND. azstep.EQ.0.0)
     &			BETA=elvinp(nf)*RAD
					EL = BETA*DEGS
					OLDEL=EL
					TMPELV=EL	! used in ray-counting
					CBETA = SIN(PID2-BETA)
					R(1) = R0
					R(2) = TH0
					R(3) = PH0
					R(4) = SIN(BETA)
					R(5) = CBETA*CGAMMA
					R(6) = CBETA*SGAMMA
* find angle between incoming k-vector and outgoing ray and hence the
* relative power (in dB) carried by the ray
					CALL SCATRAY(r(4),r(5),r(6),gndkth(ntx),gndkph(ntx),
     &			ABS(gndkra(ntx)),w(306),relpow)
					T = 0.
					RSTART = 1.
* ALLOW IONOSPHERIC MODEL SUBROUTINES TO READ AND PRINT DATA
* lines limit should be set to 17 for 66 line per page printout...
* the npage limit I think sets the number of traces per page
					CALL RINDEX
					IF(W(300).EQ.0.0)THEN
						IF(I.EQ.1 .OR. NPAGE.GE.4 .OR. LINES.GT.19)THEN
							LINES = 0
							NPAGE = 0
							WRITE(16, 99014)CHAR(12),ID, NDATE, MODX, MODY, MODZ, 
     &					MSPITZ, MODRIN, MFLD, KOLL
99014	    FORMAT(A1, 10A8, 25X, A9/4(1X,A8)/2X, 2A8, A5, 1X, 2A10, 
     &		   A4, 1X, A8, A5, 1X, A4, ' COLLISIONS '/)
							WRITE(16, 99006)F, AZA
99006			FORMAT(1X, 'FREQUENCY=', F12.6, 
     &			       ' MHZ, AZIMUTH ANGLE OF TRANSM', 
     &			       'ISSION =', F12.6, ' DEG')
						END IF
					NPAGE = NPAGE + 1
					WRITE(16, 99008)EL
99008		    FORMAT(/31X, 'ELEVATION ANGLE OF TRANSMISSION =', 
     &			   F12.6, ' DEG'/)
					ENDIF
					IF(N2.GT.0.)THEN
						MU = SQRT(N2/(R(4)**2+R(5)**2+R(6)**2))
						DO NN = 4, 6
							R(NN) = R(NN)*MU
						END DO
						DO NN = 7, N
							R(NN) = 0.
						END DO
c			CALL SECC(SECS)
						secs=0.0
						OSEC = SECS
						CALL TRACE
* write out results if ray hits ground near to rx
						IF(RAYNRX.EQ.1.0 .AND. RCLOSE)THEN
							NCLOSE=NCLOSE+1
							WRITE(55,'(I5,3F9.4,6F9.3)')NCLOSE,F,AZA,OLDEL,
     &				DSTANC,relpow-STOAMP,STOELL,STOAZI,STOGRP,STOPSE
							RUNAMP=RUNAMP+10.0**((30.0-STOAMP+relpow)/10.0)	! running total of rel. sig. str.
							RUNPSE=RUNPSE+STOPSE
							RUNSQR=RUNSQR+STOPSE*STOPSE
							IF(OLDEL.LT.ELMIN) ELMIN = OLDEL
							IF(OLDEL.GT.ELMAX) ELMAX = OLDEL
							IF(AZA.LT.AZMIN) AZMIN = AZA
							IF(AZA.GT.AZMAX) AZMAX = AZA
							RCLOSE=.FALSE.
						ENDIF
			
						IF(W(300).EQ.0.0)THEN
							secs=0.0
						ENDIF
* write out ray data for IDL-optimised files (rays.dat, kvect.dat)
						CALL WRITEUNF
						IF(PENET .AND. ONLY.NE.0. .AND. IHOP.EQ.1)
     &			   GO TO 220
						ELSE
							CALL ELECTX
							FN = SIGN(SQRT(ABS(X))*F, X)
							WRITE(16, 99010)FN
99010			FORMAT(
     &	       '0TRANSMITTER IN EVANESCENT REGION, TRANSMISSION IMPOSSI'
     &	       , 'BLE'/'0PLASMA FREQUENCY = ', E17.10)
							GO TO 220
					END IF
				END DO
C ends elevation loop
220		CONTINUE
				IF(PLT.NE.0.)CALL ENDPLT
			END DO
C ends azimuth loop
			IF(PENET .AND. ONLY.NE.0. .AND. IHOP.EQ.1 .AND. 
     &	       NAZ.EQ.1 .AND. NBETA.EQ.1)GO TO 300

C Set RESET to 1.0 to call new paper for new frequency
c KSH 14.11.90
c            RESET=1.0

		END DO
C ends frequency loop
	END DO
C ends transmitter loop

C  55 IF (RAYSET.NE.0.) WRITE(1,5000)
99013	FORMAT(78X, '-')
300	CONTINUE
	MODX(2) = MODSAV
	IF(W(94).EQ.1.0)THEN
        IF(NCLOSE.NE.0)THEN
           AVGPHS=RUNPSE/REAL(NCLOSE)
           ERRPSE=SQRT((RUNSQR/REAL(NCLOSE)-AVGPHS*AVGPHS)
     &                 /REAL(NCLOSE))
        ELSE
           AVGPHS=0.0
           ERRPSE=0.0
        ENDIF
* write to summary.dat
		RUNAMP=10.0*LOG10(RUNAMP)
	    WRITE(56,'(A,I8))')' Number of rays =',NCLOSE
	    WRITE(56,'(2(A,F10.4))')' Min Elevation =',ELMIN,
     &            ' Max Elevation =',ELMAX
	    WRITE(56,'(2(A,F10.4))')' Min Azimuth   =',AZMIN,
     &            ' Max Azimuth   =',AZMAX
		WRITE(56,'(A,F8.3)')' Total amplitude : ',RUNAMP
        WRITE(56,'(A,F8.3)')' Avge phase path : ',AVGPHS
        WRITE(56,'(A,F8.3)')' Error in phase  : ',ERRPSE
		WRITE(56,'(A)')'----------------------------------------------------------'
		WRITE(56,*)
	ENDIF
* write to ranges.dat
     	WRITE(58)NUMRAN
     	WRITE(58)(GNDRAN(KK), KK=1,NUMRAN),(GRPRAN(KK), KK=1,NUMRAN),
     &    (RANELV(KK), KK=1,NUMRAN),(RANALT(KK), KK=1,NUMRAN)

	CALL GROUNDOUT
			
	GO TO 100
	END
	
! 	**********************************************************************
! 	**********************************************************************
	SUBROUTINE AGWAVE(PERIOD, SPEED)
C
C  THIS SUBROUTINE IS CALLED BY MODTID, INITIALIZES A FEW PARAMETERS,
C  AND CALLS SUBROUTINE TRAVRS TO SOLVE THE NAVIER-STOKES EQUATIONS BY
C  USING THE MULTILAYER METHOD TO STEP THROUGH THE ATMOSPHERE.
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /INPT  / KX, OMEGA, INTGR(16), NLAYER
	COMMON /MATRIX/ AY(8, 6), A(6, 6), NWAVE
	COMMON /OUTPT / K(6), AP(6), ARB(16), ARBC(16)
	COMMON /TRAV  / TOPATM, BOTATM, TOLMAX, TOLMIN, CURAMP(6, 2), 
     &			OLDAMP(6), ZBEG, ZEND, DEPTHO, Z, NEQNS
	COMMON /WAVE  / U(250), W(250)
	COMPLEX*16 KX, U, W, CURAMP, OLDAMP, AY, A, K, AP, ARBC
	EQUIVALENCE(INDEX, INTGR(1)), (NTEST, INTGR(3)), 
     &		    (IEXTR, INTGR(6)), (KCHECK, INTGR(8)), 
     &		    (KEY, INTGR(10)), (MPRINT, INTGR(4)), (NN, INTGR(2))
	TOPATM = 500.E5
	BOTATM = 0.
	TOLMAX = .05
	INDEX = 1
	NN = 251
	IEXTR = 3
	DEPTHO = -2.E5
	OMEGA = 6.2831853/(PERIOD*60.)
	KX = OMEGA/(SPEED*100.)
	NEQNS = 2
	NLAYER = 1000
	NTEST = 31
	MPRINT = 1
	KCHECK = 0
	NWAVE = 6
	IF(INDEX.EQ.0)NWAVE = 2
	KEY = 0
	TOLMIN = .001
	CALL SETUP(TOPATM)
	OLDAMP(1) = AY(1, 1)
	OLDAMP(2) = AY(2, 1)
	CALL TRAVRS
	KCHECK = 1
	CALL SETUP(BOTATM)
	RETURN
	END


! 	*******************************************************************
! 	*******************************************************************
	SUBROUTINE AHNFNC
C
C  CALCULATES THE REFRACTIVE INDEX AND ITS GRADIENT USING THE
C  APPLETON-HARTREE FORMULA -- NO FIELD, NO COLLISIONS
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /RIN   / COLL, FIELD, SPITZ, N2, N2I, PNPR, PNPRI, PNPTH, 
     &			PNPTHI, PNPPH, PNPPHI, PNPVR, PNPVRI, PNPVTH, 
     &			PNPVTI, PNPVPH, PNPVPI, NNP, NNPI, PNPT, PNPTI, 
     &			GAM, PGP(6), POLAR, LPOLAR, FACTR, SPACE
	COMMON /RINC  / MODRIN
	CHARACTER*10 MODRIN(3)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 MODX(2)
	LOGICAL SPACE
	REAL*8 N2, N2I, NNP, NNPI
	COMPLEX*16 GAM, PGP, POLAR, LPOLAR
C     DATA FIELD/0./,COLL/0./,SPITZ/0./,POLAR/(0.,1.)/,LPOLAR/(0., 0.)/,
C    + GAM/(0.,0.)/,                                              NNP/1.
C    +/,PNPX/-0.5/,PNPVR/0./,PNPVTH/0./,PNPVPH/0./
C     DATA MODRIN/'APPLETON-H','ARTREE FOR','MULA'/
C
C     REPLACING ILLEGAL DATA STATEMENT ABOVE
C
	IF(ICODE.EQ.0)THEN
	    MODRIN(1) = 'APPLETON-H'
	    MODRIN(2) = 'ARTREE FOR'
	    MODRIN(3) = 'MULA      '
	    FIELD = 0.0
	    COLL = 0.0
	    SPITZ = 0.0
	    POLAR = (0.0, 1.0)
	    LPOLAR = (0.0, 0.0)
	    GAM = (0.0, 0.0)
	    NNP = 1.0
	    PNPX = -0.5
	    PNPVR = 0.0
	    PNPVTH = 0.0
	    PNPVPH = 0.0
	    RETURN
	END IF
C
C
C
	PXPT = 0.0
	PXPPH = 0.0
	PXPPH = 0.0
	PXPTH = 0.0
	PXPR = 0.0
	X = 0.0
	CALL ELECTX
	PNPR = PNPX*PXPR
	PNPTH = PNPX*PXPTH
	PNPPH = PNPX*PXPPH
	PNPT = PNPX*PXPT
	N2 = 1. - X
	SPACE = N2.EQ.1.
	RETURN
	END


! 	*******************************************************************
! 	*******************************************************************
**==AHNFWC.FOR
! 	*******************************************************************
! 	*******************************************************************
	SUBROUTINE AHNFWC
C
C  CALCULATES THE REFRACTIVE INDEX AND ITS GRADIENT USING THE
C  APPLETON-HARTREE FORMULA -- NO FIELD, WITH COLLISIONS
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /RIN   / COLL, FIELD, SPITZ, N2, PNPR, PNPTH, PNPPH, 
     &			PNPVR, PNPVTH, PNPVPH, NNP, PNPT, GAM, PGP(6), 
     &			POLAR, LPOLAR, FACTR, SPACE
	COMMON /RINC  / MODRIN
	CHARACTER*10 MODRIN(3)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /ZZ    / Z, PZPR, PZPTH, PZPPH
	COMMON /ZZC   / MODZ
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
C
	CHARACTER*10 MODZ
	LOGICAL SPACE
	COMPLEX*16 N2, PNPR, PNPTH, PNPPH, PNPVR, PNPVTH, PNPVPH, NNP, 
     &		   PNPT, GAM, PGP, POLAR, LPOLAR, U, I, PNPX, PNPZ
C     DATA COLL/1./,FIELD/0./,SPITZ/0./,POLAR/(0.,1.)/,LPOLAR/(0., 0.)/,
C    + GAM/(0.,0.)/,                                              I/(0.,
C    +1.)/,ABSLIM/1.E-5/,PNPVR/0./,PNPVTH/0./,PNPVPH/0./,    Z/0./,PZPR/
C    +0./,PZPTH/0./,PZPPH/0./
C     DATA MODRIN/'APPLETON-H','ARTREE FOR','MULA'/
C
C     REPLACING ILLEGAL DATA STATEMENT ABOVE
C
	IF(ICODE.EQ.0)THEN
	    MODRIN(1) = 'APPLETON-H'
	    MODRIN(2) = 'ARTREE FOR'
	    MODRIN(3) = 'MULA      '
	    COLL = 1.0
	    FIELD = 0.0
	    SPITZ = 0.0
	    POLAR = (0.0, 1.0)
	    LPOLAR = (0.0, 0.0)
	    GAM = (0.0, 0.0)
	    I = (0.0, 1.0)
	    ASBLIM = 1.E-5
	    PNPVR = 0.0
	    PNPVTH = 0.0
	    PNPVPH = 0.0
	    Z = 0.0
	    PZPR = 0.0
	    PZPTH = 0.0
	    PZPPH = 0.0
	    RETURN
	END IF
C
C
C
	PXPT = 0.0
	PXPPH = 0.0
	PXPTH = 0.0
	PXPR = 0.0
	X = 0.0
	CALL ELECTX
	CALL COLFRZ
	U = 1. - I*Z
	N2 = 1. - X/U
	PNPX = -1./(2.*U)
	PNPZ = -I*X/(2.*U**2)
	NNP = N2 - (2.*X*PNPX+Z*PNPZ)
	PNPR = PNPX*PXPR + PNPZ*PZPR
	PNPTH = PNPX*PXPTH + PNPZ*PZPTH
	PNPPH = PNPX*PXPPH + PNPZ*PZPPH
	PNPT = PNPX*PXPT
	SPACE = DREAL(N2).EQ.1. .AND. ABS(DIMAG(N2)).LT.ABSLIM
	RETURN
	END
**==AHSPNC.FOR
	SUBROUTINE AHSPNC
C
C  CALCULATES THE REFRACTIVE INDEX AND ITS GRADIENT USING THE
C  APPLETON-HARTREE FORMULA WITH FIELD, NO COLLISIONS
C  INCLUDING FORMULAS FOR A SPITZE
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	DIMENSION VXY(3)
	COMMON /RK    / STEP, E1MAX, E1MIN, E2MAX, E2MIN, FACT, 
     &			RSTART, N, MODE
	COMMON /RIN   / COLL, FIELD, SPITZ, N2, N2I, PNPR, PNPRI, PNPTH, 
     &			PNPTHI, PNPPH, PNPPHI, PNPVR, PNPVRI, PNPVTH, 
     &			PNPVTI, PNPVPH, PNPVPI, NNP, NNPI, PNPT, PNPTI, 
     &			GAM, GAMI, PGPR, PGPRI, PGPT, PGPTI, PGPP, 
     &			PGPPI, PGPVR, PGPVRI, PGPVT, PGPVTI, PGPVP, 
     &			PGPVPI, POLAR, POLARI, LPOLAR, LPOLRI, YT2, 
     &			SPACE
	COMMON /RINC  / MODRIN
	CHARACTER*10 MODRIN(3)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /YY    / Y, PYPR, PYPTH, PYPPH, YR, PYRPR, PYRPT, PYRPP, 
     &			YTH, PYTPR, PYTPT, PYTPP, YPH, PYPPR, PYPPT, 
     &			PYPPP
	COMMON /YYC   / MODY
	CHARACTER*10 MODY
	COMMON R(20), T/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(VR, R(4)), (VT, R(5)), (VP, R(6))
	EQUIVALENCE(RAY, W(1))
	LOGICAL SPACE
	REAL*8 N2, N2I, NNP, NNPI, LPOLAR, LPOLRI, NPNPX, NPNPG
	
	IF(ICODE.EQ.0)THEN
	    MODRIN(1) = 'APPLETON-H'
	    MODRIN(2) = 'ARTREE FOR'
	    MODRIN(3) = 'MULA      '
	    COLL = 0.0
	    FIELD = 1.0
	    SPITZ = 1.0
	    POLAR = 0.0
	    U = 1.0
	    Y = 0.0
	    PYPR = 0.0
	    PYPTH = 0.0
	    PYPPH = 0.0
	    YR = 0.0
	    PYRPR = 0.0
	    PYRPT = 0.0
	    YRPP = 0.0
	    YTH = 0.0
	    PYTPR = 0.0
	    PYTPT = 0.0
	    PYTPP = 0.0
	    YPH = 0.0
	    PYPPR = 0.0
	    PYPPT = 0.0
	    PYPPP = 0.0
	    PGPRI = 0.0
	    PGPTI = 0.0
	    PGPPI = 0.0
	    PGPVRI = 0.0
	    PGPVTI = 0.0
	    PGPVPI = 0.0
	    RETURN
	END IF
C
C
C
	PXPT = 0.0
	PXPPH = 0.0
	PXPTH = 0.0
	PXPR = 0.0
	X = 0.0
	CALL ELECTX
	CALL MAGY
	V2 = VR**2 + VT**2 + VP**2
	VDOTY = VR*YR + VT*YTH + VP*YPH
	YL2 = VDOTY**2/V2
	VXY(1) = VT*YPH - VP*YTH
	VXY(2) = VP*YR - VR*YPH
	VXY(3) = VR*YTH - VT*YR
	YT2 = (VXY(1)**2+VXY(2)**2+VXY(3)**2)/V2
	PVXYPR = 2.*(VXY(1)*(VT*PYPPR-VP*PYTPR)+VXY(2)
     &		 *(VP*PYRPR-VR*PYPPR)+VXY(3)*(VR*PYTPR-VT*PYRPR))
	PVXYPT = 2.*(VXY(1)*(VT*PYPPT-VP*PYTPT)+VXY(2)
     &		 *(VP*PYRPT-VR*PYPPT)+VXY(3)*(VR*PYTPT-VT*PYRPT))
	PVXYPP = 2.*(VXY(1)*(VT*PYPPP-VP*PYTPP)+VXY(2)
     &		 *(VP*PYRPP-VR*PYPPP)+VXY(3)*(VR*PYTPP-VT*PYRPP))
	PVXYP1 = 2.*(YTH*VXY(3)-YPH*VXY(2))
	PVXYP2 = 2.*(YPH*VXY(1)-YR*VXY(3))
	PVXYP3 = 2.*(YR*VXY(2)-YTH*VXY(1))
	YPYPR = YT2*VDOTY/V2*(VR*PYRPR+VT*PYTPR+VP*PYPPR)
	YPYPT = YT2*VDOTY/V2*(VR*PYRPT+VT*PYTPT+VP*PYPPT)
	YPYPP = YT2*VDOTY/V2*(VR*PYRPP+VT*PYTPP+VP*PYPPP)
	YPYPVR = YT2*(VDOTY*YR-YL2*VR)/V2
	YPYPVT = YT2*(VDOTY*YTH-YL2*VT)/V2
	YPYPVP = YT2*(VDOTY*YPH-YL2*VP)/V2
	UX = U - X
	TEST = 4.*YL2*UX**2
	IF(TEST.GE.1.E-5*YT2**2)THEN
	    GAM = (-YT2+RAY*SQRT(YT2**2+TEST))/(2.*UX)
	ELSE
*	    WRITE(16, 99001)
99001	    FORMAT(' TEST.LT.1.E-5*YT2**2')
	    GAM = (RAY-1.)*YT2/(2.*UX)
     &		   + RAY*UX*YL2/YT2*(1.-YL2*UX**2/YT2**2)
	END IF
	IF(RSTART.NE.0.)THEN
C
C  ********* FIND EQUATION NUMBER FOR INTEGRATING GAMMA
C
	    NG = 6
	    DO NR = 7, 9
		IF(W(NR+50).NE.0.)NG = NG + 1
	    END DO
C
C  ********* INITIALIZE GAMMA AT START OF INTEGRATION
C
	    R(NG+1) = GAM
	END IF
C
C  *********
C
	GAMMA = R(NG+1)
	YPGAM = YL2 + GAMMA**2
	YMGAM = YL2 - GAMMA**2
C
C  ********* LONGITUDINAL PROPAGATION
C
	IF(ABS(YMGAM).LT.1.E-3*YL2)YMGAM = YT2*GAMMA/UX
C
C  *********
C
	IF(YL2.LT.1.E-3*Y**2)THEN
C
C  ********* TRANSVERSE PROPAGATION
C
	    YPGAM = GAMMA*(2.*UX*GAMMA+YT2)/UX
	    YMGAM = YT2*GAMMA/UX
	END IF
C
C  *********
C
	YPGPR = (-PXPR*YMGAM**2-YMGAM*GAMMA*PVXYPR/V2+2.*GAMMA*YPYPR)
     &		/YPGAM
	YPGPT = (-PXPTH*YMGAM**2-YMGAM*GAMMA*PVXYPT/V2+2.*GAMMA*YPYPT)
     &		/YPGAM
	YPGPP = (-PXPPH*YMGAM**2-YMGAM*GAMMA*PVXYPP/V2+2.*GAMMA*YPYPP)
     &		/YPGAM
	YPGPVR = GAMMA*(YMGAM*(2.*YT2*VR-PVXYP1)/V2+2.*YPYPVR)/YPGAM
	YPGPVT = GAMMA*(YMGAM*(2.*YT2*VT-PVXYP2)/V2+2.*YPYPVT)/YPGAM
	YPGPVP = GAMMA*(YMGAM*(2.*YT2*VP-PVXYP3)/V2+2.*YPYPVP)/YPGAM
	PGPR = YPGPR/YT2
	PGPT = YPGPT/YT2
	PGPP = YPGPP/YT2
	PGPVR = YPGPVR/YT2
	PGPVT = YPGPVT/YT2
	PGPVP = YPGPVP/YT2
	GAMU = U + GAMMA
	N2 = 1. - X/GAMU
	NPNPX = -.5/GAMU
	NPNPG = .5*X/GAMU**2
	PNPR = YT2*NPNPX*PXPR + NPNPG*YPGPR
	PNPTH = YT2*NPNPX*PXPTH + NPNPG*YPGPT
	PNPPH = YT2*NPNPX*PXPPH + NPNPG*YPGPP
	PNPVR = NPNPG*YPGPVR
	PNPVTH = NPNPG*YPGPVT
	PNPVPH = NPNPG*YPGPVP
	NNP = YT2 + NPNPG*(-2.*YT2*GAMMA**3+YMGAM**2*2.*X)/YPGAM
	PNPT = (YT2*NPNPX-YMGAM**2/YPGAM)*PXPT
	SPACE = N2.EQ.1.
	POLARI = -GAMMA*SQRT(V2)/VDOTY
* bug fix (sto, 1/9/93)
	IF(YT2.GE.0.0)THEN
	    LPOLRI = X*SQRT(YT2)/(UX*GAMU)
        ELSE
	    LPOLRI = 0.0
	ENDIF

	RETURN
	END
**==AHSPWC.FOR
	SUBROUTINE AHSPWC
C
C  CALCULATES THE REFRACTIVE INDEX AND ITS GRADIENT USING THE
C  APPLETON-HARTREE FORMULA WITH FIELD, WITH COLLISIONS
C  INCLUDING FORMULAS FOR A SPITZE
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	DIMENSION VXY(3)
	COMMON /RK    / STEP, E1MAX, E1MIN, E2MAX, E2MIN, FACT, 
     &			RSTART, N, MODE
	COMMON /RIN   / COLL, FIELD, SPITZ, N2, PNPR, PNPTH, PNPPH, 
     &			PNPVR, PNPVTH, PNPVPH, NNP, PNPT, GAM, PGPR, 
     &			PGPT, PGPP, PGPVR, PGPVT, PGPVP, POLAR, LPOLAR, 
     &			YT2, SPACE
	COMMON /RINC  / MODRIN
	CHARACTER*10 MODRIN(3)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /YY    / Y, PYPR, PYPTH, PYPPH, YR, PYRPR, PYRPT, PYRPP, 
     &			YTH, PYTPR, PYTPT, PYTPP, YPH, PYPPR, PYPPT, 
     &			PYPPP
	COMMON /YYC   / MODY
	CHARACTER*10 MODY
	COMMON /ZZ    / Z, PZPR, PZPTH, PZPPH
	COMMON /ZZC   / MODZ
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 MODZ
	COMMON R(20), T/WW/W0, W(400)
	COMMON /WWC   / ID
	CHARACTER*10 ID(10)
	EQUIVALENCE(VR, R(4)), (VT, R(5)), (VP, R(6))
	EQUIVALENCE(RAY, W(1))
	LOGICAL SPACE
	COMPLEX*16 N2, PNPR, PNPTH, PNPPH, PNPVR, PNPVTH, PNPVPH, NNP, 
     &		   PNPT, GAM, PGPR, PGPT, PGPP, PGPVR, PGPVT, PGPVP, 
     &		   POLAR, LPOLAR, I, U, UX, GAMMA, PUPR, PUPT, PUPP, 
     &		   YPGPR, YPGPT, YPGPP, YPGPVR, YPGPVT, YPGPVP, NPNPX, 
     &		   NPNPU, NPNPG, YPGAM, YMGAM, GAMU, TEST
C     DATA COLL/1./,FIELD/1./,SPITZ/1./,I/(0.,1.)/,ABSLIM/1.E-5/,  Y/0./
C    +,PYPR/0./,PYPTH/0./,PYPPH/0./,YR/0./,PYRPR/0./,        PYRPT/0./,P
C    +YRPP/0./,YTH/0./,PYTPR/0./,PYTPT/0./,PYTPP/0./,  YPH/0./,PYPPR/0./
C    +,PYPPT/0./,PYPPP/0./,                        Z/0./,PZPR/0./,PZPTH/
C    +0./,PZPPH/0./
C     DATA MODRIN/'APPLETON-H','ARTREE FOR','MULA'/
C
C     REPLACING ILLEGAL DATA STATEMENT ABOVE
C
	IF(ICODE.EQ.0)THEN
	    MODRIN(1) = 'APPLETON-H'
	    MODRIN(2) = 'ARTREE FOR'
	    MODRIN(3) = 'MULA      '
	    COLL = 1.0
	    FIELD = 1.0
	    SPITZ = 1.0
	    I = (0.0, 1.0)
	    ABSLIM = 1.E-5
	    Y = 0.0
	    PYPR = 0.0
	    PYPTH = 0.0
	    PYPPH = 0.0
	    YR = 0.0
	    PYRPR = 0.0
	    PYRPT = 0.0
	    PYRPP = 0.0
	    YTH = 0.0
	    PYTPR = 0.0
	    PYTPT = 0.0
	    PYTPP = 0.0
	    YPH = 0.0
	    PYPPR = 0.0
	    PYPPT = 0.0
	    PYPPP = 0.0
	    Z = 0.0
	    PZPR = 0.0
	    PZPTH = 0.0
	    PZPPH = 0.0
	    RETURN
	END IF
C
C
C
	PXPT = 0.0
	PXPPH = 0.0
	PXPTH = 0.0
	PXPR = 0.0
	X = 0.0
	V2 = VR**2 + VT**2 + VP**2
	CALL ELECTX
	CALL MAGY
	VDOTY = VR*YR + VT*YTH + VP*YPH
	YL2 = VDOTY**2/V2
	VXY(1) = VT*YPH - VP*YTH
	VXY(2) = VP*YR - VR*YPH
	VXY(3) = VR*YTH - VT*YR
	YT2 = (VXY(1)**2+VXY(2)**2+VXY(3)**2)/V2
	PVXYPR = 2.*(VXY(1)*(VT*PYPPR-VP*PYTPR)+VXY(2)
     &		 *(VP*PYRPR-VR*PYPPR)+VXY(3)*(VR*PYTPR-VT*PYRPR))
	PVXYPT = 2.*(VXY(1)*(VT*PYPPT-VP*PYTPT)+VXY(2)
     &		 *(VP*PYRPT-VR*PYPPT)+VXY(3)*(VR*PYTPT-VT*PYRPT))
	PVXYPP = 2.*(VXY(1)*(VT*PYPPP-VP*PYTPP)+VXY(2)
     &		 *(VP*PYRPP-VR*PYPPP)+VXY(3)*(VR*PYTPP-VT*PYRPP))
	PVXYP1 = 2.*(YTH*VXY(3)-YPH*VXY(2))
	PVXYP2 = 2.*(YPH*VXY(1)-YR*VXY(3))
	PVXYP3 = 2.*(YR*VXY(2)-YTH*VXY(1))
	YPYPR = YT2*VDOTY/V2*(VR*PYRPR+VT*PYTPR+VP*PYPPR)
	YPYPT = YT2*VDOTY/V2*(VR*PYRPT+VT*PYTPT+VP*PYPPT)
	YPYPP = YT2*VDOTY/V2*(VR*PYRPP+VT*PYTPP+VP*PYPPP)
	YPYPVR = YT2*(VDOTY*YR-YL2*VR)/V2
	YPYPVT = YT2*(VDOTY*YTH-YL2*VT)/V2
	YPYPVP = YT2*(VDOTY*YPH-YL2*VP)/V2
	CALL COLFRZ
	U = DCMPLX(1.D0, -Z)
	UX = U - X
	PUPR = -I*PZPR
	PUPT = -I*PZPTH
	PUPP = -I*PZPPH
	TEST = 4.*YL2*UX**2
	IF(ABS(TEST).GE.1.E-5*YT2**2)THEN
	    GAM = (-YT2+RAY*SQRT(YT2**2+TEST))/(2.*UX)
	ELSE
*	    WRITE(16, 99001)
99001	    FORMAT(' TEST.LT.1.E-5*YT2**2')
	    GAM = (RAY-1.)*YT2/(2.*UX)
     &		   + RAY*UX*YL2/YT2*(1.-YL2*UX**2/YT2**2)
	END IF
	IF(RSTART.NE.0.)THEN
C
C  ********* FIND EQUATION NUMBER FOR INTEGRATING GAMMA
C
	    NG = 6
	    DO NR = 7, 9
		IF(W(NR+50).NE.0.)NG = NG + 1
	    END DO
C
C  ********* INITIALIZE GAMMA AT START OF INTEGRATION
C
	    R(NG+1) = DREAL(GAM)
	    R(NG+2) = DIMAG(GAM)
	END IF
C
C  *********
C
	GAMMA = DCMPLX(R(NG+1), R(NG+2))
	YPGAM = YL2 + GAMMA**2
	YMGAM = YL2 - GAMMA**2
C
C  ********* LONGITUDINAL PROPAGATION
C
	IF(ABS(YMGAM).LT.1.E-3*YL2)YMGAM = YT2*GAMMA/UX
C
C  *********
C
	IF(YL2.LT.1.E-3*Y**2)THEN
C
C  ********* TRANSVERSE PROPAGATION
C
	    YPGAM = GAMMA*(2.*UX*GAMMA+YT2)/UX
	    YMGAM = YT2*GAMMA/UX
	END IF
C
C  *********
C
	YPGPR = (YMGAM**2*(PUPR-PXPR)-YMGAM*GAMMA*PVXYPR/V2+
     &		2.*GAMMA*YPYPR)/YPGAM
	YPGPT = (YMGAM**2*(PUPT-PXPTH)-YMGAM*GAMMA*PVXYPT/V2+
     &		2.*GAMMA*YPYPT)/YPGAM
	YPGPP = (YMGAM**2*(PUPP-PXPPH)-YMGAM*GAMMA*PVXYPP/V2+
     &		2.*GAMMA*YPYPP)/YPGAM
	YPGPVR = GAMMA*(YMGAM*(2.*YT2*VR-PVXYP1)/V2+2.*YPYPVR)/YPGAM
	YPGPVT = GAMMA*(YMGAM*(2.*YT2*VT-PVXYP2)/V2+2.*YPYPVT)/YPGAM
	YPGPVP = GAMMA*(YMGAM*(2.*YT2*VP-PVXYP3)/V2+2.*YPYPVP)/YPGAM
	PGPR = YPGPR/YT2
	PGPT = YPGPT/YT2
	PGPP = YPGPP/YT2
	PGPVR = YPGPVR/YT2
	PGPVT = YPGPVT/YT2
	PGPVP = YPGPVP/YT2
	GAMU = U + GAMMA
	N2 = 1. - X/GAMU
	NPNPX = -.5/GAMU
	NPNPG = 0.5*X/GAMU**2
	NPNPU = NPNPG
	PNPR = YT2*(NPNPX*PXPR+NPNPU*PUPR) + NPNPG*YPGPR
	PNPTH = YT2*(NPNPX*PXPTH+NPNPU*PUPT) + NPNPG*YPGPT
	PNPPH = YT2*(NPNPX*PXPPH+NPNPU*PUPP) + NPNPG*YPGPP
	PNPVR = NPNPG*YPGPVR
	PNPVTH = NPNPG*YPGPVT
	PNPVPH = NPNPG*YPGPVP
	NNP = YT2*(1.+I*Z*NPNPU)
     &	       + NPNPG*(-2.*YT2*GAMMA**3+YMGAM**2*(I*Z+2.*X))/YPGAM
	PNPT = (YT2*NPNPX-YMGAM**2/YPGAM)*PXPT
	SPACE = DREAL(N2).EQ.1. .AND. ABS(DIMAG(N2)).LT.ABSLIM
	POLAR = -I*GAMMA*SQRT(V2)/VDOTY
* bug fix (sto, 1/9/93)
	IF(YT2.GT.0.0)THEN
	    LPOLAR = I*X*SQRT(YT2)/(UX*GAMU)
        ELSE
           LPOLAR = 0.0
	ENDIF

	RETURN
	END
**==AHWFNC.FOR
	SUBROUTINE AHWFNC
C
C  CALCULATES THE REFRACTIVE INDEX AND ITS GRADIENT USING THE
C  APPLETON-HARTREE FORMULA WITH FIELD, NO COLLISIONS
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /RIN   / COLL, FIELD, SPITZ, N2, N2I, PNPR, PNPRI, PNPTH, 
     &			PNPTHI, PNPPH, PNPPHI, PNPVR, PNPVRI, PNPVTH, 
     &			PNPVTI, PNPVPH, PNPVPI, NNP, NNPI, PNPT, PNPTI, 
     &			GAM, GAMI, PGP(6), POLAR, POLARI, LPOLAR, 
     &			LPOLRI, FACTR, SPACE
	COMMON /RINC  / MODRIN
	CHARACTER*10 MODRIN(3)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /YY    / Y, PYPR, PYPTH, PYPPH, YR, PYRPR, PYRPT, PYRPP, 
     &			YTH, PYTPR, PYTPT, PYTPP, YPH, PYPPR, PYPPT, 
     &			PYPPP
	COMMON /YYC   / MODY
	CHARACTER*10 MODY
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
C
	CHARACTER*10 ID(10)
	EQUIVALENCE(VR, R(4)), (VTH, R(5)), (VPH, R(6))
	EQUIVALENCE(RAY, W(1))
	LOGICAL SPACE
	REAL*8 N2, N2I, NNP, NNPI, LPOLAR, LPOLRI
	COMPLEX*16 PGP
C     DATA FIELD/1./,COLL/0./,SPITZ/0./,POLAR/0./,N2I/0./,U/1./,  Y/0./,
C    +PYPR/0./,PYPTH/0./,PYPPH/0./,YR/0./,PYRPR/0./,        PYRPT/0./,PY
C    +RPP/0./,YTH/0./,PYTPR/0./,PYTPT/0./,             PYTPP/0./,YPH/0./
C    +,PYPPR/0./,PYPPT/0./,PYPPP/0./
C     DATA MODRIN/'APPLETON-H','ARTREE FOR','MULA'/
C
C     REPLACING ILLEGAL DATA STATEMENT ABOVE
C

	IF(ICODE.EQ.0)THEN
	    MODRIN(1) = 'APPLETON-H'
	    MODRIN(2) = 'ARTREE FOR'
	    MODRIN(3) = 'MULA      '
	    FIELD = 1.0
	    COLL = 0.0
	    SPITZ = 0.0
	    POLAR = 0.0
	    N2I = 0.0
	    U = 1.0	! this corrected to 1.0, sto, 7/12/93
	    Y = 0.0
	    PYPR = 0.0
	    PYPTH = 0.0
	    PYPPH = 0.0
	    YR = 0.0
	    PYRPR = 0.0
	    PYRPT = 0.0
	    PYRPP = 0.0
	    YTH = 0.0
	    PYTPR = 0.0
	    PYTPT = 0.0
	    PYTPP = 0.0
	    YPH = 0.0
	    PYPPR = 0.0
	    PYPPT = 0.0
	    PYPPP = 0.0
	    RETURN
	END IF
C
C
C
	PXPT = 0.0
	PXPPH = 0.0
	PXPTH = 0.0
	PXPR = 0.0
	X = 0.0
	CALL ELECTX
	CALL MAGY
	V2 = VR**2 + VTH**2 + VPH**2
	VDOTY = VR*YR + VTH*YTH + VPH*YPH
	YLV = VDOTY/V2
	YL2 = VDOTY**2/V2
	YT2 = Y**2 - YL2
	YT4 = YT2*YT2
	UX = U - X
	UX2 = UX*UX
	RAD = RAY*SQRT(YT4+4.*YL2*UX2)
	D = 2.*UX - YT2 + RAD
	D2 = D*D
	N2 = 1. - 2.*X*UX/D
	PNPPS = 2.*X*UX*(-1.+(YT2-2.*UX2)/RAD)/D2
	PPSPR = YL2/Y*PYPR - (VR*PYRPR+VTH*PYTPR+VPH*PYPPR)*YLV
	PPSPTH = YL2/Y*PYPTH - (VR*PYRPT+VTH*PYTPT+VPH*PYPPT)*YLV
	PPSPPH = YL2/Y*PYPPH - (VR*PYRPP+VTH*PYTPP+VPH*PYPPP)*YLV
	PNPX = -(2.*UX2-YT2*(U-2.*X)+(YT4*(U-2.*X)+4.*YL2*UX*UX2)/RAD)
     &	       /D2
	PNPY = 2.*X*UX*(-YT2+(YT4+2.*YL2*UX2)/RAD)/(D2*Y)
	NNP = N2 - (2.*X*PNPX+Y*PNPY)
	PNPR = PNPX*PXPR + PNPY*PYPR + PNPPS*PPSPR
	PNPTH = PNPX*PXPTH + PNPY*PYPTH + PNPPS*PPSPTH
	PNPPH = PNPX*PXPPH + PNPY*PYPPH + PNPPS*PPSPPH
	PNPVR = PNPPS*(VR*YL2-VDOTY*YR)/V2
	PNPVTH = PNPPS*(VTH*YL2-VDOTY*YTH)/V2
	PNPVPH = PNPPS*(VPH*YL2-VDOTY*YPH)/V2
	PNPT = PNPX*PXPT
	SPACE = N2.EQ.1.
	POLARI = SQRT(V2)*(YT2-RAD)/(2.*VDOTY*UX)
	GAM = (-YT2+RAD)/(2.*UX)
* bug fix (sto, 1/9/93), YT2 can sometimes be very small and -ve
	IF(YT2.GE.0.0)THEN
	    LPOLRI = X*SQRT(YT2)/(UX*(U+GAM))
	ELSE
	    LPOLRI = 0.0
        ENDIF

	RETURN
	END
**==AHWFWC.FOR
	SUBROUTINE AHWFWC
C
C  CALCULATES THE REFRACTIVE INDEX AND ITS GRADIENT USING THE
C  APPLETON-HARTREE FORMULA WITH FIELD, WITH COLLISIONS
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /RIN   / COLL, FIELD, SPITZ, N2, PNPR, PNPTH, PNPPH, 
     &			PNPVR, PNPVTH, PNPVPH, NNP, PNPT, GAM, PGP(6), 
     &			POLAR, LPOLAR, FACTR, SPACE
	COMMON /RINC  / MODRIN
	CHARACTER*10 MODRIN(3)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /YY    / Y, PYPR, PYPTH, PYPPH, YR, PYRPR, PYRPT, PYRPP, 
     &			YTH, PYTPR, PYTPT, PYTPP, YPH, PYPPR, PYPPT, 
     &			PYPPP
	COMMON /YYC   / MODY
	CHARACTER*10 MODY
	COMMON /ZZ    / Z, PZPR, PZPTH, PZPPH
	COMMON /ZZC   / MODZ
	CHARACTER*10 MODZ
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
C
	CHARACTER*10 ID(10)
	EQUIVALENCE(VR, R(4)), (VTH, R(5)), (VPH, R(6))
	EQUIVALENCE(RAY, W(1))
	COMPLEX*16 N2, PNPR, PNPTH, PNPPH, PNPVR, PNPVTH, PNPVPH, NNP, 
     &		   PNPT, GAM, PGP, POLAR, LPOLAR, I, U, RAD, D, PNPPS, 
     &		   PNPX, PNPY, PNPZ, UX, UX2, D2
	LOGICAL SPACE
	
	IF(ICODE.EQ.0)THEN
	    MODRIN(1) = 'APPLETON-H'
	    MODRIN(2) = 'ARTREE FOR'
	    MODRIN(3) = 'MULA      '
	    COLL = 1.0
	    FIELD = 1.0
	    SPITZ = 0.0
	    I = (0.0, 1.0)
	    ABSLIM = 1.E-5
	    Y = 0.0
	    PYPR = 0.0
	    PYPTH = 0.0
	    PYPPH = 0.0
	    YR = 0.0
	    PYRPR = 0.0
	    PYRPT = 0.0
	    PYRPP = 0.0
	    YTH = 0.0
	    PYTPR = 0.0
	    PYTPT = 0.0
	    PYTPP = 0.0
	    YPH = 0.0
	    PYPPR = 0.0
	    PYPPT = 0.0
	    PYPPP = 0.0
	    Z = 0.0
	    PZPR = 0.0
	    PZPTH = 0.0
	    PZPPH = 0.0
	    RETURN
	END IF
C
C
C
	PXPT = 0.0
	PXPPH = 0.0
	PXPTH = 0.0
	PXPR = 0.0
	X = 0.0
	CALL ELECTX
	CALL MAGY
	V2 = VR**2 + VTH**2 + VPH**2
	VDOTY = VR*YR + VTH*YTH + VPH*YPH
	YLV = VDOTY/V2
	YL2 = VDOTY**2/V2
	YT2 = Y**2 - YL2
	YT4 = YT2*YT2
	CALL COLFRZ
	U = DCMPLX(1.D0, -Z)
	UX = U - X
	UX2 = UX*UX
	RAD = RAY*SQRT(YT4+4.*YL2*UX2)
	D = 2.*U*UX - YT2 + RAD
	D2 = D*D
	N2 = 1. - 2.*X*UX/D

	PNPPS = 2.*X*UX*(-1.+(YT2-2.*UX2)/RAD)/D2
	PPSPR = YL2/Y*PYPR - (VR*PYRPR+VTH*PYTPR+VPH*PYPPR)*YLV
	PPSPTH = YL2/Y*PYPTH - (VR*PYRPT+VTH*PYTPT+VPH*PYPPT)*YLV
	PPSPPH = YL2/Y*PYPPH - (VR*PYRPP+VTH*PYTPP+VPH*PYPPP)*YLV
	PNPX = -(2.*U*UX2-YT2*(U-2.*X)+(YT4*(U-2.*X)+4.*YL2*UX*UX2)/RAD)
     &	       /D2
	PNPY = 2.*X*UX*(-YT2+(YT4+2.*YL2*UX2)/RAD)/(D2*Y)
	PNPZ = I*X*(-2.*UX2-YT2+YT4/RAD)/D2
	PNPR = PNPX*PXPR + PNPY*PYPR + PNPZ*PZPR + PNPPS*PPSPR
	PNPTH = PNPX*PXPTH + PNPY*PYPTH + PNPZ*PZPTH + PNPPS*PPSPTH
	PNPPH = PNPX*PXPPH + PNPY*PYPPH + PNPZ*PZPPH + PNPPS*PPSPPH
	PNPVR = PNPPS*(VR*YL2/V2-YLV*YR)
	PNPVTH = PNPPS*(VTH*YL2/V2-YLV*YTH)
	PNPVPH = PNPPS*(VPH*YL2/V2-YLV*YPH)
	NNP = N2 - (2.*X*PNPX+Y*PNPY+Z*PNPZ)
	PNPT = PNPX*PXPT
	SPACE = DREAL(N2).GE.(1.-1.E-11) .AND. ABS(DIMAG(N2)).LT.ABSLIM
	POLAR = -I*SQRT(V2)*(-YT2+RAD)/(2.*VDOTY*UX)
	GAM = (-YT2+RAD)/(2.*UX)
* bug fix (sto, 1/9/93)
	IF(YT2.GE.0.0)THEN
	    LPOLAR = I*X*SQRT(YT2)/(UX*(U+GAM))
        ELSE
	    LPOLAR = 0.0
	ENDIF

	RETURN
	END
**==ATMODL.FOR
	SUBROUTINE ATMODL(Z, INDEX)
C
C  MEAN ATMOSPHERE USES ARDC 1959 ATMOSPHERE BELOW 30 KM, CIRA 1965 MEAN
C  ATMOSPHERE FOR 30 TO 300 KM, AND CIRA MODEL 5 0800 ATMOSPHERE ABOVE 30
C  COMPUTES TEMPERATURE (KELVIN), PRESSURE (DYNES/CM**2), MOLECULAR WEIGH
C  GRAVITATIONAL ACCELERATION (CM/SEC**2), AND GAMMA, FOR A HEIGHT OF Z
C  KILOMETRES. IF INDEX=1, VISCOSITY (GM/CM*SEC, OR POISE) AND THERMAL
C  CONDUCTIVITY (ERG/CM**2/SEC/DEGREE/CM)) ARE COMPUTED. IF INDEX=2, MAGN
C  FIELD (GAUSS) AND PEDERSEN CONDUCTIVITY (ABMHO/CM) ARE COMPUTED.
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /AIR   / T, P, M, G, GAM, MU, LAMBDA, B, SIG
c	REAL*8 MGR
C KSH 25.1.91 sun compile warning : local variable MGR not used
	REAL*8 LAMBDA, M, MO(46), MU, MUO(46)
	DIMENSION BO(46), EDDYMU(46), FO(46), GAMO(46), PO(46), 
     &		  SIGMAO(46), TO(46), ZO(46)
	EQUIVALENCE(GAMMA, GAM)
99001	FORMAT(
     &	      '0CURRENT VERSION OF ATMOSPHERIC MODEL DOES NOT EXTEND TO'
     &	      , F6.1, 'KM.')
C
C  THE MODEL ATMOSPHERE IS DIVIDED INTO LAYERS WITHIN WHICH THE KINETIC
C  TEMPERATURE AND OTHER ATMOSPHERIC PARAMETERS ARE TAKEN TO BE LINEAR. T
C  BASE OF THE J'TH LAYER IS AT THE ALTITUDE ZO(J).
C
	DATA ZO/0., 11., 25., 30., 40., 50., 60., 70., 80., 90., 100., 
     &	     110., 120., 130., 140., 150., 160., 170., 180., 190., 200., 
     &	     210., 220., 230., 240., 250., 260., 270., 280., 290., 300., 
     &	     320., 340., 360., 380., 400., 420., 440., 460., 480., 500., 
     &	     520., 540., 560., 580., 600./
C
C  AT WHICH THE KINETIC TEMPERATURE IS TAKEN TO BE.
C
	DATA TO/288.2, 216.7, 216.7, 230.1, 250.5, 271.0, 243.3, 216.6, 
     &	     186.0, 185.8, 208.1, 251.1, 355.0, 477.1, 573.3, 652.3, 
     &	     721.4, 783.6, 839.6, 889.7, 934.2, 971.3, 1004.3, 1033.5, 
     &	     1057.5, 1078.0, 1096.0, 1111.1, 1124.1, 1135.1, 1144.0, 
     &	     1156., 1167., 1174., 1180., 1184., 1186., 1188., 1190., 
     &	     1191., 1192., 2*1193., 3*1194./
C
C  THE PRESSURE AT THE BASE OF EACH LAYER IS.
C
	DATA PO/1.013E6, 2.270E5, 2.527E4, 1.200E4, 2.944E3, 8.096E2, 
     &	     2.195E2, 5.120E1, 9.745E0, 1.633E0, 3.095E-1, 7.690E-2, 
     &	     2.733E-2, 1.315E-2, 7.501E-3, 4.696E-3, 3.126E-3, 2.176E-3, 
     &	     1.569E-3, 1.162E-3, 8.806E-4, 6.794E-4, 5.322E-4, 4.222E-4, 
     &	     3.386E-4, 2.741E-4, 2.236E-4, 1.838E-4, 1.520E-4, 1.264E-4, 
     &	     1.056E-4, 7.451E-5, 5.347E-5, 3.884E-5, 2.850E-5, 2.110E-5, 
     &	     1.570E-5, 1.180E-5, 8.920E-6, 6.786E-6, 5.193E-6, 3.998E-6, 
     &	     3.097E-6, 2.414E-6, 1.894E-6, 1.497E-6/
C
C  THE MEAN MOLECULAR WEIGHT IS.
C
	DATA MO/9*28.9644, 28.94, 28.30, 27.66, 27.01, 26.36, 25.75, 
     &	     25.17, 24.61, 24.08, 23.56, 23.06, 22.58, 22.12, 21.68, 
     &	     21.26, 20.86, 20.48, 20.12, 19.78, 19.46, 19.16, 18.87, 
     &	     18.32, 17.88, 17.48, 17.14, 16.83, 16.55, 16.29, 16.05, 
     &	     15.80, 15.56, 15.31, 15.03, 14.74, 14.41, 14.06/
C
C  GAMMA (THE RATIO OF SPECIFIC HEATS) IS COMPUTED AS AN AVERAGE OVER SPE
C
	DATA GAMO/10*1.40, 1.41, 2*1.42, 1.43, 1.44, 1.45, 1.46, 2*1.47, 
     &	     1.48, 1.49, 1.50, 1.51, 1.52, 1.53, 2*1.54, 1.55, 1.56, 
     &	     1.57, 1.58, 1.59, 1.60, 1.61, 1.62, 2*1.63, 2*1.64, 3*1.65, 
     &	     4*1.66/
C
C  FCTR IS A NUMBER, RELATED TO THE PRANDTL NUMBER, USED TO CALCULATE LAM
C
	DATA FO/10*1.88, 1.91, 1.94, 1.97, 2.00, 2.03, 2.06, 2.09, 2.12, 
     &	     2.15, 2.17, 2.20, 2.23, 2.25, 2.27, 2.29, 2.31, 2.33, 2.35, 
     &	     2.37, 2.39, 2.40, 2.43, 2.45, 2.47, 2.49, 2.50, 2.52, 
     &	     2*2.53, 2.54, 3*2.55, 3*2.56/
C
C  VISCOSITY IS COMPUTED BY SUTHERLAND'S FORMULA FOR MOLECULES
C  (SEE KAYE & LABY, TABLES OF PHYSICAL AND CHEMICAL CONSTANTS,1966),
C  AND BY DALGARNO AND SMITH (PLANET.SPACE SCI.,1962) FOR ATOMIC
C  OXYGEN, AND A WEIGHTED MEAN IS TAKEN.
C
	DATA MUO/1.79E-4, 2*1.42E-4, 1.49E-4, 1.60E-4, 1.70E-4, 1.56E-4, 
     &	     1.42E-4, 2*1.25E-4, 1.38E-4, 1.60E-4, 2.09E-4, 2.57E-4, 
     &	     2.92E-4, 3.18E-4, 3.41E-4, 3.60E-4, 3.78E-4, 3.94E-4, 
     &	     4.08E-4, 4.20E-4, 4.30E-4, 4.40E-4, 4.48E-4, 4.56E-4, 
     &	     4.62E-4, 4.68E-4, 4.73E-4, 4.77E-4, 4.81E-4, 4.87E-4, 
     &	     4.92E-4, 4.96E-4, 4.99E-4, 5.02E-4, 5.04E-4, 5.05E-4, 
     &	     5.06E-4, 5.07E-4, 5.08E-4, 2*5.09E-4, 3*5.10E-4/
C
C  EDDY VISCOSITY IS ESTIMATED BY MIDGLEY AND LIEMOHN (J.GEOPHYS.RES.,196
C
	DATA EDDYMU/6.1, 1.8, .29, .14, 6.E-2, 3.2E-2, 7.2E-2, 5.1E-2, 
     &	     2.E-2, 6.3E-3, 2.E-3, 3.9E-4, 7.3E-5, 4.9E-5, 2.5E-5, 
     &	     1.8E-6, 5.8E-7, 1.6E-7, 8.E-8, 4.3E-9, 26*0./
C
C  THE AEROSPACE ENVIRONMENT CHART OF THE A.F.C.R.L. GIVES THE MAGNETIC F
C
	DATA BO/.570, .567, .564, .562, .559, .556, .553, .550, .548, 
     &	     .545, .542, .540, .537, .533, .531, .529, .526, .524, .521, 
     &	     .519, .516, .514, .511, .509, .506, .504, .501, .499, .496, 
     &	     .494, .491, .486, .482, .477, .473, .468, .464, .459, .455, 
     &	     .451, .447, .442, .438, .434, .430, .426/
C
C  THE SATELLITE ENVIRONMENT HANDBOOK GIVES THE PEDERSEN CONDUCTIVITY:
C
	DATA SIGMAO/4*0., 5.E-20, 2.E-19, 6.E-19, 5.E-18, 1.E-17, 
     &	     2.E-17, 1.E-16, 7.E-16, 3.E-15, 3.E-15, 2*2.E-15, 2*1.E-15, 
     &	     9.E-16, 7.E-16, 2*6.E-16, 5.E-16, 4*4.E-16, 5*5.E-16, 
     &	     2*3.E-16, 2.E-16, 1.E-16, 9.E-17, 7.E-17, 6.E-17, 4.E-17, 
     &	     2*3.E-17, 2*2.E-17, 2*1.E-17/
C
C  MISCELLANEOUS CONSTANTS ARE THE GAS CONSTANT R, THE RADIUS OF THE EART
C  (RADIUS), AND THE VALUE OF G AT THE EARTH'S SURFACE MULTIPLIED BY RADI
C  THE LAST TWO CONSTANTS ARE USED TO CALCULATE THE GRAVITATIONAL ACCELER
C  G, AND ARE CHOSEN SO THAT G WILL AGREE WITH CIRA AT THE SURFACE
C  AND AT 300 KM.
C
	DATA R, RADIUS, GR2/8.31432E7, 6374.3, 3.9791E10/
	J = Z/10.
C
C  TO BEGIN WITH,CALCULATE WHICH LAYER Z IS IN:
C
	IF(Z.LT.11.)J = 0
	IF(Z.GE.11. .AND. Z.LT.25.)J = 1
	IF(J.GT.30)J = 30 + (J-30)/2
	J = J + 1
	IF(J.EQ.46)J = 45
	IF(J.LE.45)THEN
	    JP1 = J + 1
C
C  CALCULATE THE DESIRED QUANTITIES IN THE J'TH LAYER.
C
	    ZRATIO = (Z-ZO(J))/(ZO(JP1)-ZO(J))
C
C  COMPUTE TEMPERATURE ,MOLECULAR WEIGHT AND GAMMA.
C
	    T = TO(J) + ZRATIO*(TO(JP1)-TO(J))
	    M = 28.9644
	    GAMMA = 1.4
	    FCTR = 1.88
	    IF(J.GE.10)THEN
		M = MO(J) + ZRATIO*(MO(JP1)-MO(J))
		GAMMA = GAMO(J) + ZRATIO*(GAMO(JP1)-GAMO(J))
		IF(INDEX.GT.0)FCTR = FO(J) + ZRATIO*(FO(JP1)-FO(J))
	    END IF
	ELSE
	    WRITE(6, 99001)Z
	    STOP
	END IF
	ZPR = Z + RADIUS
C
C  COMPUTE PRESSURE AND GRAVITATIONAL FIELD.
C
	P = PO(J)*EXP(-ZRATIO*LOG(PO(J)/PO(JP1)))
	G = GR2/(ZPR*ZPR)
	IF(INDEX.EQ.0)RETURN
C
C  COMPUTE TRANSPORT COEFFICIENTS. SEE MIDGLEY AND LIEMOHN FOR METHOD OF
C  DETERMINING LAMBDA.
C
	MU = MUO(J) + ZRATIO*(MUO(JP1)-MUO(J))
	IF(J.LE.20)MU = MU + EDDYMU(J) + ZRATIO*(EDDYMU(JP1)-EDDYMU(J))
	LAMBDA = MU*FCTR*R/M/(GAMMA-1.)
	IF(INDEX.EQ.1)RETURN
C
C  CALCULATE PEDERSEN CONDUCTIVITY AND MAGNETIC FIELD.
C
	SIG = SIGMAO(J) + ZRATIO*(SIGMAO(JP1)-SIGMAO(J))
	B = BO(J) + ZRATIO*(BO(JP1)-BO(J))
	RETURN
	END
**==BACKUP.FOR
	SUBROUTINE BACKUP(HS)
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /RK    / STEP, E1MAX, E1MIN, E2MAX, E2MIN, FACT, 
     &			RSTART, N, MODE
	COMMON /TRAC  / SMT, GROUND, PERIGE, THERE, MINDIS, NEWRAY
	COMMON R(20), T, STP, DRDT(20)/WW/W0, W(400)
	COMMON /WWC   / ID
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (INTYP, W(41)), (STEP1, W(44))
	REAL*8 INTYP
	LOGICAL GROUND, PERIGE, THERE, MINDIS, NEWRAY, HOME
C
C  ********* DIAGNOSTIC PRINTOUT
C  CALL PRINTR ('BACK UP0',0.)
C  ********* GOING AWAY FROM THE HEIGHT HS
C
	HOME = DRDT(1)*(R(1)-EARTHR-HS).GE.0.
	IF(HS.GT.0. .AND. .NOT.HOME .OR. HS.EQ.0. .AND. DRDT(1).GT.0.)
     &	   GO TO 100
C
C  ********* FIND NEAREST INTERSECTION OF RAY WITH THE HEIGHT HS
C
	DO I = 1, 10
	    STEP = -(R(1)-EARTHR-HS)/DRDT(1)
	    STEP = SIGN(MIN(ABS(STP),ABS(STEP)), STEP)
	    IF(ABS(R(1)-EARTHR-HS).LT..5E-4 .AND. ABS(STEP).LT.1.)
     &	       GO TO 200
C
C  ********* DIAGNOSTIC PRINTOUT
C  CALL PRINTR('BACK UP1',0.)
C
	    MODE = 1
	    RSTART = 1.
	    CALL RKAM
	    RSTART = 1.
	END DO
C
C
C  ********* FIND NEAREST CLOSEST APPROACH OF RAY TO THE HEIGHT HS
C
	ENTRY GRAZE(hs)
c	hs=0.0
	THERE = .FALSE.
C
C  ********* DIAGNOSTIC PRINTOUT
C  CALL PRINTR ('GRAZE 0 ',0.)
C
	IF(SMT.LE.ABS(R(1)-EARTHR-HS))THEN
	    DO I = 1, 10
		STEP = -R(4)/DRDT(4)
		STEP = SIGN(MIN(ABS(STP),ABS(STEP)), STEP)
		IF(ABS(R(4)).LE.1.E-6 .AND. ABS(STEP).LT.1.)GO TO 200
C
C  ********* DIAGNOSTIC PRINTOUT
C  CALL PRINTR ('GRAZE 1 ',0.)
C
		MODE = 1
		RSTART = 1.
		CALL RKAM
		RSTART = 1.
		IF(DRDT(4)*(R(1)-EARTHR-HS).LT.0.)GO TO 100
		IF(R(5).EQ.0. .AND. R(6).EQ.0.)GO TO 200
	    END DO
	END IF
C
C  ********* IF A CLOSEST APPROACH COULD NOT BE FOUND IN 10 STEPS, IT
C  ********* PROBABLY MEANS THAT THE RAY INTERSECTS THE HEIGHT HS
C
C
C  ********* DIAGNOSTIC PRINTOUT
C  CALL PRINTR ('BACK UP2',0.)
C
100	CONTINUE
	MODE = 1
C
C  ********* ESTIMATE DISTANCE TO NEAREST INTERSECTION OF RAY WITH HEIGHT
C  ********* HS BEHIND THE PRESENT RAY POINT
C
* bug fix, sto, 20/1/94 (prevents -ve sqrt).
* second bug fix, sto, 29/4/94. else step set too large occasionally
	TEMPVAL=R(4)**2-2.*(R(1)-EARTHR-HS)*DRDT(4)
	IF(TEMPVAL.LT.0.0)THEN
		STEP=-R(4)/DRDT(4)	! just taking real part
		STEP = SIGN(MIN(ABS(STP),ABS(STEP)), STEP)	! fixes step size
	ELSE
		STEP = (-R(4)-SQRT(TEMPVAL))/DRDT(4)
	   STEP = SIGN(MIN(ABS(STP),ABS(STEP)), STEP)
	ENDIF
	RSTART = 1.
	CALL RKAM
	RSTART = 1.
C
C  ********* FIND NEAREST INTERSECTION OF RAY WITH HEIGHT HS
C
	DO I = 1, 10
	    STEP = -(R(1)-EARTHR-HS)/DRDT(1)
	    STEP = SIGN(MIN(ABS(STP),ABS(STEP)), STEP)
	    IF(ABS(R(1)-EARTHR-HS).LT..5E-4 .AND. ABS(STEP).LT.1.)
     &	       GO TO 200
C
C  ********* DIAGNOSTIC PRINTOUT
C  CALL PRINTR ('BACK UP3',0.)
C
	    MODE = 1
	    RSTART = 1.
	    CALL RKAM
	    RSTART = 1.
	END DO
	THERE = .TRUE.
C
C  ********* RESET STANDARD MODE AND INTEGRATION TYPE
C
200	CONTINUE
	MODE = INTYP
	STEP = STEP1
	RETURN
	END
**==BULGE.FOR
	SUBROUTINE BULGE
C
C  ANALYTICAL MODEL OF THE VARIATION OF THE EQUATORIAL F2 LAYER
C  IN GEOMAGNETIC LATITUDE  (EQUATORIAL BULGE AND ANOMALY)
C  SEE FIGURE 3.18B, PAGE 133 IN DAVIES (1965).
C  THIS MODEL HAS NO VARIATION IN GEOMAGNETIC LONGITUDE.
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /XX2   / PX2PR
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /PERTOF/ISWITCH
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (PERT, W(150))
C
	IF(ICODE.EQ.0)THEN
	    MODX(1) = ' BULGE'
	    RETURN
	END IF
	H = R(1) - EARTHR
	PFC2P1 = 0.
	PHMPTH = 0.0
	HMAX = 350.
	FC2 = 225.
	IF(H.GE.100.)THEN
C
C  EQUATORIAL BULGE
C
	    BULLAT = 7.5*(PID2-R(2))
	    IF(ABS(BULLAT).LT.PI)THEN
		HMAX = 430. + 80.*COS(BULLAT)
		PHMPTH = 600.*SIN(BULLAT)
	    END IF
C
C  EQUATORIAL ANOMALY
C
	    ANMLAT = 22.5*(PID2-R(2))/PI
	    POW = 2. - ABS(ANMLAT)
	    FC2 = 50.*ANMLAT**2*EXP(POW) + 40.
	    PFC2P1 = -1125./PI*POW*ANMLAT*EXP(POW)
	END IF
C
C  FORCING PLASMA FREQ AT 100 KM TO BE 2 MHZ IN ORDER TO CALCULATE SH
C
	ALPHA = 2.*LOG(FC2/4.) + 1.
	Z100 = -LOG(ALPHA)
	DO I = 1, 5
	    Z100 = -LOG(ALPHA-Z100)
	END DO
	SH = (100.-HMAX)/Z100
	Z = (H-HMAX)/SH
	EXZ = 1. - EXP(-Z)
	X = FC2*EXP(.5*(EXZ-Z))/F**2
	PXPR = -0.5*X*EXZ/SH
	PX2PR = .25*X/SH**2*EXZ**2 - X/2./SH**2*EXP(-Z)
	PXPTH = -PXPR*(1.-Z/Z100)
     &		*PHMPTH + (1.-Z*EXZ/(Z100*(1.-EXP(-Z100))))*X/FC2*PFC2P1
	PXPPH = 0.
	IF(PERT.NE.0. .AND. ISWITCH.NE.0) CALL ELECT1
	RETURN
	END
**==C.FOR
	FUNCTION C(X)
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	DOUBLE PRECISION PIH, XD, Y, V, A, QZ, QN, Q, Z
	DATA A1/0.3183099/, A2/0.10132/, B1/0.0968/, B2/0.154/
	PIH = 1.5707963267949
	XA = ABS(X)
	IF(XA.LE.4.)THEN
C
C
C
	    XD = X
	    Y = PIH*XD*XD
	    V = Y*Y
	    A = 1.D0
	    Z = A
	    M = 15.*(XA+1.)
	    DO I = 1, M
		KZ = 2*(I-1)
		KV = 4*(I-1)
		QZ = KV + 1
		QN = (KZ+1)*(KZ+2)*(KV+5)
		Q = QZ/QN
		A = -A*Q*V
		Z = Z + A
	    END DO
	    Z = Z*XD
	    C = Z
	    RETURN
	END IF
C
C
C
	W = PIH*X*X
	XV = XA**4
	C = 0.5 + (A1-B1/XV)*SIN(W)/XA - (A2-B2/XV)*COS(W)/XA**3
	IF(X.LT.0.)C = -C
	RETURN
	END
**==CHAPX.FOR
	SUBROUTINE CHAPX
C
C  CHAPMAN LAYER WITH TILTS, RIPPLES, AND GRADIENTS
C  W(104) = 0.5 FOR AN ALPHA-CHAPMAN LAYER
C  = 1.0 FOR A BETA-CHAPMAN LAYER
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /XX2   / PX2PR
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /PERTOF/ISWITCH
	CHARACTER*10 ID(10)
	EQUIVALENCE(THETA, R(2))
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (FC, W(101)), (HM, W(102))
     &		    , (SH, W(103)), (ALPHA, W(104)), (A, W(105)), 
     &		    (B, W(106)), (C, W(107)), (E, W(108)), 
     &		    (PERT, W(150))
C
	IF(ICODE.EQ.0)THEN
	    MODX(1) = ' CHAPX'
	    RETURN
	END IF
	THETA2 = THETA - PID2
	HMAX = HM + EARTHR*E*THETA2
	H = R(1) - EARTHR
	Z = (H-HMAX)/SH
	D = 0.
	IF(B.NE.0.)D = PIT2/B
	TEMP = 1. + A*SIN(D*THETA2) + C*THETA2
	EXZ = 1. - EXP(-Z)
	X = (FC/F)**2*TEMP*EXP(ALPHA*(EXZ-Z))
	PXPR = -ALPHA*X*EXZ/SH
	PX2PR = ALPHA**2*X*EXZ**2/SH**2 - ALPHA*X/SH**2*EXP(-Z)
	PXPTH = X*(D*A*SIN(PID2-D*THETA2)+C)/TEMP - PXPR*EARTHR*E
	PXPPH = 0.
	IF(PERT.NE.0. .AND. ISWITCH.NE.0)CALL ELECT1
c	WRITE(47, 99001)H, X, W(157)
99001	FORMAT(' ', F20.10, E20.10, F20.10)
	RETURN
	END
**==CLOUD.FOR
	SUBROUTINE CLOUD
C
C  PERTURBATION BY SPHERICALLY SYMMETRIC CLOUD
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX/CONST/PI, 
     &			PIT2, PID2, DEGS, RAD, AC, DUM(2)
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	COMMON /FLG   / NTYP, NEWWR, NEWWP, PENET, LINES, IHOP, HPUNCH
	EQUIVALENCE(EARTHR, W(2)), (PLAT, W(24)), (PLON, W(25)), 
     &		    (Z0, W(151)), (WD, W(152)), (PEAK, W(153)), 
     &		    (CLAT, W(154)), (CLON, W(155)), (F, W(6))
	LOGICAL NEWWP, NEWWR, PENET
C
	IF(ICODE.EQ.0)THEN
	    MODX(2) = ' CLOUD '
	    RETURN
	END IF
	IF(NEWWP)THEN
C
C  CONVERT CLOUD CENTRE TO GEOMAGNETIC COLAT. AND LONG.
C
	    PEAK = PEAK
	    SP = SIN(PLAT)
	    CP = COS(PLAT)
	    SDPH = SIN(CLON-PLON)
	    CDPH = COS(CLON-PLON)
	    SL = SIN(CLAT)
	    CL = COS(CLAT)
	    GCLAT = ACOS(CDPH*CP*CL+SP*SL)
	    GCLON = ATAN2(SDPH*CL, CDPH*SP*CL-CP*SL)
	    SC = SIN(GCLAT)
	    CC = COS(GCLAT)
	    RC = EARTHR + Z0
	END IF
	STH = SIN(R(2))
	CTH = COS(R(2))
	SLN = SIN(R(3)-GCLON)
	CLN = COS(R(3)-GCLON)
	TEST = (R(1)**2+RC**2-2.*R(1)*RC*(STH*SC*CLN+CTH*CC))/WD**2
	IF(TEST.GT.100.)RETURN
	EL = PEAK*EXP(-TEST)*AC/F**2
	X = X + EL
	PXPR = PXPR - 2.*EL/WD**2*(R(1)-RC*(STH*SC*CLN+CTH*CC))
	PXPTH = PXPTH + 2.*EL/WD**2*R(1)*RC*(CTH*SC*CLN-STH*CC)
	PXPPH = PXPPH - 2.*EL/WD**2*R(1)*RC*STH*SC*SLN
	PXPT = 0.
	RETURN
	END
**==COLFRZ.FOR
	SUBROUTINE COLFRZ
C
C ---- SELECTS THE REQUIRED COLLISION FREQUENCY PROFILE
C
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
C
	IF(INDEXC.LT.1 .OR. INDEXC.GT.4)CALL PMDSTOP
	IF(INDEXC.EQ.1)THEN
	    CALL TABLEZ
	ELSE IF(INDEXC.EQ.2)THEN
	    CALL CONSTZ
	ELSE IF(INDEXC.EQ.3)THEN
	    CALL EXPZ
	ELSE IF(INDEXC.EQ.4)THEN
	    CALL EXPZ2
	END IF
	RETURN
	END
**==CONSTY.FOR
	SUBROUTINE CONSTY
C
C  CONSTANT DIP AND GYROFREQUENCY
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /YY    / Y, PYPR, PYPTH, PYPPH, YR, PYRPR, PYRPT, PYRPP, 
     &			YTH, PYTPR, PYTPT, PYTPP, YPH, PYPPR, PYPPT, 
     &			PYPPP
	COMMON /YYC   / MODY
	CHARACTER*10 MODY
	COMMON /WW    / W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(F, W(6)), (FH, W(201)), (DIP, W(202))
C
	IF(ICODE.EQ.0)THEN
	    MODY = 'CONSTY'
	    RETURN
	END IF
	Y = FH/F
	YR = Y*SIN(DIP)
	YTH = Y*COS(DIP)
	RETURN
	END
**==CONSTZ.FOR
	SUBROUTINE CONSTZ
C
C  CONSTANT COLLISION FREQUENCY
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /ZZ    / Z, PZPR, PZPTH, PZPPH
	COMMON /ZZC   / MODZ
	CHARACTER*10 MODZ
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (NU, W(251)), 
     &		    (HMIN, W(252))
	REAL*8 NU
C
	IF(ICODE.EQ.0)THEN
	    MODZ = 'CONSTZ'
	    RETURN
	END IF
C
	H = R(1) - EARTHR
	Z = 0.
	IF(H.GT.HMIN)Z = NU/(PIT2*F)*1.E-6
	RETURN
	END
**==CUBEY.FOR
	SUBROUTINE CUBEY
C
C  CONSTANT DIP.
C  GYROFREQ DECREASES AS CUBE OF DISTANCE FROM CENTER OF EARTH.
C  THIS MODEL HAS SAME HEIGHT VARIATION AS A DIPOLE FIELD.
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /YY    / Y, PYPR, PYPTH, PYPPH, YR, PYRPR, PYRPT, PYRPP, 
     &			YTH, PYTPR, PYTPT, PYTPP, YPH, PYPPR, PYPPT, 
     &			PYPPP
	COMMON /YYC   / MODY
	CHARACTER*10 MODY
	COMMON R/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (FH, W(201)), 
     &		    (DIP, W(202))
C
	IF(ICODE.EQ.0)THEN
	    MODY = 'CUBEY'
	    RETURN
	END IF
C
	Y = (EARTHR/R)**3*FH/F
	YR = Y*SIN(DIP)
	YTH = Y*COS(DIP)
	PYPR = -3.*Y/R
	PYRPR = -3.*YR/R
	PYTPR = -3.*YTH/R
	RETURN
	END
**==DATT.FOR
	SUBROUTINE DATT(NDATE)
C
C     MACHINE DEPENDENT SUBROUTINE TO RETURN THE DATE
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	CHARACTER*9 NDATE
	CALL FDATE(NDATE)
	RETURN
	END
**==DCHAPT.FOR
	SUBROUTINE DCHAPT
C
C  TWO CHAPMAN LAYERS WITH TILTS
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /XX2   / PX2PR
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /PERTOF/ISWITCH
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (FC1, W(101)), 
     &		    (HM1, W(102)), (SH1, W(103)), (FC2, W(104)), 
     &		    (HM2, W(105)), (SH2, W(106)), (C, W(107)), 
     &		    (COSYS, W(109)), (E, W(108)), (PERT, W(150))
C
	IF(ICODE.EQ.0)THEN
	    MODX(1) = 'DCHAPT'
	    RETURN
	END IF
	EARTHE = EARTHR*E
	IF(cosys .EQ. 1)THEN 
		theta2 = r(2) - PID2
	ELSE
		CALL c2geog(PID2-r(2), r(3), templat, templon)
		theta2=templat-PID2
	ENDIF
	HMAX = HM1 + EARTHE*THETA2
	PXPTH = 0.
	PXPR = 0.0
	X = 0.0
	PXPPH = 0.
	PX2PR = 0.
	H = R(1) - EARTHR
	IF(H.GE.0.)THEN
	    Z1 = (H-HMAX)/SH1
	    EXPZ1 = 1. - EXP(-Z1)
	    TEMP = 1. + C*THETA2
* check for -ve NE (sto, 19/8/96)
	    IF(TEMP.LT.0.0) TEMP=0.0
	    X = 0.
	    IF(0.5*(EXPZ1-Z1).GE.-60.)X = (FC1/F)
     &	       **2*TEMP*EXP(.5*(EXPZ1-Z1))
	    PXPR = -0.5*X*EXPZ1/SH1
	    PX2PR = -0.5/SH1*(PXPR*EXPZ1+X*EXP(-Z1)/SH1)
	    PXPTH = X*C/TEMP - PXPR*EARTHE
	    IF(FC2.NE.0.)THEN
		Z2 = (H-HM2-EARTHE*THETA2)/SH2
		EXPZ2 = 1. - EXP(-Z2)
		X2 = 0.
		IF(0.5*(EXPZ2-Z2).GE.-60.)X2 = (FC2/F)
     &		   **2*TEMP*EXP(.5*(EXPZ2-Z2))
		X = X + X2
		PXPR2 = -0.5*X2*EXPZ2/SH2
		PXPR = PXPR + PXPR2
		PX2PR2 = -0.5/SH2*(PXPR2*EXPZ2+X2*EXP(-Z2)/SH2)
		PX2PR = PX2PR + PX2PR2
		PXPTH = PXPTH + X2*C/TEMP - PXPR2*EARTHE
	    END IF
	END IF
	IF(PERT.NE.0. .AND. ISWITCH.NE.0)CALL ELECT1
c KSH 17.1.91 when determining whether a ray has penetrated reference
c is made to HMAX.  This is calculated form the height of the max in the
c lower of the 2 layers in this model.  Rays therefore appear to penetrate
c the ionosphere once they have passed this HMAX but may still be within the
c 2nd layer
c Therefore set HMAX = 400.0 , this should be appropriate for most ionospheres
	HMAX = 400.0
	RETURN
	END
**==DIPOLY.FOR
	SUBROUTINE DIPOLY
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /YY    / Y, PYPR, PYPTH, PYPPH, YR, PYRPR, PYRPT, PYRPP, 
     &			YTH, PYTPR, PYTPT, PYTPP, YPH, PYPPR, PYPPT, 
     &			PYPPP
	COMMON /YYC   / MODY
	CHARACTER*10 MODY
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (FH, W(201))
C
	IF(ICODE.EQ.0)THEN
	    MODY = 'DIPOLY'
	    open(97,file='out/magy.dat')
	    write(97,*) 'height lat lon Y YR YTH YPH'
	    RETURN
	END IF
	SINTH = SIN(R(2))
	COSTH = SIN(PID2-R(2))
	TERM9 = SQRT(1.+3.*COSTH**2)
	T1 = FH*(EARTHR/R(1))**3/F
	Y = T1*TERM9
	YR = 2.*T1*COSTH
	YTH = T1*SINTH
	PYRPR = -3.*YR/R(1)
	PYRPT = -2.*YTH
	PYTPR = -3.*YTH/R(1)
	PYTPT = .5*YR
	PYPR = -3.*Y/R(1)
	PYPTH = -3.*Y*SINTH*COSTH/TERM9**2
	write(97,*) R(1)-EARTHR,R(2),R(3),Y,YR,YTH,YPH
	RETURN
	END
**==DISPER.FOR
	SUBROUTINE DISPER(NWAVE)
C
C  SOLVES THE ACOUSTIC GRAVITY DISPERSION RELATION IN THE PRESENCE OR ABS
C  OF VISCOSITY AND THERMAL CONDUCTIVITY (NWAVE=2 OR NWAVE=6 IS THE NUMBE
C  OF PLANE WAVE SOLUTIONS).
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /INPT  / KX, OMEGA, INTGR(16), NLAYER
	COMMON /MISC  / ALF241, ALPHA2, BETA2, E1MB, E1PB, ETA3, ETA4, 
     &			ETA7, ETA11, ETA31, ETA41, ETA71, IALFD2, 
     &			IALPHA, IETALF, ISB, ISB1, ISB2, ISB3, ISBBY, 
     &			ISBXZ, SBBY2, SBBY2N, BXBY, BX2, BY2, BZ2, 
     &			GAMMA1
	COMMON /OUTPT / K(6), AP(6), ARB(16), ARBC(16)
	COMMON /ATMOS / ALPHA, BETA, ETA, NU, BX, BY, BZ, GAMMA, SIGMA
	COMMON /TRAV  / TOPATM, BOTATM, TOLMAX, TOLMIN, CURAMP(6, 2), 
     &			OLDAMP(6), ZBEG, ZEND, DEPTHO, Z, NEQNS
	COMPLEX*16 A, A0, A1, ALF241, ALPHA, ALPHA2, AP, AUXC1, AUXC2, 
     &		   AUXC3, B, B0, B1, B2, BETA, BETA2, C, C0, C1, C2, C3, 
     &		   D, D0, D1, D2, E1MB, E1PB, ETA, ETA3, ETA4, ETA7, 
     &		   ETA11, ETA31, ETA41, ETA71, G0, G1, G2, G3, G4, 
     &		   IALFD2, IALPHA, IETALF, ISB, ISB1, ISB2, ISB3, ISBBY, 
     &		   ISBXZ, K, KX, NU, R(4), RCHECK, SBBY2, SBBY2N, X1, 
     &		   X2, X3, SMALL, ARBC, CURAMP, OLDAMP
	DIMENSION ERROR(4)
	EQUIVALENCE(INDEX, INTGR(1)), (NTEST, INTGR(3)), 
     &		    (MPRINT, INTGR(4)), (IEXTR, INTGR(6)), 
     &		    (KCHECK, INTGR(8)), (KEY, INTGR(10))
	DATA TWOPI, SMALL/6.2831853, (0., 1.E-15)/
99001	FORMAT('0FIRST WAVE NUMBER IS NOT THE GRAVITY WAVE AT Z =', 
     &	       -5PF7.2//' PITTEWAY & HINES:', 
     &	       0P2E14.4//' EXACT SOLUTION:  ', 8E14.4)
99002	FORMAT('0HELP! NWAVE IS GREATER THAN 6'///)
	C1 = -BETA*(1.+1./GAMMA1)
	C0 = BETA2/GAMMA1 + ALPHA2
	IF(NWAVE.GT.2)THEN
	    AUXC1 = ETA3*ETA41
	    C3 = -AUXC1*NU
	    C2 = AUXC1/GAMMA1 + NU*BETA*ETA71 + ETA3
	    AUXC2 = ALPHA2*ETA31
	    AUXC1 = AUXC2*ETA
	    AUXC1 = -BETA2 - AUXC1 - AUXC1
	    C1 = -NU*AUXC1 - BETA*ETA71/GAMMA1 - BETA
	    C0 = AUXC1/GAMMA1 + AUXC2
	    IF(INDEX.LT.2)THEN
		A = C1/C0
		B = C2/C0
		C = C3/C0
	    ELSE
		AUXC1 = GAMMA1 + E1MB
		AUXC2 = GAMMA1 + E1PB
		AUXC3 = GAMMA1 + ETA41
		B2 = -NU*(ISB1*ETA41+ISB3*ETA3)
		B1 = ISB1*(AUXC3/GAMMA1+NU*E1PB)
     &		      + ISB3*(ETA3/GAMMA1-NU*E1MB) + SBBY2N
		B0 = (ISB3*AUXC1-ISB1*AUXC2-SBBY2)/GAMMA1
		IF(NWAVE.GT.6)THEN
		    A0 = ISB2 - BETA
		    A1 = ETA3
		    D2 = -SBBY2N*(ETA3*BZ2+ETA41*BX2)
		    D1 = (SBBY2N*(-BZ2*(E1MB+ISB)+BX2*(E1PB-ISB))
     &			 +SBBY2*(BZ2*ETA3+BX2*AUXC3)/GAMMA1)/GAMMA1
		    D0 = SBBY2/GAMMA1*(BZ2*(AUXC1+ISB)-BX2*(AUXC2-ISB))
		    AUXC3 = C2 + B2
		    AUXC2 = C1 + B1
		    AUXC1 = C0 + B0
		    G4 = A1*C3
		    G3 = A0*C3 + A1*AUXC3
		    G2 = A0*AUXC3 + A1*AUXC2 + D2
		    G1 = A0*AUXC2 + A1*AUXC1 + D1
		    G0 = A0*AUXC1 + D0
		    A = G3/G4
		    B = G2/G4
		    C = G1/G4
		    D = G0/G4
C
C  CALL POLY4(A,B,C,D,R(4),R(3),R(1),R(2))
C
		    WRITE(6, 99002)
		    GO TO 100
		ELSE
		    C = C0 + B0
		    A = (C1+B1)/C
		    B = (C2+B2)/C
		    C = C3/C
		END IF
	    END IF
	ELSE
	    AUXC1 = SQRT(-C0/C1-ALF241)
C
C  AUXC1 IS THE SAME AS LOWER CASE KZ (DIVIDED BY KX).
C  DECIDE WHETHER WAVE IS AN INTERNAL WAVE OR A SURFACE WAVE
C
	    IF(ABS(DREAL(AUXC1)).LT.ABS(DIMAG(AUXC1)))THEN
C
C  FOR SURFACE WAVE, REQUIRE FIRST WAVE TO DECREASE EXPONENTIALLY.
C
		IF(DIMAG(AUXC1).GT.0.)AUXC1 = -AUXC1
	    ELSE
C
C  FOR INTERNAL WAVE, REQUIRE GROUP VELOCITY OF FIRST WAVE TO BE UPWARD.
C
		IF(DREAL(ALPHA*BETA*AUXC1/(BETA2-GAMMA1*ALPHA2)).LT.0.)
     &		   AUXC1 = -AUXC1
	    END IF
	    K(1) = AUXC1 + IALFD2
	    K(2) = -AUXC1 + IALFD2
	    RETURN
	END IF
	CALL POLY3(A, B, C, X1, X2, X3, 1)
	IF(ABS(X1).LT.1.E-15)X1 = SMALL
	IF(ABS(X2).LT.1.E-15)X2 = SMALL
	IF(ABS(X3).LT.1.E-15)X3 = SMALL
	R(1) = (1., 0.)/X1
	R(2) = (1., 0.)/X2
	R(3) = (1., 0.)/X3
100	CONTINUE
	DO J = 2, NWAVE, 2
	    I = J/2
	    AUXC1 = SQRT(R(I)-ALF241)
C
C  AUXC1 IS THE SAME AS LOWER CASE KZ (DIVIDED BY KX).
C
	    IF(DIMAG(AUXC1).GT.0.)AUXC1 = -AUXC1
	    K(J-1) = AUXC1 + IALFD2
	    K(J) = -AUXC1 + IALFD2
	END DO
	IF(KCHECK.EQ.0)RETURN
	AUXC1 = SQRT((BETA2+GAMMA1*ALPHA2)/(GAMMA*BETA)-ALF241)
	AUXC2 = 1.5*ETA*BETA/(AUXC1*GAMMA*GAMMA)
     &		*((GAMMA1*ALPHA2/BETA2)**2+(GAMMA*(GAMMA-2.)+.33333333)
     &		*ALPHA2/BETA2-1.3333333)
	AUXC3 = NU*GAMMA1/(2.*AUXC1*GAMMA)*(AUXC1*AUXC1+ALF241)
     &		*(AUXC1*AUXC1+ALF241-BETA)
	AUXC1 = AUXC1 + AUXC2 + AUXC3
	RCHECK = AUXC1*AUXC1 + ALF241
	JMAX = NWAVE/2
	DO J = 1, JMAX
	    ERROR(J) = ABS(R(J)-RCHECK)
	END DO
	IF(ERROR(1).LT.MIN(ERROR(2),ERROR(3)))RETURN
	WRITE(6, 99001)Z, RCHECK, (R(J), J=1, JMAX)
	RETURN
	END
**==DOPPLE.FOR
	SUBROUTINE DOPPLE
C
C  COMPUTES DN/DT FROM PROFILES HAVING THE SAME FORM
C  AS THOSE USED BY SUBROUTINE TABLE X
C  NEEDS SUBROUTINE GAUSEL
C  MAKES AN EXPONENTIAL EXTRAPOLATION DOWN USING THE BOTTOM
C  TWO POINTS.
C  ADDED INTO THE RAY TRACING PROGRAM BY CT.SPRACKLEN
C  30/5/77
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	DIMENSION HPC(250), FN2C(250), ALPHA(250), BETA(250), GAMMA(250)
     &		  , DELTA(250), SLOPE(250), MAT(4, 5)
	COMMON /CONST / PI, PIT2, PID2, DEGS, RAD, K, DUM(2)
	COMMON /XX    / XDUM, PXPR, PXPTH, PXPPH, X, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (READFN, W(151))
	REAL*8 MAT, K
C
	IF(ICODE.EQ.0)THEN
	    MODX(2) = 'DOPPLER'
	    RETURN
	END IF
C
	IF(READFN.EQ.0.)GO TO 200
	READFN = 0.
	READ(15, 99001, END=100)NOC, (HPC(I), FN2C(I), I=1, NOC)
100	CONTINUE
	DO I = 1, NOC
	    FN2C(I) = FN2C(I)*(W(301)+1.)
	END DO
	WRITE(16, 99002)(HPC(I), FN2C(I), I=1, NOC)
99001	FORMAT(BZ, I4/(F8.2,E12.4))
99002	FORMAT('1', 14X, 'HEIGHT', 4X, 
     &	       '     DN/DT      '/(1X,F20.10,E20.10))
	A = 0.
	IF(FN2C(1).NE.0.)A = LOG(FN2C(2)/FN2C(1))/(HPC(2)-HPC(1))
	FN2C(1) = K*FN2C(1)
	FN2C(2) = K*FN2C(2)
	SLOPE(1) = A*FN2C(1)
	SLOPE(NOC) = 0.
	NMAX = 1
	DO I = 2, NOC
	    IF(FN2C(I).GT.FN2C(NMAX))NMAX = I
	    IF(I.NE.NOC)THEN
		FN2C(I+1) = K*FN2C(I+1)
		DO J = 1, 3
		    M = I + J - 2
		    MAT(J, 1) = 1.
		    MAT(J, 2) = HPC(M)
		    MAT(J, 3) = HPC(M)**2
		    MAT(J, 4) = FN2C(M)
		END DO
		CALL GAUSEL(MAT, 4, 3, 4, NRANK)
		IF(NRANK.LT.3)GO TO 300
		SLOPE(I) = MAT(2, 4) + 2.*MAT(3, 4)*HPC(I)
	    END IF
	    DO J = 1, 2
		M = I + J - 2
		MAT(J, 1) = 1.
		MAT(J, 2) = HPC(M)
		MAT(J, 3) = HPC(M)**2
		MAT(J, 4) = HPC(M)**3
		MAT(J, 5) = FN2C(M)
		L = J + 2
		MAT(L, 1) = 0.
		MAT(L, 2) = 1.
		MAT(L, 3) = 2.*HPC(M)
		MAT(L, 4) = 3.*HPC(M)**2
		MAT(L, 5) = SLOPE(M)
	    END DO
	    CALL GAUSEL(MAT, 4, 4, 5, NRANK)
	    IF(NRANK.LT.4)GO TO 300
	    ALPHA(I) = MAT(1, 5)
	    BETA(I) = MAT(2, 5)
	    GAMMA(I) = MAT(3, 5)
	    DELTA(I) = MAT(4, 5)
	END DO
	HMAX = MAX(HMAX, HPC(NMAX))
	NH = 2
200	CONTINUE
	H = R(1) - EARTHR
	F2 = F*F
	IF(H.LT.HPC(1))THEN
	    NH = 2
	    IF(FN2C(1).NE.0.)X = FN2C(1)*EXP(A*(H-HPC(1)))/F2
	ELSE IF(H.GE.HPC(NOC))THEN
	    X = FN2C(NOC)/F2
	ELSE
	    NSTEP = 1
	    IF(H.LT.HPC(NH-1))NSTEP = -1
250	    CONTINUE
	    IF(HPC(NH-1).LE.H .AND. H.LT.HPC(NH))THEN
		X = (ALPHA(NH)+H*(BETA(NH)+H*(GAMMA(NH)+H*DELTA(NH))))
     &		    /F2
	    ELSE
		NH = NH + NSTEP
		GO TO 250
	    END IF
	END IF
	RETURN
300	CONTINUE
	WRITE(16, 99003)I, HPC(I)
99003	FORMAT(' THE', I4, 
     &	       'TH POINT IN THE   DN/DT          PROFILE HAS T', 
     &	       'HE HEIGHT', F8.2, 
     &	       ' KM, WHICH IS THE SAME AS ANOTHER POINT.')
	CALL EXIT
	END
**==DTORUS.FOR
	SUBROUTINE DTORUS
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (C1, W(151)), (A1, W(152)), 
     &		    (B1, W(153)), (BETA, W(154)), (H1, W(155)), 
     &		    (C2, W(156)), (A2, W(157)), (B2, W(158)), 
     &		    (H2, W(159))
	REAL*8 LAMBD1, LAMBD2
	DATA PDPP/0.0/
C
	IF(ICODE.EQ.0)THEN
	    MODX(2) = 'DTORUS'
	    RETURN
	END IF
	IF(X.EQ.0. .AND. PXPR.EQ.0. .AND. PXPTH.EQ.0. .AND. PXPPH.EQ.0.)
     &	   RETURN
	IF(C1.EQ.0.)RETURN
	R1 = EARTHR + H1
	R2 = EARTHR + H2
	Z1 = R(1) - R1
	Z2 = R(1) - R2
	LAMBD1 = R1*(R(2)-PID2)
	LAMBD2 = R2*(R(2)-PID2+(H1-H2)/R2/TAN(BETA))
	SINBET = SIN(BETA)
	COSBET = COS(BETA)
	P1 = LAMBD1*COSBET + Z1*SINBET
	P2 = LAMBD2*COSBET + Z2*SINBET
	Y1 = Z1*COSBET - LAMBD1*SINBET
	Y2 = Z2*COSBET - LAMBD2*SINBET
	DELTA1 = C1*EXP(-(P1/A1)**2-(Y1/B1)**2)
	DELTA2 = C2*EXP(-(P2/A2)**2-(Y2/B2)**2)
	DEL1 = 1. + DELTA1 + DELTA2
	PDPR1 = -2.*DELTA1*(P1*SINBET/A1**2+Y1*COSBET/B1**2)
	PDPR2 = -2.*DELTA2*(P2*SINBET/A2**2+Y2*COSBET/B2**2)
	PDPT1 = -2.*DELTA1*(P1*R1*COSBET/A1**2-Y1*R1*SINBET/B1**2)
	PDPT2 = -2.*DELTA2*(P2*R2*COSBET/A2**2-Y2*R2*SINBET/B2**2)
	PXPR = PXPR*DEL1 + X*(PDPR1+PDPR2)
	PXPTH = PXPTH*DEL1 + X*(PDPT1+PDPT2)
	PXPPH = PXPPH*DEL1*PDPP
	X = X*DEL1
	RETURN
	END
**==ELECT1.FOR
	SUBROUTINE ELECT1
C
C ---- SELECTS ELECTRON DENSITY PERTURBATION MODEL
C
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
C
	IF(INDEXP.LT.0 .OR. INDEXP.GT.14)CALL PMDSTOP
	IF(INDEXP.EQ.0)THEN
	    CALL NOPERT
	ELSE IF(INDEXP.EQ.1)THEN
	    CALL TORUS
	ELSE IF(INDEXP.EQ.2)THEN
	    CALL DTORUS
	ELSE IF(INDEXP.EQ.3)THEN
	    CALL TROUGH
	ELSE IF(INDEXP.EQ.4)THEN
	    CALL SHOCK
	ELSE IF(INDEXP.EQ.5)THEN
	    CALL WAVE1
	ELSE IF(INDEXP.EQ.6)THEN
	    CALL WAVE2
	ELSE IF(INDEXP.EQ.7)THEN
	    CALL CLOUD
	ELSE IF(INDEXP.EQ.8)THEN
	    CALL MODTID
	ELSE IF(INDEXP.EQ.9)THEN
	    CALL DOPPLE
	ELSE IF(INDEXP.EQ.10)THEN
	    CALL BLOB
	ELSE IF(INDEXP.EQ.11)THEN
	    CALL BLOB2
	ELSE IF(INDEXP.EQ.12)THEN
	    CALL RING
	ELSE IF(INDEXP.EQ.13)THEN
	    CALL TROUGH2
	ELSE IF(INDEXP.EQ.14)THEN
		CALL BLOBFAI
	END IF
	RETURN
	END
**==ELECTX.FOR
	SUBROUTINE ELECTX
C
C ---- SELECTS THE REQUIRED ELECTRON DENSITY MODEL
C
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	IF(INDEXE.LT.1 .OR. INDEXE.GT.9)CALL PMDSTOP
	IF(INDEXE.EQ.1)THEN
	    CALL TABLEX
	ELSE IF(INDEXE.EQ.2)THEN
	    CALL CHAPX
	ELSE IF(INDEXE.EQ.3)THEN
	    CALL VCHAPX
	ELSE IF(INDEXE.EQ.4)THEN
	    CALL DCHAPT
	ELSE IF(INDEXE.EQ.5)THEN
	    CALL LINEAR
	ELSE IF(INDEXE.EQ.6)THEN
	    CALL QPARAB
	ELSE IF(INDEXE.EQ.7)THEN
	    CALL BULGE
	ELSE IF(INDEXE.EQ.8)THEN
	    CALL EXPX
	ELSE IF(INDEXE.EQ.9)THEN
	    CALL IRI
	END IF
	RETURN
	END
**==EXPX.FOR
	SUBROUTINE EXPX
C
C  EXPONENTIAL ELECTRON DENSITY MODEL
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DEGS, RAD, K, DUM(2)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	CHARACTER*10 ID(10)
	COMMON /XX2   / PX2PR
	COMMON /PERTOF/ISWITCH
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (N0, W(101)), (H0, W(102))
     &		    , (A, W(103)), (PERT, W(150))
	REAL*8 N, N0, K
C     DATA MODX(1)/'EXPX'/,HMAX/350./
	MODX(1) = 'EXPX'
	HMAX = 350.0
	H = R(1) - EARTHR
	Y = A*(H-H0)
	IF(Y.GE.-650.)THEN
	    N = N0*EXP(Y)
	    X = K*N/F**2
	    PXPR = A*X
	    PXPPH = 0.
	    PXPTH = 0.0
	    PX2PR = A*PXPR
	    IF(PERT.NE.0. .AND. ISWITCH.NE.0)CALL ELECT1
	    RETURN
	END IF
	PXPPH = 0.
	PXPTH = 0.0
	PX2PR = 0.0
	PXPR = 0.0
	X = 0.0
	RETURN
	END
**==EXPZ.FOR
	SUBROUTINE EXPZ
C
C  EXPONENTIAL COLLISION FREQUENCY MODEL
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /ZZ    / Z, PZPR, PZPTH, PZPPH
	COMMON /ZZC   / MODZ
	CHARACTER*10 MODZ
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	REAL*8 NU, NU0
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (NU0, W(251)), 
     &		    (H0, W(252)), (A, W(253))
C
	IF(ICODE.EQ.0)THEN
	    MODZ = ' EXPZ '
	    RETURN
	END IF
	H = R(1) - EARTHR
	NU = NU0/EXP(A*(H-H0))
	Z = NU/(PIT2*F*1.E6)
	PZPR = -A*Z
	RETURN
	END
**==EXPZ2.FOR
	SUBROUTINE EXPZ2
C
C  COLLISION FREQUENCY PROFILE FROM TWO EXPONENTIALS
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /ZZ    / Z, PZPR, PZPTH, PZPPH
	COMMON /ZZC   / MODZ
	CHARACTER*10 MODZ
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (NU1, W(251)), 
     &		    (H1, W(252)), (A1, W(253)), (NU2, W(254)), 
     &		    (H2, W(255)), (A2, W(256))
	REAL*8 NU1, NU2
C
	IF(ICODE.EQ.0)THEN
	    MODZ = ' EXPZ2'
	    RETURN
	END IF
C
	H = R(1) - EARTHR
	EXP1 = NU1*EXP(-A1*(H-H1))
	EXP2 = NU2*EXP(-A2*(H-H2))
	Z = (EXP1+EXP2)/(PIT2*F*1.E6)
	PZPR = (-A1*EXP1-A2*EXP2)/(PIT2*F*1.E6)

	RETURN
	END
**==FACTRS.FOR
	SUBROUTINE FACTRS
C
C  COMPUTES THE POLARIZATIONS OF THE WAVES AND PUTS THEM IN THE ARRAY AY
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /ATMOS / ALPHA, BETA, ETA, NU, BX, BY, BZ, GAMMA, SIGMA
	COMMON /INPT  / KX, OMEGA, INTGR(16), NLAYER
	COMMON /MATRIX/ AY(8, 6), A(6, 6), NWAVE
	COMMON /OUTPT / K(6), AP(6), ARB(16), ARBC(16)
	COMMON /AIR   / T, P, M, G, GAM, MU, LAMBDA, B, SIG
	COMMON /MISC  / ALF241, ALPHA2, BETA2, E1MB, E1PB, ETA3, ETA4, 
     &			ETA7, ETA11, ETA31, ETA41, ETA71, IALFD2, 
     &			IALPHA, IETALF, ISB, ISB1, ISB2, ISB3, ISBBY, 
     &			ISBXZ, SBBY2, SBBY2N, BXBY, BX2, BY2, BZ2, 
     &			GAMMA1
	COMMON /TRAV  / TOPATM, BOTATM, TOLMAX, TOLMIN, CURAMP(6, 2), 
     &			OLDAMP(6), ZBEG, ZEND, DEPTHO, Z, NEQNS
 
	COMPLEX*16 A, A11, A12, A21, A22, A33, ALF241, ALPHA, ALPHA2, 
     &		   AP, AT, AU, AV, AW, AY, BETA, BETA2, E1MB, E1PB, ETA, 
     &		   ETA3, ETA4, ETA7, ETA11, ETA31, ETA41, ETA71, ARBC, 
     &		   FRAC, IALFD2, IALPHA, IETALF, ISB, ISB1, ISB2, ISB3, 
     &		   ISBBY, ISBXZ, K, KAPPA, KMIALF, KX, NU, R, SBBY2, 
     &		   SBBY2N
 
	COMPLEX*16 AUXC1, AUXC2, AUXC3, AUXC4, AUXC5, AUXC6, AUXC7, 
     &		   AUXC8, AUXC9, AUXC10, AUXC11, DET, CURAMP, OLDAMP
 
	REAL*8 M, MU, LAMBDA
	EQUIVALENCE(INDEX, INTGR(1)), (NTEST, INTGR(3)), 
     &		    (MPRINT, INTGR(4)), (IEXTR, INTGR(6)), 
     &		    (KCHECK, INTGR(8)), (KEY, INTGR(10))
	DATA NN/0/
	DO J = 1, NEQNS
	    KAPPA = K(J)
	    KMIALF = KAPPA - IALPHA
	    R = 1. + KAPPA*KMIALF
	    IF(INDEX.NE.0)THEN
		FRAC = GAMMA1/(1.-NU*R*GAMMA1)
		AUXC1 = ETA3*R
		AUXC2 = E1MB
		AUXC3 = ETA41*R
		AUXC4 = -E1PB
		AUXC5 = (R-1.)*FRAC
		AUXC7 = (ETA11+FRAC)*KAPPA
		AUXC8 = -IALPHA*ETA31
		AUXC9 = ETA11*KAPPA
		AUXC10 = IETALF + IETALF
		AUXC11 = KMIALF*FRAC
		IF(INDEX.GE.2)THEN
		    AUXC2 = AUXC2 + ISB1
		    AUXC4 = AUXC4 + ISB3
		    AUXC8 = AUXC8 - ISBXZ
		    AUXC10 = AUXC10 - ISBXZ
		END IF
		A11 = AUXC1 + AUXC2 + FRAC
		A12 = AUXC7 + AUXC8
		A21 = AUXC9 + AUXC10 + AUXC11
		A22 = AUXC3 + AUXC4 + AUXC5
	    ELSE
		AUXC1 = R*GAMMA
		AUXC2 = -GAMMA - BETA
		AUXC3 = GAMMA*KAPPA
		AUXC4 = GAMMA1*KMIALF
		A11 = GAMMA - BETA
		A12 = AUXC3 - IALPHA
		A21 = KAPPA + AUXC4
		A22 = AUXC1 + AUXC2
		FRAC = GAMMA1
	    END IF
	    IF(NWAVE.GE.8)THEN
		AUXC6 = -BETA + ISB2
		A33 = AUXC1 + AUXC6
	    END IF
	    IF(NTEST.NE.0)THEN
		NN = NN + 1
		IF(NN.GT.NTEST)THEN
		    NN = 1
C
C  FOR SELECTED CASES, COMPUTE DETERMINANT AND CHECK TO SEE IF IT VANISHE
C
		    IF(NWAVE.GT.2)THEN
			DET = A11*A22 - A12*A21
			TEST1R = MAX(ABS(DREAL(AUXC1)), 
     &				 ABS(DREAL(AUXC2)), ABS(DREAL(FRAC)))
			TEST1I = MAX(ABS(DIMAG(AUXC1)), 
     &				 ABS(DIMAG(AUXC2)), ABS(DIMAG(FRAC)))
			TEST2R = MAX(ABS(DREAL(AUXC3)), 
     &				 ABS(DREAL(AUXC4)), ABS(DREAL(AUXC5)))
			TEST2I = MAX(ABS(DIMAG(AUXC3)), 
     &				 ABS(DIMAG(AUXC4)), ABS(DIMAG(AUXC5)))
			TEST3R = MAX(TEST1R*TEST2R, TEST1I*TEST2I)
			TEST3I = MAX(TEST1R*TEST2I, TEST1I*TEST2R)
			TEST1R = MAX(ABS(DREAL(AUXC7)), 
     &				 ABS(DREAL(AUXC8)))
			TEST1I = MAX(ABS(DIMAG(AUXC7)), 
     &				 ABS(DIMAG(AUXC8)))
			TEST2R = MAX(ABS(DREAL(AUXC9)), 
     &				 ABS(DREAL(AUXC10)), ABS(DREAL(AUXC11)))
			TEST2I = MAX(ABS(DIMAG(AUXC9)), 
     &				 ABS(DIMAG(AUXC10)), ABS(DIMAG(AUXC11)))
			TEST3R = MAX(TEST3R, TEST1R*TEST2R, 
     &				 TEST1I*TEST2I)
			TEST3I = MAX(TEST3I, TEST1R*TEST2I, 
     &				 TEST1I*TEST2R)
			IF(NWAVE.GT.6)THEN
			    DET = A33*DET + 
     &				  SBBY2*(BX*BZ*(A12+A21)-A11*BZ2-
     &				  A22*BX2)
			    TEST1R = MAX(ABS(DREAL(AUXC1)), 
     &				     ABS(DREAL(AUXC6)))
			    TEST1I = MAX(ABS(DIMAG(AUXC1)), 
     &				     ABS(DIMAG(AUXC6)))
			    TEST2R = MAX(TEST1R*TEST3R, TEST1I*TEST3I)
			    TEST2I = MAX(TEST1R*TEST3I, TEST1I*TEST3R)
			ELSE
			    TEST2R = TEST3R
			    TEST2I = TEST3I
			END IF
		    ELSE
			DET = A11*A22 - A12*A21
			TEST1R = MAX(ABS(DREAL(AUXC1)), 
     &				 ABS(DREAL(AUXC2)))
			TEST1I = MAX(ABS(DIMAG(AUXC1)), 
     &				 ABS(DIMAG(AUXC2)))
			TEST3R = ABS(DREAL(A11))
			TEST3I = ABS(DIMAG(A11))
			TEST2R = MAX(TEST1R*TEST3R, TEST1I*TEST3I)
			TEST2I = MAX(TEST1R*TEST3I, TEST1I*TEST3R)
			TEST1R = MAX(ABS(DREAL(AUXC3)), 
     &				 ABS(DREAL(IALPHA)))
			TEST1I = MAX(ABS(DIMAG(AUXC3)), 
     &				 ABS(DIMAG(IALPHA)))
			TEST3R = MAX(ABS(DREAL(KAPPA)), 
     &				 ABS(DREAL(AUXC4)))
			TEST3I = MAX(ABS(DIMAG(KAPPA)), 
     &				 ABS(DIMAG(AUXC4)))
			TEST2R = MAX(TEST2R, TEST1R*TEST3R, 
     &				 TEST1I*TEST3I)
			TEST2I = MAX(TEST2I, TEST1R*TEST3I, 
     &				 TEST1I*TEST3R)
		    END IF
C
C  THE LARGEST TERM IN THE EXPANSION OF THE DETERMINANT IS TAKEN TO BE
C  (TEST2R,TEST2I).
C
		    ERROR = ABS(DET)/MAX(TEST2R, TEST2I)
		    IF(ERROR.LE.3.E-3)THEN
		    END IF
		END IF
	    END IF
	    AU = -A22
	    AW = A21
	    IF(NWAVE.GE.8)THEN
		AU = BZ*A12 - BX*A22
		AW = BX*A21 - BZ*A11
		IF(J.GT.6)THEN
		    AV = (A21*A12-A11*A22)/ISBBY
		ELSE
		    AV = ISBBY/A33*(BXBY*(A12+A21)-BZ2*A11-BX2*A22)
		END IF
	    END IF
	    AT = T*(AU+KAPPA*AW)*FRAC
	    AP(J) = AU + KMIALF*AW + AT/T
	    AY(1, J) = AW
	    AY(2, J) = AP(J) + IALPHA*AW
	    IF(INDEX.GT.0)AY(2, J) = AY(2, J) + ETA*(KAPPA*AW*4.-AU-AU)
	    AY(3, J) = AU
	    AY(4, J) = AT
	    IF(NEQNS.NE.2)THEN
		AY(5, J) = ETA*(KAPPA*AU+AW)
		AY(6, J) = NU*KAPPA*AT/T
		IF(NEQNS.NE.6)THEN
		    AY(7, J) = AV
		    AY(8, J) = ETA*KAPPA*AV
		END IF
	    END IF
	END DO
	RETURN
	END
**==FGSW.FOR
	SUBROUTINE FGSW(X, F, DF, G, DG)
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMPLEX*16 F, DF, G, DG
	CALL FSW(X, F, DF)
	IF(ABS(X).LE.50.)THEN
	    G = X*F
	    DG = F + X*DF
	    RETURN
	END IF
	X2 = X*X
	X3 = X2*X
	T2 = 2.*X2
	T3 = 3.*X2
	T4 = 4.*X2
	T8 = 8.*X2
	T12 = 12.*X2
	T16 = 16.*X2
	G = DCMPLX(1.-35./T4*(1.-99./T4*(1.-195./T4*(1.-323./T4)))/T4, 
     &	    2.5*(1.-63./T4*(1.-143./T4*(1.-255./T4*(1.-399./T4))))/X)
	DG = .5*DCMPLX
     &	     (35.*(1.-99./T2*(1.-585./T8*(1.-323./T3*(1.-2415./T16))))
     &	     /X3, 
     &	     -5.*(1.-189./T4*(1.-715./T12*(1.-357./T4*(1.-513./T4))))
     &	     /X2)
	RETURN
	END
**==FSW.FOR
	SUBROUTINE FSW(Z, F, DF)
C
C  F(Z) = Z*C3/2(Z) + 2.5*I*C5/2(Z)  AND  DF(Z) = DF/DZ
C  WHERE THE INPUT Z IS REAL AND THE OUTPUT F AND DF ARE COMPLEX.
C  NEEDS THE SUBPROGRAMS FOR THE FRESNEL INTEGRAL FUNCTIONS S AND C
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	DIMENSION A(10), B(10), D(10)
	COMPLEX*16 F, DF, C1, C2, C3, C8, W, TEMP, I
	DATA I/(0., 1.)/, PI/3.1415926536/, A3/1.333333333/
	DATA C2/(1., 1.)/, C3/(1., -1.)/, C4/.79788456/, C6/1.999999999/
C
C  C4=SQRT(2./PI)
C
	DATA A/.36230845E-02, .29579186E+00, .23193588E+01, 
     &	     .91355870E+01, .25856287E+02, .60488560E+02, .12562218E+03, 
     &	     .24214980E+03, .44918106E+03, .84244774E+03/, 
     &	     B/.16747479E-02, .84796280E-01, .25285001E+00, 
     &	     .22665867E+00, .83871933E-01, .13811875E-01, .98017417E-03, 
     &	     .26299148E-04, .19761006E-06, .18781476E-09/, 
     &	     D/.10080653E-03, .46117941E-01, .38507643E+00, 
     &	     .68507885E+00, .42648105E+00, .10742102E+00, .10985920E-01, 
     &	     .40924533E-03, .41881263E-05, .54513142E-08/, G/1.5045055/
	C1 = 2./3.*I
	C8 = C2*A3*SQRT(PI/2.)
	X = Z
	X2 = X*X
	X3 = X2*X
	IF(ABS(X).LE.50.)THEN
	    IF(ABS(X).GT.6.)THEN
C
C  HERMITE
C
		XQ = X**2
		X2 = XQ
		FR = 0.
		FI = 0.
		DFR = 0.
		DFI = 0.
		DO J = 1, 10
		    SS = A(J) + XQ
		    SB = B(J)/SS
		    SD = D(J)/SS
		    FR = FR + SB
		    FI = FI + SD
		    DFR = DFR + SB/SS
		    DFI = DFI + SD/SS
		END DO
		F = DCMPLX(X*FR, FI)*G
		DF = G*(FR-2.*X*DCMPLX(X*DFR,DFI))
		RETURN
	    ELSE IF(ABS(X).LT..05)THEN
C
C  POWER SERIES
C
		X = ABS(Z)
		X2 = X*X
		X3 = X2*X
		X4 = X*X3
		X5 = X*X4
		TEMP = -C8*SQRT(X)*EXP(I*X)
		ACON = 4./3.
		BCON = 16./9.
		CCON = 64./315.
		DCON = 2./3.
		ECON = 8./3.
		FCON = 32./45.
		F = DCMPLX(ACON*X-BCON*X3+CCON*X5, DCON+ECON*X2-FCON*X4)
     &		     + TEMP*X
		DF = DCMPLX(4./3.-16./3.*X2+64./63.*X4, 
     &		     16./3.*X-128./45.*X3+256./945.*X5)
     &		      + TEMP*DCMPLX(1.5D0, X)
		IF(Z.GE.0.)RETURN
		F = -CONJG(F)
		DF = CONJG(DF)
		RETURN
C
C  FRESNEL
C
	    ELSE IF(X.GT.0.)THEN
		Y = C4*SQRT(X)
		X2 = X*X
		W = (COS(X)+I*SIN(X))*(1.-C2*(C(Y)-I*S(Y)))
		F = C1 + C6*(X-C2*X*X/Y*W)
		DF = A3*DCMPLX(1.D0, X) - DCMPLX(1.5D0, X)*A3*C2*X/Y*W
		RETURN
	    ELSE
		Y = C4*SQRT(-X)
		X2 = X*X
		W = (COS(X)+I*SIN(X))*(1.-C3*(C(Y)+I*S(Y)))
		F = C1 + C6*(X+C3*X*X/Y*W)
		DF = A3*DCMPLX(1.D0, X) + DCMPLX(1.5D0, X)*A3*C3*X/Y*W
		RETURN
	    END IF
	END IF
C
C  ASYMPTOTIC
C
	X2 = X*X
	X3 = X2*X
	X4 = X3*X
	X5 = X4*X
	T2 = 2.*X2
	T3 = 3.*X2
	T4 = 4.*X2
	T8 = 8.*X2
	T16 = 16.*X2
	T28 = 28.*X2
	F = DCMPLX((1.-35./T4*(1.-99./T4*(1.-195./T4*(1.-323./T4))))/X, 
     &	    5.*(1.-63./T4*(1.-143./T4*(1.-255./T4*(1.-399./T4))))/T2)
	DF = -
     &	     DCMPLX((1.-105./T4*(1.-165./T4*(1.-273./T4*(1.-2907./T28)))
     &	     )/X2, 
     &	     5.*(1.-63./T2*(1.-429./T8*(1.-255./T3*(1.-1995./T16))))/X3)
	RETURN
	END
**==GAUSEL.FOR
	SUBROUTINE GAUSEL(C, NRD, NRR, NCC, NSF)
C
C  ********* SAME AS SUBROUTINE GAUSSEL WRITTEN BY L. DAVID LEWIS ********
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	DIMENSION C(NRD, NCC), L(128, 2)
C
C  BITS = 2.**-18
C
	DATA BITS/3.8146972656E-6/
	NR = NRR
	NC = NCC
	IF(NC.LT.NR .OR. NR.GT.128 .OR. NR.LE.0)CALL EXIT
C
C
C  INITIALIZE.
C
	NSF = 0
	NRM = NR - 1
	NRP = NR + 1
	D = 1.
	LSD = 1
	DO KR = 1, NR
	    L(KR, 1) = KR
	    L(KR, 2) = 0
	END DO
	IF(NR.NE.1)THEN
C
C
C  ELIMINATION PHASE.
C
	    DO KP = 1, NRM
		KPP = KP + 1
		PM = 0.
		MPN = 0
C
C
C  SEARCH COLUMN KP FROM DIAGONAL DOWN FOR MAX PIVOT.
C
		DO KR = KP, NR
		    LKR = L(KR, 1)
		    PT = ABS(C(LKR,KP))
		    IF(PT.GT.PM)THEN
			PM = PT
			MPN = KR
			LMP = LKR
		    END IF
		END DO
C
C
C  IF MAX PIVOT IS ZERO, MATRIX IS SINGULAR.
C
		IF(MPN.EQ.0)GO TO 100
		NSF = NSF + 1
		IF(MPN.NE.KP)THEN
C
C
C  NEW ROW NUMBER KP HAS MAX PIVOT.
C
		    LSD = -LSD
		    L(MPN, 1) = L(KP, 1)
		    L(KP, 2) = L(MPN, 1)
		    L(KP, 1) = LMP
		END IF
C
C
C  ROW OPERATIONS TO ZERO COLUMN KP BELOW DIAGONAL.
C
		MKP = L(KP, 1)
		P = C(MKP, KP)
		D = D*P
		DO KR = KPP, NR
		    MKR = L(KR, 1)
		    Q = C(MKR, KP)/P
		    IF(Q.NE.0.)THEN
C
C
C  SUBTRACT Q * PIVOT ROW FROM ROW KR.
C
			DO LC = KPP, NC
			    R = Q*C(MKP, LC)
			    C(MKR, LC) = C(MKR, LC) - R
			    IF(ABS(C(MKR,LC)).LT.ABS(R)*BITS)C(MKR, LC)
     &				= 0.
			END DO
		    END IF
		END DO
	    END DO
	END IF
C
C
C  LOWER RIGHT HAND CORNER.
C
	LNR = L(NR, 1)
	P = C(LNR, NR)
	IF(P.NE.0.)THEN
	    NSF = NSF + 1
	    D = D*P*LSD
	    IF(NR.NE.NC)THEN
C
C
C  BACK SOLUTION PHASE.
C
		DO MC = NRP, NC
		    C(LNR, MC) = C(LNR, MC)/P
		    IF(NR.NE.1)THEN
			DO LL = 1, NRM
			    KR = NR - LL
			    MR = L(KR, 1)
			    KRP = KR + 1
			    DO MS = KRP, NR
				LMS = L(MS, 1)
				R = C(MR, MS)*C(LMS, MC)
				C(MR, MC) = C(MR, MC) - R
				IF(ABS(C(MR,MC)).LT.ABS(R)*BITS)
     &				   C(MR, MC) = 0.
			    END DO
			    C(MR, MC) = C(MR, MC)/C(MR, KR)
			END DO
		    END IF
		END DO
C
C
C  SHUFFLE SOLUTION ROWS BACK TO NATURAL ORDER.
C
		DO LL = 1, NRM
		    KR = NR - LL
		    MKR = L(KR, 2)
		    IF(MKR.NE.0)THEN
			MKP = L(KR, 1)
			DO LC = NRP, NC
			    Q = C(MKR, LC)
			    C(MKR, LC) = C(MKP, LC)
			    C(MKP, LC) = Q
			END DO
		    END IF
		END DO
	    END IF
C
C
C  NORMAL AND SINGULAR RETURNS.  GOOD SOLUTION COULD HAVE D=0.
C
	    C(1, 1) = D
	    GO TO 200
	END IF
100	CONTINUE
	C(1, 1) = 0.
200	CONTINUE
	RETURN
	END
**==GOTOER.FOR
	SUBROUTINE GOTOER
C
C     ROUTINE CALLED ON FAILURE OF A COMPUTED GOTO
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	PRINT *, 'HELP COMPUTED GOTO FAILURE !!!'
	STOP
	END
**==HARMON.FOR
	SUBROUTINE HARMON
C
C  MODEL OF THE EARTH S MAGNETIC FIELD BASED ON A HARMONIC ANALYSIS
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	DIMENSION PHPTH(7, 7), PGPTH(7, 7), A1(7, 7), B1(7, 7)
	DIMENSION H(7, 7), G(7, 7), GG(7, 7), HH(7, 7), SINP(7), COSP(7)
	COMMON /YY    / Y, PYPR, PYPTH, PYPPH, YR, PYRPR, PYRPT, PYRPP, 
     &			YTH, PYTPR, PYTPT, PYTPP, YPH, PYPPR, PYPPT, 
     &			PYPPP
	COMMON /YYC   / MODY
	CHARACTER*10 MODY
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	CHARACTER*10 ID(10)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	EQUIVALENCE(THETA, R(2)), (PHI, R(3))
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (READFH, W(200))
C
C  RATIO OF CHARGE TO MASS FOR ELECTRON
C
	DATA EOM/1.7589E7/
	DATA SET/0./, H/1., 48*0./, G/49*0./, PHPTH/49*0./, PGPTH/49*0./
C
	IF(ICODE.EQ.0)THEN
	    MODY = 'HARMONY'
	    RETURN
	END IF
	IF(SET.NE.1.0)THEN
	    DO M = 1, 7
		DO N = 1, 7
		    B1(M, N) = (N+M-1)*(N-M+1)/(2*N-1.)
		    A1(M, N) = B1(M, N)/(2*N+1)
		END DO
	    END DO
	    SET = 1.
	END IF
	IF(READFH.EQ.0.)GO TO 200
	READ(15, 99001, END=100)GG, HH
99001	FORMAT(BZ, 1X, F9.4, 6F10.4)
100	CONTINUE
	WRITE(16, 99002)GG
99002	FORMAT('1', 10X, '0', 14X, '1', 14X, '2', 14X, '3', 14X, '4', 
     &	       14X, '5', 14X, '6'/9X, 7('G',14X)/10X, 7('N',14X)
     &	       //(1X,7F15.6))
	WRITE(16, 99003)HH
99003	FORMAT(//11X, '0', 14X, '1', 14X, '2', 14X, '3', 14X, '4', 14X, 
     &	       '5', 14X, '6'/9X, 7('H',14X)/10X, 7('N',14X)//(1X,7F15.6)
     &	       )
	READFH = 0.
200	CONTINUE
	COSTHE = COS(THETA)
	SINTHE = SIN(THETA)
	AOR = EARTHR/R(1)
	PAORPR = -AOR/R(1)
	CNST2 = AOR
	PCNSPR = PAORPR
	PFIN1P = 0.
	PFIN1T = 0.0
	PFIN1R = 0.0
	FIN1 = 0.0
	PFIN2P = 0.0
	PFIN2T = 0.0
	PFIN2R = 0.0
	FIN2 = 0.0
	PFIN3P = 0.0
	PFIN3T = 0.0
	PFIN3R = 0.0
	FIN3 = 0.0
	DO M = 1, 7
	    SINP(M) = SIN((M-1)*PHI)
	    COSP(M) = COS((M-1)*PHI)
	END DO
	H(1, 2) = COSTHE
	H(2, 2) = SINTHE
	DO M = 1, 5
	    H(M+1, M+2) = COSTHE*H(M+1, M+1)
	    H(M+2, M+2) = SINTHE*H(M+1, M+1)
	    DO N = M, 5
		H(M, N+2) = COSTHE*H(M, N+1) - A1(M, N)*H(M, N)
	    END DO
	END DO
	DO M = 1, 6
	    G(M+1, M+1) = -M*COSTHE*H(M+1, M+1)
	    PHPTH(M+1, M+1) = -G(M+1, M+1)/SINTHE
	    PGPTH(M+1, M+1) = M*SINTHE*H(M+1, M+1)
     &			       - M*COSTHE*PHPTH(M+1, M+1)
	    DO N = M, 6
		G(M, N+1) = -N*COSTHE*H(M, N+1) + B1(M, N)*H(M, N)
		PHPTH(M, N+1) = -G(M, N+1)/SINTHE
		PGPTH(M, N+1) = N*SINTHE*H(M, N+1)
     &				 - N*COSTHE*PHPTH(M, N+1) + B1(M, N)
     &				*PHPTH(M, N)
	    END DO
	END DO
	DO N = 1, 7
	    PCRPHH = 0.0
	    PCRPTH = 0.0
	    CR = 0.0
	    PCTHPP = 0.0
	    PCTHPT = 0.0
	    CTH = 0.0
	    PCPHPP = 0.0
	    PCPHPT = 0.0
	    CPH = 0.0
	    DO M = 1, N
		TEMP1 = GG(M, N)*COSP(M) + HH(M, N)*SINP(M)
		TEMP2 = (M-1)*(HH(M,N)*COSP(M)-GG(M,N)*SINP(M))
		CR = CR + H(M, N)*TEMP1
		PCRPTH = PCRPTH + PHPTH(M, N)*TEMP1
		PCRPPH = PCRPPH + H(M, N)*TEMP2
		CTH = CTH + G(M, N)*TEMP1
		PCTHPT = PCTHPT + PGPTH(M, N)*TEMP1
		PCTHPP = PCTHPP + G(M, N)*TEMP2
		CPH = CPH + H(M, N)*TEMP2
		PCPHPT = PCPHPT + PHPTH(M, N)*TEMP2
		PCPHPP = PCPHPP - H(M, N)*(M-1)**2*TEMP1
	    END DO
	    CNST2 = CNST2*AOR
	    PCNSPR = CNST2*PAORPR + AOR*PCNSPR
	    FIN1 = FIN1 + N*CNST2*CR
	    PFIN1R = PFIN1R + N*PCNSPR*CR
	    PFIN1T = PFIN1T + N*CNST2*PCRPTH
	    PFIN1P = PFIN1P + N*CNST2*PCRPPH
	    FIN2 = FIN2 + CNST2*CTH
	    PFIN2R = PFIN2R + PCNSPR*CTH
	    PFIN2T = PFIN2T + CNST2*PCTHPT
	    PFIN2P = PFIN2P + CNST2*PCTHPP
	    FIN3 = FIN3 + CNST2*CPH
	    PFIN3R = PFIN3R + PCNSPR*CPH
	    PFIN3T = PFIN3T + CNST2*PCPHPT
	    PFIN3P = PFIN3P + CNST2*PCPHPP
	END DO
	HTHETA = -FIN2/SINTHE
	HPHI = FIN3/SINTHE
C
C  ********* CONVERT FROM MAG FIELD IN GAUSS TO GYROFREQ IN MHZ
C
	CONST = -EOM/PIT2*1.E-6/F
	YR = -CONST*FIN1
	YTH = CONST*HTHETA
	YPH = CONST*HPHI
	Y = SQRT(YR**2+YTH**2+YPH**2)
	PYRPR = -CONST*PFIN1R
	PYTPR = -CONST*PFIN2R/SINTHE
	PYPPR = CONST*PFIN3R/SINTHE
	PYPR = (YR*PYRPR+YTH*PYTPR+YPH*PYPPR)/Y
	PYRPT = -CONST*PFIN1T
	PYTPT = -CONST*(PFIN2T/SINTHE+HTHETA*COSTHE/SINTHE)
	PYPPT = CONST*(PFIN3T/SINTHE-HPHI*COSTHE/SINTHE)
	PYPTH = (YR*PYRPT+YTH*PYTPT+YPH*PYPPT)/Y
	PYRPP = -CONST*PFIN1P
	PYTPP = -CONST*PFIN2P/SINTHE
	PYPPP = CONST*PFIN3P/SINTHE
	PYPPH = (YR*PYRPP+YTH*PYTPP+YPH*PYPPP)/Y
	RETURN
C
C  COEFFICIENTS IN GAUSSIAN UNITS FROM H. S. JONES AND P. F. MELOTTE
C  (THE HARMONIC ANALYSIS OF THE EARTH S MAGNETIC FIELD FOR EPOCH 1942,
C  MONTHLY NOTICES OF THE ROYAL ASTRONOMICAL SOCIETY, GEOPHYSICAL SUPP.
C  VOL. 6, P 409, 1953)
C  THE FOLLOWING 14 CARDS CAN BE USED AS DATA CARDS FOR THIS SUBROUTINE
C  0.
C  .3039    .0218
C  .0176    -.0509    -.0135
C  -.0255    .0515    -.0236    -.0074
C  -.0398   -.0397    -.0238     .0087    -.0018
C  .0293    -.0329    -.0130     .0031     .0034     .0005
C  -.0211   -.0073    -.0007     .0210     .0017    -.0004     .0006
C  0.
C  -.0555
C  .0260    -.0044
C  .0190    -.0033    -.0001
C  -.0139     .0076     .0019     .0010
C  .0057    -.0018     .0009     .0032    -.0004
C  -.0026    -.0204     .0018     .0009     .0004     .0002
C
	END


! 	*********************************************************************************
! 	*********************************************************************************
**==HASEL.FOR
! 	*********************************************************************************
! 	*********************************************************************************
	SUBROUTINE HASEL
C
C  EVALUATES THE DIFFERENTIAL EQS. USED IN RAY TRACING
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DEGS, RAD, K, C, LOGTEN
	COMMON /RIN   / COLL, FIELD, SPITZ, N2, N2I, PNPR, PNPRI, PNPTH, 
     &			PNPTHI, PNPPH, PNPPHI, PNPVR, PNPVRI, PNPVTH, 
     &			PNPVTI, PNPVPH, PNPVPI, NNP, NNPI, PNPT, PNPTI, 
     &			GAM, GAMI, PGPR, PGPRI, PGPT, PGPTI, PGPP, 
     &			PGPPI, PGPVR, PGPVRI, PGPVT, PGPVTI, PGPVP, 
     &			PGPVPI, POLAR, LPOLAR, FACTR, SPACE
	COMMON /RINC  / MODRIN
	CHARACTER*10 MODRIN(3)
	COMMON R(20), T, STP, DRDT(20)/WW/W0, W(400)
	COMMON /WWC   / ID
	CHARACTER*10 ID(10)
	EQUIVALENCE(TH, R(2)), (PH, R(3)), (VR, R(4)), (VTH, R(5)), 
     &		    (VPH, R(6)), (DTHDT, DRDT(2)), (DPHDT, DRDT(3)), 
     &		    (DVRDT, DRDT(4)), (DVTHDT, DRDT(5)), 
     &		    (DVPHDT, DRDT(6))
	EQUIVALENCE(F, W(6))
	REAL*8 N2, N2I, NNP, NNPI, LOGTEN, K
	COMPLEX*16 POLAR, LPOLAR
	LOGICAL*4 SPACE
	FACTR = 1.0
	CALL RINDEX
	STH = SIN(TH)
	CTH = SIN(PID2-TH)
	DRDT(1) = (FACTR*VR-PNPVR)/NNP
	DTHDT = (FACTR*VTH-PNPVTH)/(NNP*R(1))
	DPHDT = (FACTR*VPH-PNPVPH)/(NNP*R(1)*STH)
	DVRDT = PNPR/NNP + DTHDT*VTH + DPHDT*VPH*STH
	DVTHDT = (PNPTH/NNP-DRDT(1)*VTH+R(1)*DPHDT*VPH*CTH)/R(1)
	DVPHDT = (PNPPH/NNP-DRDT(1)*STH*VPH-R(1)*CTH*DTHDT*VPH)
     &		 /(R(1)*STH)
	NR = 6
C
C  ********* PHASE PATH
C
	IF(W(57).NE.0.)THEN
	    NR = NR + 1
	    DRDT(NR) = FACTR*N2/NNP
	END IF
C
C  ********* ABSORPTION
C
	IF(W(58).NE.0.)THEN
	    NR = NR + 1
	    DRDT(NR) = FACTR*1.E7*PIT2*F*ABS(N2I)/(LOGTEN*C*NNP)
	END IF
C
C  ********* DOPPLER SHIFT
C
	IF(W(59).NE.0.)THEN
	    NR = NR + 1
	    DRDT(NR) = -1.E6*F*PNPT/(C*NNP)
	END IF
C
C  ********* GAMMA
C
	IF(W(60).NE.0.)THEN
	    NR = NR + 1
	    DRDT(NR) = PGPR*DRDT(1) + PGPT*DTHDT + PGPP*DPHDT + 
     &		       PGPVR*DVRDT + PGPVT*DVTHDT + PGPVP*DVPHDT
	END IF
C
C  ********* GAMMA (IMAG)
C
	IF(W(61).NE.0.)THEN
	    NR = NR + 1
	    DRDT(NR) = PGPRI*DRDT(1) + PGPTI*DTHDT + PGPPI*DPHDT + 
     &		       PGPVRI*DVRDT + PGPVTI*DVTHDT + PGPVPI*DVPHDT
	END IF
C
C  ********* GEOMETRICAL PATH LENGTH
C
	IF(W(62).NE.0.)THEN
	    NR = NR + 1
	    DRDT(NR) = SQRT(DRDT(1)**2+R(1)**2*(DTHDT**2+(STH*DPHDT)**2)
     &		       )
	END IF
C
C  ********* OTHER CALCULATIONS
C
	RETURN
	END
**==LABPLT.FOR
	SUBROUTINE LABPLT
C
C  *** LABELS THE CURRENT PLOT***
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	CHARACTER*10 TYPE(3)
	CHARACTER IHEAD*85, LABEL*85
	COMMON /CONST / PI, PIT2, PID2, DEGS, DUM(4)
	COMMON /FLG   / NTYP, NEWWR, NEWWP, PENET, LINES, IHOP, HPUNCH
	COMMON /WW    / W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (AZ1, W(10)), (PLT, W(81))
     &		    , (FACTR, W(82)), (TIC, W(87))
	LOGICAL PENET, NEWWR, NEWWP
	REAL*8 LTIC
	DATA TYPE/'EXTRAORDRY', 'NO FIELD  ', ' ORDINARY '/

	IF(W(303).EQ.0.0)THEN 
	WRITE(IHEAD, 99001)ID
99001	FORMAT(10A8)
c	CALL CTRMAG(15)
 
	IF(PLT.EQ.2.)THEN
c	    CALL PLACE(1, 1)
c	    CALL CTRMAG(12)  ! smaller text so RING parameters plotted
	ELSE IF(PLT.EQ.1.)THEN
c	    CALL PLACE(1, 35)
	ELSE IF(PLT.EQ.3.)THEN
c	    CALL PLACE(1, 3)
	END IF
 
c	CALL TYPECS(IHEAD)
	AZA = AZ1*DEGS
	DTIC = TIC*EARTHR
 
	IF(PLT.EQ.3.)THEN
 	    WRITE(LABEL,99004)F,AZA,TYPE(NTYP)
99004       FORMAT('Frequency is',F6.2,'MHz,   Azimuth is ',F6.2,
     &         ',  Mode is ',A10)
c	    CALL PLACE (1,35)
c	    CALL TYPECS(LABEL)
	    WRITE(LABEL,99005)DTIC
99005       FORMAT(F6.2,'km between Tick Marks along earth surface')
c	    CALL CRLNFD
c	    CALL TYPECS(LABEL)
	ELSE
C for options 1, 2 ie as original
	    WRITE(LABEL, 99002)F, AZA, TYPE(NTYP), DTIC
99002	    FORMAT('F =', F9.4, ', AZ= ', F9.4, ', ', A10, F9.4, 
     &	       ' KM BETWEEN TICK', 'MARKS  ')
c	    CALL CRLNFD
c	    CALL TYPECS(LABEL)
	END IF
C for horizontal plot only
 	IF(PLT.EQ.2.)THEN
	    LTIC = DTIC/FACTR
c	    CALL CRLNFD
 
	    WRITE(LABEL, 99003)LTIC
99003	    FORMAT(F8.4, ' KM BETWEEN TICK MARKS VERTICAL')
c	    CALL SPACE(15)
c	    CALL TYPECS(LABEL)
	END IF

* if a ring perturbation has been chosen, write out parameters
	IF(INDEXP.EQ.12 .AND. W(150).NE.0.0 .AND. W(151).NE.0.0)THEN
	    WRITE(LABEL,'(3(A,F6.2),2(A,F6.1),A)')'RING, POS=',
     &		W(152)*180.0/PI,'N,',W(153)*180.0/PI,'E, I=',W(151)*100
     &		,'%, h=',W(155),'km, r=',W(154),'km'
c	    CALL CRLNFD
c	    CALL TYPECS(LABEL)
	    WRITE(LABEL,'(2(A,F6.1),A)')'Lh=',W(156),', Lv=',W(157),
     &		'km'
c	    CALL CRLNFD
c	    CALL SPACE(6)
c	    CALL TYPECS(LABEL)
	ELSE IF(INDEXP.EQ.5 .AND. W(150).NE.0.0 .AND. W(153).NE.0.0)THEN
* if a wave perturbation has been chosen then write out parameters
	    WRITE(LABEL,'(4(A,F6.1),A)')'WAVE, Z=',W(151),'KM, H=',W(152),
     &          'KM, I=',W(153)*100,'%, VX=',W(154),'KM/SEC'
c            CALL CRLNFD
c            CALL TYPECS(LABEL)
            WRITE(LABEL,'(3(A,F6.1),A)')'Lamda-X=',W(155),
     &          ' km, Lambda-Z=',W(156),' km, Time=',W(157),
     &          ' Wave-Periods'
c            CALL CRLNFD
c            CALL SPACE(6)
c            CALL TYPECS(LABEL)
	ENDIF
	ENDIF  
	RETURN
	END
**==LINEAR.FOR
	SUBROUTINE LINEAR
C
C  LINEAR ELECTRON DENSITY MODEL
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DEGS, RAD, K, DUM(2)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /XX2   / PX2PR
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /PERTOF/ISWITCH
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (FACT, W(101)), 
     &		    (HM, W(102)), (HMIN, W(103)), (PERT, W(150))
	REAL*8 K
C
	IF(ICODE.EQ.0)THEN
	    MODX(1) = 'LINEAR'
	    RETURN
	END IF
	H = R(1) - EARTHR
	HMAX = HM
	PXPR = 0.
	X = 0.0
	PXPPH = 0.
	PXPTH = 0.0
	PX2PR = 0.
	IF(H.GT.HMIN)THEN
	    PXPR = K*FACT/F**2
	    X = PXPR*(H-HMIN)
	    PX2PR = 0.
	END IF
	IF(PERT.NE.0. .AND. ISWITCH.NE.0)CALL ELECT1
	RETURN
	END
**==MAGY.FOR
	SUBROUTINE MAGY
C
C ---- SELECTS THE REQUIRED MAGNETIC FIELD MODEL
C
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
C
	IF(INDEXM.LT.1 .OR. INDEXM.GT.4)CALL PMDSTOP
	IF(INDEXM.EQ.1)THEN
	    CALL CONSTY
	ELSE IF(INDEXM.EQ.2)THEN
	    CALL DIPOLY
	ELSE IF(INDEXM.EQ.3)THEN
	    CALL CUBEY
	ELSE IF(INDEXM.EQ.4)THEN
	    CALL HARMON
	END IF
	RETURN
	END
**==MODTID.FOR
	SUBROUTINE MODTID
C
C  THE OUTPUTS ARE THE ELECTRON DENSITY (NE) AND ITS GRADIENT (GRADNE) FO
C  AMBIENT TIME-INDEPENDENT HORIZONTALLY STRATIFIED IONOSPHERE (SPECIFIED
C  SUBROUTINE ELDENS) PERTURBED BY A TID.  THE INPUTS ARE THE SPHERICAL P
C  COORDINATES R, THETA, AND PHI WHICH SPECIFY THE POSITION OF THE FIELD
C  AT WHICH THE VALUES OF NE AND GRADNE ARE DESIRED. THESE SPHERICAL COOR
C  COINCIDE WITH GEOMAGNETIC COORDINATES (EXCEPT THETA IS THE CO-LATITUDE
C  IN ADDITION, IF INIT > 0, NEW TID PARAMETERS ARE READ IN AND CERTAIN
C  INITIALIZATIONS PERFORMED. THE TID IS SPECIFIED BY ITS PERIOD (IN MINU
C  ITS HORIZONTAL PHASE SPEED (SPEED, IN M/SEC), ITS AZIMUTH OF PROPAGATI
C  (AZ, IN DEGREES E OF N), AN ARBITRARY ADDITIVE PHASE (PHASE, IN DEGREE
C  AND ITS AMPLITUDE (AMP, IN M/SEC), DEFINED TO BE THE MAXIMUM NEUTRAL V
C  PERTURBATION OF THE UNDERLYING GRAVITY WAVE. IF ANY OF THESE PARAMETER
C  NOT PROVIDED, DEFAULT VALUES CORRESPONDING TO A "TYPICAL" MEDIUM-SCALE
C  ARE ASSUMED (PERIOD = 30, SPEED =200, AMP = 10, AZ = 0., PHASE =0).
C  THE RADAR LATITUDE AND LONGITUDE AND THE RADIUS OF THE EARTH MUST ALSO
C  PROVIDED THROUGH COMMON/RDRLOC/RLAT,RLON,RE. RLAT, RLON, THETA, AND PH
C  RADIANS, R AND RE ARE IN KILOMETERS, AND NE IS IN THE UNITS USED IN
C  SUBROUTINE ELDENS (PROVIDED BY THE USER). THE TIME-DEPENDENCE OF THE T
C  IS IGNORED (SINCE TID PERIODS ARE MUCH LONGER THAN THE TIME FOR A RADA
C  TO PASS THROUGH THE IONOSPHERE). THE SPEED OF THE TID IS RESTRICTED TO
C  BE LESS THAN 300 M/SEC (ROUGHLY THE SPEED OF SOUND AT THE EARTH'S SURF
C  AND ITS PERIOD TO BE GREATER THAN 6 MINUTES (ROUGHLY THE BRUNT PERIOD
C  THE EARTH'S SURFACE).
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
C KSH 25.1.91 SUN compile error : external name WAVE already used
c therefore I have changed the name of the common block to WAVECB
	COMMON R(6)/WW/W0, V(400)/WAVECB/U(250), W(250)/XX2/PX2PR
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(RE, V(2)), (F, V(6)), (READFN, V(150)), 
     &		    (RLAT, V(151)), (RLON, V(152)), (PERIOD, V(153)), 
     &		    (SPEED, V(154)), (AZ, V(155)), (AMP, V(156)), 
     &		    (PHASE, V(157))
	REAL*8 LAMBDA, KX, NN2, NEO, NUE, NE
c	COMPLEX*16 AUXC3
c KSH 25.1.91 sun compile warning :local variable AUXC3 not used
	COMPLEX*16 I, U, W, KZ, AUXC1, AUXC2, UE, UEJP1, UEJ
	DIMENSION GRADNE(3)
	DATA CNVRT, TWOPI, ALPH, GAM, I/.01745329, 6.2831853, 1.E-7, 
     &	     2.E-12, (0., 1.)/
C
	IF(ICODE.EQ.0)THEN
	    MODX(2) = 'MODTID'
	    RETURN
	END IF
	IF(READFN.NE.2.)THEN
	    READFN = 2.
	    CNV = 8.0613825E-5/F**2
	    LAMBDA = PERIOD*SPEED*.06
	    KX = TWOPI/LAMBDA
	    OMEGA = TWOPI/(PERIOD*60.)
	    SINAZ = SIN(AZ*CNVRT)
	    COSAZ = COS(AZ*CNVRT)
C
C  SUBROUTINE AGWAVE SOLVES FOR THE HORIZONTAL AND VERTICAL
C  VELOCITY PERTURBATIONS OF THE GRAVITY WAVE AND PUTS THEM
C  INTO THE COMPLEX ARRAYS U AND W. FOR EXAMPLE, U(J) IS THE
C  HORIZONTAL VELOCITY PERTURBATION AT ALTITUDE 2*(J-1) KM.
C
	    CALL AGWAVE(PERIOD, SPEED)
C
C  THE FOLLOWING STATEMENTS NORMALIZE THE GRAVITY WAVE SO IS
C  MAXIMUM NEUTRAL VELOCITY PERTURBATION IS AMP.
C
	    VSQMAX = 0.
	    DO J = 1, 250
		VSQ = ABS(U(J))**2 + ABS(W(J))**2
		IF(VSQ.GT.VSQMAX)VSQMAX = VSQ
	    END DO
	    AUX = AMP/SQRT(VSQMAX)/1000.
	    DO J = 1, 250
		U(J) = AUX*U(J)
		W(J) = AUX*W(J)
	    END DO
C
C  COMPUTE THE COORDINATES (SCOLAT,SLON) OF THE FICTITIOUS
C  TID SOURCE, WHICH IS TAKEN TO LIE A GROUND RANGE OF A
C  QUARTER OF AN EARTH'S CIRCUMFERENCE AWAY FROM THE RADAR IN
C  A DIRECTION ANTIPARALELL TO AZ.
C
	    SLON = RLON + ATAN2(-SINAZ, COS(TWOPI/4.-RLAT)*COSAZ)
	    COSINS = -SIN(TWOPI/4.-RLAT)*COSAZ
	    SCOLAT = ACOS(COSINS)
	    SINES = SIN(SCOLAT)
	    PHASE = PHASE*CNVRT
	END IF
	SINE = SIN(TWOPI/4.-R(2))
	COSINE = COS(TWOPI/4.-R(2))
	COSPHI = COS(R(3)-SLON)
	AUX = COSINS*COSINE + SINES*SINE*COSPHI
C
C  COMPUTE THE DIP ANGLE AND DIRECTION COSINES OF THE EARTH'S
C  MAGNETIC FIELD
C
	DIP = ATAN(2.*COSINE/SINE)
	BZ = SIN(DIP)
	BX = COS(DIP)*(SINES*COSINE*COSPHI-COSINS*SINE)/SQRT(1.-AUX*AUX)
C
C  COMPUTE THE GROUND RANGE FROM THE TID SOURCE TO THE FIELD POINT.
C
	X1 = RE*ACOS(AUX)
	Z = R(1) - RE
C
C  SUBROUTINE ELDENS COMPUTES THE AMBIENT ELECTRON DENSITY
C  NEO (ELECTRONS/CM**3) AND ITS FIRST AND SECOND Z DERIVATIVES AT
C  AN ALTITUDE Z.
C
	NEO = X/CNV
	DNEO = PXPR/CNV
	D2NEO = PX2PR/CNV
C
C  SUBROUTINE N2DENS COMPUTES THE AMBIENT NUMBER DENSITY OF
C  NITROGEN MOLECULES (MOLS/CM**3) AND ITS Z DERIVATIVE AT AN
C  ALTITUDE Z.
C
	CALL N2DENS(Z, NN2, DNN2)
C
C  COMPUTE PERTURBED ELECTRON DENSITY (NE).
C
	ALFNEO = ALPH*NEO
	BETA = GAM*NN2
	AUX = ALFNEO + BETA
	NUE = BETA*ALFNEO*(BETA+BETA+ALFNEO)/AUX/AUX
	DNUE = (2.*ALPH*BETA**3*DNEO+GAM*ALFNEO**2*(3.*BETA+ALFNEO)
     &	       *DNN2)/AUX**3
	J = Z/2. + 1.
	IF(J.GT.249)J = 249
	UEJ = BX*U(J) + BZ*W(J)
	UEJP1 = BX*U(J+1) + BZ*W(J+1)
	KZ = I/2.*LOG(UEJP1/UEJ)
	UE = UEJ*EXP(-I*(KX*X1+KZ*(Z-2*(J-1))-PHASE))
	AUXC1 = BZ*DNEO - I*NEO*(BX*KX+BZ*KZ)
	AUXC2 = I*OMEGA + NUE
	NE = NEO - DREAL(UE*AUXC1/AUXC2)
	IF(NE.LT.0.)THEN
	    NE = 0.
	    GRADNE(3) = 0.
	    GRADNE(2) = 0.0
	    GRADNE(1) = 0.0
	    RETURN
	END IF
C
C  COMPUTE THE ELECTRON DENSITY GRADIENT (GRADNE).
C
	DNEDX = DREAL(I*KX*UE*AUXC1/AUXC2)
	DNEDZ = DNEO + 
     &		DREAL(-UE/AUXC2*(BZ*D2NEO-I*DNEO*(BX*KX+BZ*KZ)-AUXC1*
     &		(I*KZ+DNUE/AUXC2)))
	AUX = DNEDX*RE/R(1)/SIN(X1/RE)
	GRADNE(1) = DNEDZ
	GRADNE(2) = AUX*(SINE*COSINS-COSINE*SINES*COSPHI)
	GRADNE(3) = AUX*SINES*SIN(R(3)-SLON)
	X = NE*CNV
	PXPR = CNV*GRADNE(1)
	PXPTH = CNV*GRADNE(2)
	PXPPH = CNV*GRADNE(3)
	PXPT = -CNV*OMEGA*DREAL(UE*AUXC1/AUXC2)
	RETURN
	END
**==N2DENS.FOR
	SUBROUTINE N2DENS(Z, NN2, DNN2)
C
C  COMPUTES THE NUMBER DENSITY OF MOLECULAR NITROGEN (NN2)
C  AND ITS Z DERIVATIVE (DNN2) AT AN ALTITUDE Z. THE UNITS
C  OF Z, NN2, AND DNN2 ARE KM, MOLECULES/CM**3, AND
C  MOLECULES/CM**3/KM, RESPECTIVELY.
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	REAL*8 N2(46), NN2
	DATA N2/1.99E19, 6.59E18, 1.44E18, 2.95E17, 6.65E16, 1.69E16, 
     &	     5.10E15, 1.34E15, 2.96E14, 4.965E13, 8.178E12, 1.62E12, 
     &	     4.01E11, 1.40E11, 6.36E10, 3.34E10, 1.91E10, 1.16E10, 
     &	     7.37E9, 4.86E9, 3.29E9, 2.29E9, 1.62E9, 1.17E9, 8.51E8, 
     &	     6.28E8, 4.67E8, 3.495E8, 2.635E8, 2.00E8, 1.52E8, 8.67E7, 
     &	     5.14E7, 3.075E7, 1.85E7, 1.125E7, 6.84E6, 4.19E6, 2.58E6, 
     &	     1.59E6, 9.86E5, 6.13E5, 3.82E5, 2.39E5, 1.50E5, 9.43E4/
	IF(Z.LT.0.)Z = 0.
	J = Z/10. + 1.
	IF(J.LE.30)THEN
	    AUX = LOG(N2(J+1)/N2(J))
	    NN2 = N2(J)*EXP(AUX*(Z/10.-J+1.))
	    DNN2 = NN2*AUX/10.
	    RETURN
	END IF
	J = 31 + (J-31)/2
	IF(J.GT.45)J = 45
	AUX = LOG(N2(J+1)/N2(J))
	NN2 = N2(J)*EXP(AUX*(Z/20.-J+16.))
	DNN2 = NN2*AUX/20.
	RETURN
	END
**==NOPERT.FOR
	SUBROUTINE NOPERT
C
C  USE WHEN AN ELECTRON DENSITY PERTURBATION IS NOT WANTED
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /XX    / X(6)
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /WW    / W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(PERT, W(150))
	MODX(2) = ' NONE '
	PERT = 0.
	RETURN
	END
**==NOTE.FOR
	SUBROUTINE NOTE(NB)
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	CHARACTER*10 NB(2, 400)
	DO I = 1, 400
	    DO J = 1, 2
		NB(J, I) = '          '
	    END DO
	END DO
	NB(1, 1) = '=1,O-RAY  '
	NB(2, 1) = '=-1,X-RAY '
	NB(1, 2) = 'EARTH RADI'
	NB(2, 2) = 'US (KM)   '
	NB(1, 3) = 'TX HEIGHT '
	NB(1, 4) = 'TX LAT(N) '
	NB(1, 5) = 'TX LON(E) '
	NB(1, 7) = 'INITIAL FR'
	NB(2, 7) = 'EQ (MHZ)  '
	NB(1, 8) = 'FINAL FREQ'
	NB(1, 9) = 'STEP IN FR'
	NB(2, 9) = 'EQ         '
	NB(1, 11) = 'INITIAL AZ'
	NB(2, 11) = 'IMUTH     '
	NB(1, 12) = 'FINAL AZIM'
	NB(2, 12) = 'UTH       '
	NB(1, 13) = 'STEP IN AZ'
	NB(2, 13) = 'IMUTH     '
	NB(1, 15) = 'INITIAL EL'
	NB(2, 15) = 'EVATION   '
	NB(1, 16) = 'FINAL ELEV'
	NB(2, 16) = 'ATION     '
	NB(1, 17) = 'STEP IN EL'
	NB(2, 17) = 'EVATION   '
	NB(1, 20) = 'RECEIVER H'
	NB(2, 20) = 'EIGHT KM  '
	NB(1, 21) = 'PENET. RAY'
	NB(2, 21) = 'WANTED    '
	NB(1, 22) = 'NO HOPS   '
	NB(1, 23) = 'MAX NO STE'
	NB(2, 23) = 'PS/HOP *  '
	NB(1, 24) = 'N LAT OF N'
	NB(2, 24) = 'GEO POLE *'
	NB(1, 25) = 'E LON OF N'
	NB(2, 25) = 'GEO POLE *'
	NB(1, 41) = 'INTEG TYP*'
	NB(1, 42) = 'MAX SSE  *'
	NB(1, 43) = 'ERATIO   *'
	NB(1, 44) = 'INTEG SS *'
	NB(1, 45) = 'MAX STEP L'
	NB(2, 45) = 'ENGTH    *'
	NB(1, 46) = 'MIN STEP L'
	NB(2, 46) = 'ENGTH    *'
	NB(1, 47) = 'FACTOR   *'
	NB(1, 57) = 'PHASE PATH'
	NB(1, 58) = 'ABSORPTION'
	NB(1, 59) = 'DOPPLER SH'
	NB(2, 59) = 'IFT       '
	NB(1, 60) = 'PATH LENGT'
	NB(2, 60) = 'H         '
	NB(1, 71) = 'NO. STEPS '
	NB(2, 71) = '/ PRINTOUT'
	NB(1, 81) = '=1, V PLOT'
	NB(2, 81) = ',=2,H PLOT'
	NB(1, 82) = '(81=2) LFA'
	NB(2, 82) = 'CT        '
	NB(1, 83) = 'LLAT      '
	NB(1, 84) = 'LLONG     '
	NB(1, 85) = 'RLAT      '
	NB(1, 86) = 'RLONG     '
	NB(1, 87) = 'TIC DIST '
	NB(1, 88) = 'HT BOTTOM'
	DO K = 83, 89
	    NB(2, K) = '      PLOT'
	END DO
	DO K = 90, 94
		NB(1, K) = 'RAY COUNTI'
		NB(2, K) = 'NG'
	ENDDO
	DO K = 100, 149
	    NB(1, K) = 'ELECTRON D'
	    NB(2, K) = 'ENSITY    '
	END DO
	DO K = 150, 199
	    NB(1, K) = 'EL DENST P'
	    NB(2, K) = 'ERTURBATNS'
	END DO
	DO K = 200, 249
	    NB(1, K) = 'MAGNETIC F'
	    NB(2, K) = 'IELD      '
	END DO
	DO K = 250, 299
	    NB(1, K) = 'COLLISION '
	    NB(2, K) = 'FREQUENCY '
	END DO
	DO K = 300, 304
	    NB(1, K) = 'OUTPUT    '
	    NB(2, K) = 'SWITCHING '
	END DO
	RETURN
	END
**==OUTPUT.FOR
	SUBROUTINE OUTPUT
C
C  THIS SUBROUTINE IS CALLED BY TRAVRS AT THE END OF EACH STEP TO PUT THE
C  VALUES OF THE HORIZONTAL AND VERTICAL NEUTRAL VELOCITY PERTURBATIONS
C  INTO THE U AND W ARRAYS.
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /MATRIX/ AY(8, 6), A(6, 6), NWAVE
	COMMON /OUTPT / K(6), AP(6), ARB(16), ARBC(16)
c KSH 25.1.91 SUN compile warning : external name WAVE already in use
c I have therefore called the common block WAVECB
	COMMON /WAVECB  / U(250), W(250)
	COMMON /TRAV  / TOPATM, BOTATM, TOLMAX, TOLMIN, CURAMP(6, 2), 
     &			OLDAMP(6), ZBEG, ZEND, DEPTHO, Z, NEQNS
	COMMON /INPT  / KX, OMEGA, INTGR(16), NLAYER
	COMMON /AIR   / T, P, M, G, GAMMA, MU, LAMBDA, B, SIGMA
c	COMPLEX*16 AUXC
C KSH 25.1.91 sun compile warning : local variable AUXC not used
	COMPLEX*16 U, W, DET, X(2), I, K, AP, ARBC, CURAMP, 
     &		   OLDAMP, AY, A, KX
	REAL*8 M, MU, LAMBDA
	EQUIVALENCE(NN, INTGR(2))
	DATA I/(0., 1.)/
	NN = NN - 1
	SQRTP = SQRT(P)
	DET = AY(1, 1)*AY(2, 2) - AY(1, 2)*AY(2, 1)
	X(1) = (OLDAMP(1)*AY(2,2)-AY(1,2)*OLDAMP(2))/DET
	X(2) = (AY(1,1)*OLDAMP(2)-OLDAMP(1)*AY(2,1))/DET
	U(NN) = (AY(3,1)*X(1)+AY(3,2)*X(2))/SQRTP
	W(NN) = (AY(1,1)*X(1)+AY(1,2)*X(2))/SQRTP
	RETURN
	END
**==PLOT.FOR
	SUBROUTINE PLOT(X, Y, NEW)
C adapted by KSH 10.1.91 to allow for option 3 in the plotting, ie for 
c W(81)=3
c since this routine is called by RAYPLT  to draw in the y axis,
c on the first call in a new plot, and to draw the rays and the
c the x axis curve
c for option 3 rays are drawn in thicker lines, therefore it is necessary
c to go back to the default thickness when drawing the axis.
c 
c KSH 1.3.91 for running on SUN the GHOST 80 calls have been amended to
c force arguments to single precision

	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /PLT   / XMIN0, XMAX0, YMIN0, YMAX0, RESET/WW/W0, W(400)
	COMMON /WWC   / ID
	CHARACTER*10 ID(10)
	CHARACTER*10 FILE(5)
	EQUIVALENCE(PLRT, W(81)),(EARTHR, W(2))

	DATA INITAL/1/, FILE/'GRAPH OUTP', 'UT USRNAME', 'EEXXX 3D R', 
     &	     'AYPLT CDC0', '1DX GHOST '/
C
C  INITIALIZE LIBRARY PLOTTING ROUTINES
C
	IF(W(303).EQ.0.0)THEN

	IF(INITAL.NE.0)THEN
	    INITAL = 0
c	    CALL PAPER(1)
c	    call gpstop(1000)
	ELSE IF(RESET.NE.0.)THEN
c	    CALL FRAME
	END IF

	IF(RESET.NE.0)THEN
	    RESET = 0.
	    IF(PLRT.EQ.1.)THEN
* needed to ensure axis is plotted for each azimuth - no idea why
c		CALL WINFOL
c		YMXNEW = 1.8384*(XMAX0-XMIN0) + YMIN0
c		CALL PSPACE(0.06, 1.05, 0.4, 2.22)
c		CALL CSPACE(0.06, 1.05, 0.0, 0.95)
c		CALL MAP(SNGL(XMIN0), SNGL(XMAX0), SNGL(YMIN0-EARTHR), 
c     +			SNGL(YMXNEW-EARTHR))
* mask required is composite plots are to be made (sto, 20/10/94)
c		CALL UNMASK(0)
c        CALL MASK(SNGL(XMIN0)-0.1*ABS(SNGL(XMIN0)), SNGL(XMAX0), 
c     +			 0.39*(SNGL(YMXNEW-YMIN0)),SNGL(YMXNEW-EARTHR))
c        CALL MSKCHR(1)
c		CALL AXORIG(SNGL(XMIN0), SNGL(YMIN0))
c		CALL CTRMAG(12)
c		CALL ANNOTP(1, 1)
c		CALL YAXISI(SNGL(50.))
c		CALL MAP(SNGL(XMIN0), SNGL(XMAX0), SNGL(YMIN0), 
c     +			SNGL(YMXNEW))
	    ELSE IF(PLRT.EQ.2.)THEN
c		CALL PSPACE(0.06, 1.05, 0.02, 0.84)
c		CALL CSPACE(0.06, 1.05, 0.02, 0.95)
c		CALL MAP(SNGL(XMIN0), SNGL(XMAX0), SNGL(YMIN0),
c     +			SNGL(YMAX0))
	    ELSE IF (PLRT.EQ.3.)THEN
c		CALL THICK(1)
c		CALL PSPACE(0.05,1.35,0.30,0.95)
c		CALL CSPACE(0.05,0.95,0.05,0.95)
c		YLOW = YMIN0 - EARTHR
c		CALL MAP(SNGL(XMIN0),SNGL(XMAX0),SNGL(YLOW),500.)
c		XAXIS = (XMAX0 + XMIN0)/2
c TBJ didnt like the y axis therefore it is commented out 
c		CALL AXORIG(SNGL(XAXIS),SNGL(YLOW))
c		CALL YAXISI(100.)
c		CALL THICK(1)	
	    END IF
 
	    IF(PLRT.EQ.1.)THEN
c		CALL WINDOW(SNGL(XMIN0), SNGL(XMAX0), SNGL(YMIN0),
c     +	SNGL(0.5*(YMIN0+YMXNEW)))
	    ELSE
c		CALL WINFOL
	    END IF
 
	END IF
 
*      **    PLOT A POINT...
C for option 3, having changed the y scale to show the height above the
c earth the point to be plotted must be adjusted by deducting the
c size of the earth radius, ie EARTHR
	IF (PLRT.EQ.3.)THEN
	    YNEW = Y - EARTHR
	END IF
 
C use these new values for option 3
	IF(NEW.EQ.0)THEN
	    IF (PLRT.EQ.3.)THEN
c		CALL JOIN(SNGL(X),SNGL(YNEW))
	    ELSE	
c	        CALL JOIN(SNGL(X), SNGL(Y))
	    END IF
	ELSE
	    IF (PLRT.EQ.3.)THEN
c		CALL POSITN(SNGL(X),SNGL(YNEW))
	    ELSE
c	        CALL POSITN(SNGL(X), SNGL(Y))
	    END IF
	END IF
* endif for w(303) eq 0.0...
	ENDIF 
	RETURN
C
C  TERMINATE THE CURRENT PLOT
C
	ENTRY PLTEND(X, Y, NEW)
	CALL LABPLT
	RETURN
	END
**==PMDSTOP.FOR
	SUBROUTINE PMDSTOP
C
C     ROUTINE CALLED ON FAILURE
C     INCLUDED FOR CYBER - VAX COMPATIBILITY
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	PRINT *, 'HELP FAILURE !!!'
	STOP
	END
**==POLCAR.FOR
	SUBROUTINE POLCAR
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	DIMENSION X0(6), X(6), R0(4)
	COMMON R(6)/COORD/S
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
C
C
C  CONVERTS SPHERICAL COORDINATES TO CARTESIAN
C
	IF(R(5).EQ.0. .AND. R(6).EQ.0.)THEN
C
C  VERTICAL INCIDENCE
C
	    VERT = 1.
	    R0(1) = R(1)
	    R0(2) = R(2)
	    R0(3) = R(3)
	    R0(4) = SIGN(1.D0, R(4))
	    RETURN
	ELSE
	    VERT = 0.
	    SINA = SIN(R(2))
	    COSA = SIN(PID2-R(2))
	    SINP = SIN(R(3))
	    COSP = SIN(PID2-R(3))
	    X0(1) = R(1)*SINA*COSP
	    X0(2) = R(1)*SINA*SINP
	    X0(3) = R(1)*COSA
	    X(4) = R(4)*SINA*COSP + R(5)*COSA*COSP - R(6)*SINP
	    X(5) = R(4)*SINA*SINP + R(5)*COSA*SINP + R(6)*COSP
	    X(6) = R(4)*COSA - R(5)*SINA
	    RETURN
	END IF
C
C
C  STEPS THE RAY A DISTANCE S, AND THEN
C  CONVERTS CARTESIAN COORDINATES TO SPHERICAL COORDINATES
C
	ENTRY CARPOL
	IF(VERT.EQ.0.)THEN
	    X(1) = X0(1) + S*X(4)
	    X(2) = X0(2) + S*X(5)
	    X(3) = X0(3) + S*X(6)
	    TEMP = SQRT(X(1)**2+X(2)**2)
	    R(1) = SQRT(X(1)**2+X(2)**2+X(3)**2)
	    R(2) = ATAN2(TEMP, X(3))
	    R(3) = ATAN2(X(2), X(1))
	    R(4) = (X(1)*X(4)+X(2)*X(5)+X(3)*X(6))/R(1)
	    R(5) = (X(3)*(X(1)*X(4)+X(2)*X(5))-(X(1)**2+X(2)**2)*X(6))
     &		   /(R(1)*TEMP)
	    R(6) = (X(1)*X(5)-X(2)*X(4))/TEMP
	    RETURN
	END IF
C
C  VERTICAL INCIDENCE
C
	R(1) = R0(1) + R0(4)*S
	R(2) = R0(2)
	R(3) = R0(3)
	R(4) = R0(4)
	R(5) = 0.
	R(6) = 0.
	RETURN
	END
**==POLY2.FOR
	SUBROUTINE POLY2(L, M, X1, X2)
C
C  SOLVES X*X +L*X +M =0.
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /INPT  / KX, OMEGA, INTGR(16), NLAYER
	COMPLEX*16 L, M, X1, X2, C, SQ, X(2), Z, KX
	EQUIVALENCE(INDEX, INTGR(1)), (NTEST, INTGR(3)), 
     &		    (MPRINT, INTGR(4)), (IEXTR, INTGR(6)), 
     &		    (KCHECK, INTGR(8)), (KEY, INTGR(10))
C
C  KEY IS INITIALIZED IN THE PROGRAM WHICH CALLS TRAVRS. KEY = 0 INDICATE
C  THAT POLY2 IS BEING CALLED FOR THE FIRST TIME IN A GIVEN TRAVERSAL.
C
	C = (.5, 0.)*L
	Z = C*C - M
	SQ = SQRT(Z)
	AIMZ = DIMAG(Z)
	IF(KEY.EQ.0)THEN
	    I = 1
	    J = 2
	ELSE IF(DREAL(Z).LE.0.)THEN
	    IF(AIMZ*AIMSZ.LE.0.)THEN
		J = I
		I = 1 + MOD(I, 2)
	    END IF
	END IF
	X(I) = -C + SQ
	X(J) = -C - SQ
	X1 = X(1)
	X2 = X(2)
	IF(ABS(X2).LT.ABS(X1))X2 = M/X1
	AIMSZ = AIMZ
	RETURN
	END
**==POLY3.FOR
	SUBROUTINE POLY3(A, B, C, X1, X2, X3, NFLAG)
C
C  SOLVES X**3 + A*X**2 + B*X + C = 0. IF NFLAG IS 1, MAKES ERROR TEST AN
C  RETURNS ALL THREE ROOTS. IF NFLAG IS 2, RETURNS ONLY ONE ROOT.
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /INPT  / KX, OMEGA, INTGR(16), NLAYER
	COMPLEX*16 A, A2, B, B1, C, C1, P, P3, Q, U, V, W1, W2, X1, X2, 
     &		   X3, KX
	EQUIVALENCE(INDEX, INTGR(1)), (NTEST, INTGR(3)), 
     &		    (MPRINT, INTGR(4)), (IEXTR, INTGR(6)), 
     &		    (KCHECK, INTGR(8)), (KEY, INTGR(10))
	DATA T, TT, TH/.37037037E-01, .740740741E-01, .333333333/
	DATA W1, W2/(-.5, .866025404), (-.5, -.866025404)/
	DATA TWOPI/6.2831853/
C
C  KEY IS INITIALIZED IN THE PROGRAM WHICH CALLS TRAVRS. KEY = 0 INDICATE
C  THAT POLY3 IS BEING CALLED FOR THE FIRST TIME IN A GIVEN TRANSVERSAL.
C
	IF(KEY.EQ.0)BRANCH = 0.
	A2 = A*A
C
C  THE SUM OF THE ROOTS IS A. RENORMALIZE X BY DIVIDING IT BY A. THE NEW
C  COEFFICIENTS ARE A1 = 1.,
C
	B1 = B/A2
	C1 = C/A2/A
	P = B1 - TH
	Q = TT - B1*TH + C1
	P3 = -P*P*P*T
	CALL POLY2(Q, P3, V, U)
C
C  TAKE CUBE ROOT OF U BY TAKING MINUS THE CUBE ROOT OF MINUS U,
C  SO THAT THE CUBE ROOT OF A NEGATIVE REAL NUMBER WILL BE
C  NEGATIVE REAL. THIS IS INTENDED TO
C  ASSURE THAT THE GRAVITY WAVE WILL BE THE FIRST ROOT WHEN VISCOSITY IS
C
	U3R = -DREAL(U)
	U3I = -DIMAG(U)
	RU = U3R*U3R + U3I*U3I
	RU = RU**.166666667
	IF(U3R.LT.0.0 .AND. KEY.NE.0)THEN
	    IF(U3I*U3IS.LT.0.)BRANCH = BRANCH + SIGN(TWOPI, U3IS)
	END IF
	TU = ATAN2(U3I, U3R) + BRANCH
	TU = TU*TH
	UR = RU*COS(TU)
	UI = RU*SIN(TU)
	U = -DCMPLX(UR, UI)
	V = -TH*P/U
	X1 = (U+V-TH)*A
	IF(NFLAG.EQ.2)RETURN
	X2 = (U*W1+V*W2-TH)*A
	X3 = (U*W2+V*W1-TH)*A
	U3IS = U3I
	KEY = 1
	RETURN
	END
**==PRINTR.FOR
	SUBROUTINE PRINTR(NWHY, CARD)
C
C  PRINTS OUTPUT AND PUNCHES RAYSETS WHEN REQUESTED
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	DIMENSION G(3, 3), G1(3, 3), RPRINT(20), NPR(20)
	CHARACTER*8 NWHY
	CHARACTER*1 TYPE(3)
	CHARACTER*6 HEADR1(20), HEADR2(20), UNITS(20), HEAD1(20), 
     &		    HEAD2(20), UNIT(20)
	COMMON /CONST / PI, PIT2, PID2, DEGS, RAD, DUM(3)
	COMMON /FLG   / NTYP, NEWWR, NEWWP, PENET, LINES, IHOP, HPUNCH
	COMMON R(20), T/WW/W0, W(400)
	COMMON /WWC   / ID
	CHARACTER*10 ID(10)
	EQUIVALENCE(THETA, R(2)), (PHI, R(3))
	COMMON /RIN   / COLL, FIELD, SPITZ, N2, N2I, PNP(8), GAM, PGP(6)
     &			, POLAR, LPOLAR, FACTOR, SPACE
	COMMON /RINC  / MODRIN
	CHARACTER*10 MODRIN(3)
	COMMON /STOHOM/ RCLOSE,NCLOSE,DSTANC,STOPSE,STOGRP,STOAMP,
     &	STOELL,STOAZI
	COMMON /STOPOW/ GNDRAN(10000), GRPRAN(10000), RANELV(10000),
     & TMPELV, DOAELL(10000), DOAAZI(10000), NUMRAN, RANALT(10000)
	COMMON /STORAY/ RADPOS(500000), THTPOS(500000), PHIPOS(500000), NUMPOS
	COMMON /STOKVE/ RADVEC(500000), THTVEC(500000), PHIVEC(500000), NUMVEC
	COMMON /CTEST / EL, RANGE, ELB, ELS, RX, HH, RR, AZD, RGM, IEND, JX
	EQUIVALENCE(EARTHR, W(2)), (XMTRH, W(3)), (TLAT, W(4)), 
     &		    (TLON, W(5)), (F, W(6)), (AZ1, W(10)), (BETA, W(14))
     &		    , (RCVRH, W(20)), (HOP, W(22)), (PLAT, W(24)), 
     &		    (PLON, W(25)), (RAYSET, W(72)), (RLAT, W(90)),
     &              (RLON,W(91)), (DLAT, W(92)), (DLON, W(93))
	LOGICAL SPACE, NEWWR, NEWWP, PENET, RCLOSE, RXHIT
	REAL*8 N2, N2I
	COMPLEX*16 PNP, GAM, PGP, POLAR, LPOLAR
	DATA TYPE/'X', 'N', 'O'/, HEADR1(7)/'  PHAS'/, HEADR2(7)
     &	     /'E PATH'/, UNITS(7)/'   KM '/, HEADR1(8)/'  ABSO'/, 
     &	     HEADR2(8)/'RPTION'/, UNITS(8)/'   DB '/, HEADR1(9)
     &	     /'   DOP'/, HEADR2(9)/'PLER  '/, UNITS(9)/'  C/S '/, 
     &	     HEADR1(10)/'    GA'/, HEADR2(10)/'MMA   '/, UNITS(10)
     &	     /' REAL '/, HEADR1(11)/'    GA'/, HEADR2(11)/'MMA   '/, 
     &	     UNITS(11)/' IMAG '/, HEADR1(12)/' PATH '/, HEADR2(12)
     &	     /'LENGTH'/, UNITS(12)/'   KM'/
	DATA HEAD1(1)/'      '/, HEAD2(1)/'      '/, UNIT(1)/'      '/
	RPRINT(1) = 0.
	CALL RINDEX

	IF(NEWWP)THEN
C
C  ********* NEW W ARRAY -- REINITIALIZE
C
	    NEWWP = .FALSE.
	    SPL = SIN(PLON-TLON)
	    CPL = SIN(PID2-(PLON-TLON))
	    SP = SIN(PLAT)
	    CP = SIN(PID2-PLAT)
	    SL = SIN(TLAT)
	    CL = SIN(PID2-TLAT)
C
C  ********* MATRIX TO ROTATE COORDINATES
C
	    G(1, 1) = CPL*SP*CL - CP*SL
	    G(1, 2) = SPL*SP
	    G(1, 3) = -SL*SP*CPL - CL*CP
	    G(2, 1) = -SPL*CL
	    G(2, 2) = CPL
	    G(2, 3) = SL*SPL
	    G(3, 1) = CL*CP*CPL + SP*SL
	    G(3, 2) = CP*SPL
	    G(3, 3) = -SL*CP*CPL + SP*CL
	    DENM = G(1, 1)*G(2, 2)*G(3, 3) + G(1, 2)*G(3, 1)*G(2, 3)
     &		    + G(2, 1)*G(3, 2)*G(1, 3) - G(2, 2)*G(3, 1)*G(1, 3)
     &		    - G(1, 2)*G(2, 1)*G(3, 3) - G(1, 1)*G(3, 2)*G(2, 3)
C
C  ********* THE MATRIX G1 IS THE INVERSE OF THE MATRIX G
C
	    G1(1, 1) = (G(2,2)*G(3,3)-G(3,2)*G(2,3))/DENM
	    G1(1, 2) = (G(3,2)*G(1,3)-G(1,2)*G(3,3))/DENM
	    G1(1, 3) = (G(1,2)*G(2,3)-G(2,2)*G(1,3))/DENM
	    G1(2, 1) = (G(3,1)*G(2,3)-G(2,1)*G(3,3))/DENM
	    G1(2, 2) = (G(1,1)*G(3,3)-G(3,1)*G(1,3))/DENM
	    G1(2, 3) = (G(2,1)*G(1,3)-G(1,1)*G(2,3))/DENM
	    G1(3, 1) = (G(2,1)*G(3,2)-G(3,1)*G(2,2))/DENM
	    G1(3, 2) = (G(3,1)*G(1,2)-G(1,1)*G(3,2))/DENM
	    G1(3, 3) = (G(1,1)*G(2,2)-G(2,1)*G(1,2))/DENM
	    R0 = EARTHR + XMTRH
C
C  ********* CARTESIAN COORDINATES OF TRANSMITTER
C
	    XR = R0*G(1, 1)
	    YR = R0*G(2, 1)
	    ZR = R0*G(3, 1)
	    CTHR = G(3, 1)
	    STHR = SIN(ACOS(CTHR))
	    PHIR = ATAN2(YR, XR)
	    ALPH = ATAN2(G(3,2), G(3,3))
C
C  *********
C
	    NR = 6
	    NP = 0
	    DO NN = 7, 20
		IF(W(NN+50).NE.0.)THEN
C
C  ********* DEPENDENT VARIABLE NUMBER NN IS BEING INTEGRATED
C  ********* NR IS THE NUMBER OF DEPENDENT VARIABLES BEING INTEGRATED
C
		    NR = NR + 1
		    IF(W(NN+50).EQ.2.)THEN
C
C  ********* DEPENDENT VARIABLE NUMBER NN IS BEING INTEGRATED AND PRINTED.
C  ********* NP IS THE NUMBER OF DEPENDENT VARIABLES BEING INTEGRATED AND
C  ********* PRINTED
C
			NP = NP + 1
C
C  ********* SAVE THE INDEX OF THE DEPENDENT VARIABLE TO PRINT
C
			NPR(NP) = NR
			HEAD1(NP) = HEADR1(NN)
			HEAD2(NP) = HEADR2(NN)
			UNIT(NP) = UNITS(NN)
		    END IF
		END IF
	    END DO
	    NP1 = MIN0(NP, 3)
	    DOPP = 0.0
	    ABSORB = 0.0
	    PDEV = 0.0
	END IF
C
C  ********* PRINT COLUMN HEADINGS AT THE BEGINNING OF EACH RAY
C
	IF(IHOP.EQ.0)THEN
	    IF(W(300).EQ.0.)THEN
			WRITE(16, 99001)(HEAD1(NN), HEAD2(NN), NN=1, NP1)
99001		FORMAT(42X, 'AZIMUTH'/41X, 'DEVIATION', 6X, 
     &		       'ELEVATION'/19X, 'HEIGHT    RANGE', 1X, 
     &		       2(4X,'XMTR   LOCAL'), 4X, 'POLARIZATI', 
     &		       'ON   GROUP PATH', 5A6, A5)
			WRITE(16, 99002)(UNIT(NN), NN=1, NP1)
99002		FORMAT(13X, 2(8X,'KM'), 3X, 2(4X,'DEG',5X,'DEG'), 5X, 
     &		       'REAL    IMAG', 7X, 'KM', 3X, 3(4X,A6,2X))
	    END IF
	    IF(RAYSET.NE.0.)THEN
C
C  ********* PUNCH A TRANSMITTER RAYSET
C
		TLOND = TLON*DEGS
		IF(TLOND.LT.0.)TLOND = TLOND + 360.
		TLATD = TLAT*DEGS
		IF(TLATD.LT.0.)TLATD = TLATD + 360.
		AZ = AZ1*DEGS
		EL = BETA*DEGS
		NHOP = HOP
C     WRITE(1,1200) ID(1),TYPE(NTYP),XMTRH,TLATD,TLOND,RCVRH,F,AZ,EL,
C    +POLAR,NHOP,'T'
99005		FORMAT(A3, A1, 4PF9.0, 3PF6.0, 3PF6.0, 4PF9.0, 4PF9.0, 
     &		       5PF10.0, 5PF10.0, 5X, 2PF5.0, 2PF5.0, I1, A1)
	    END IF
	END IF
C
C  *********
C
	V = 0.
	IF(N2.NE.0.)V = (R(4)**2+R(5)**2+R(6)**2)/N2 - 1.
	H = R(1) - EARTHR
	STH = SIN(THETA)
	CTH = SIN(PID2-THETA)
C
C  ********* CARTESIAN COORDINATES OF RAY POINT, ORIGIN AT TRANSMITTER
C
	XP = R(1)*STH*SIN(PID2-PHI) - XR
	YP = R(1)*STH*SIN(PHI) - YR
	ZP = R(1)*CTH - ZR
C
C  ********* CARTESIAN COORDINATES OF RAY POINT, ORIGIN AT TRANSMITTER AND
C  ********* ROTATED
C
	EPS = XP*G1(1, 1) + YP*G1(1, 2) + ZP*G1(1, 3)
	ETA = XP*G1(2, 1) + YP*G1(2, 2) + ZP*G1(2, 3)
	ZETA = XP*G1(3, 1) + YP*G1(3, 2) + ZP*G1(3, 3)
	RCE2 = ETA**2 + ZETA**2
	RCE = SQRT(RCE2)
C
C  ********* GROUND RANGE
C
      RANGE = EARTHR*ATAN2(RCE, EARTHR+EPS+XMTRH)
	
* writes all the ground ranges of a given trace out, sto, 26/2/96
* the additional checks (sto, 28/8/96) ensure that the same ground
* reflection is not counted twice (since RCVR occurs twice per hit
* for some rays - no idea why).
      IF(NWHY(1:4).EQ.'AOPG')THEN
      	RANALT(NUMRAN+1)=H
      ENDIF
      IF(NWHY(1:4).EQ.'RCVR' .AND. (ABS(RANGE-GNDRAN(NUMRAN)).GT.1.0e-3
     &	.OR. ABS(TMPELV-RANELV(NUMRAN)).GT.1.0e-4) .AND. W(302).EQ.0)THEN
      	NUMRAN=NUMRAN+1
      	RXHIT=.TRUE.
      	IF(NUMRAN .GT. 10000) STOP 'number ranges exceeded in PRINTR'
     	GNDRAN(NUMRAN)=RANGE
     	GRPRAN(NUMRAN)=T
     	RANELV(NUMRAN)=TMPELV
     	DOAELL(NUMRAN)=ATAN2(R(4), SQRT(R(5)**2+R(6)**2))*DEGS
*     	CALL FNDBER(PRVRAD,PRVTHT,PRVPHI,R(1),R(2),R(3),BARING)
      ENDIF

* if signal strength  and phase path at receiver required then this will be used
* min dist may not always produce correct results
	IF(W(94).EQ.1.0 .AND. (NWHY(1:4).EQ.'RCVR' .OR. (NWHY(1:8).EQ.
     &	'MIN DIST' .AND. ABS(R(1)-EARTHR).LT.10.0)))THEN
* convert receiver coordinates to geomagnetic (sto, 23/11/93)
            SP=SIN(PLAT)
            CP=COS(PLAT)
            SDPH=SIN(RLON-PLON)
            CDPH=COS(RLON-PLON)
            SL=SIN(RLAT)
            CL=COS(RLAT)
            GLAT=ACOS(CDPH*CP*CL+SP*SL)
            GLON=ATAN2(SDPH*CL, CDPH*SP*CL-CP*SL)
	    IF(ABS(GLAT-THETA).LT.DLAT .AND. ABS(GLON-PHI)
     &		.LT.DLON)THEN
			RCLOSE=.TRUE.
			DSTANC=RANGE
			NR=6
			IF(W(57).NE.0.0)THEN
		    	NR=NR+1
		    	STOPSE=R(NR)    ! the phase path
			ENDIF
			STOGRP=T	! the group path
			IF(W(58).NE.0.0)THEN
		    	NR=NR+1
				STOAMP=R(NR)    ! the absorption
			ENDIF
			STOELL=ATAN2(R(4), SQRT(R(5)**2+R(6)**2))*DEGS
     		CALL FNDBER(RADPOS(numpos),THTPOS(numpos),PHIPOS(numpos),
     &			R(1),R(2),R(3),BARING)
     		STOAZI=BARING*DEGS
    	ENDIF
	ENDIF

C
C  ********* ANGLE OF WAVE NORMAL WITH LOCAL HORIZONTAL
C
	ELL = ATAN2(R(4), SQRT(R(5)**2+R(6)**2))*DEGS
C
C  ********* STRAIGHT LINE DISTANCE FROM TRANSMITTER TO RAY POINT
C
	SR = SQRT(RCE2+EPS**2)
	IF(NP.NE.0)THEN
	    DO I = 1, NP
		NN = NPR(I)
		RPRINT(I) = R(NN)
	    END DO
	END IF
	IF(SR.GE.1.E-6)THEN
 
C  ********* ELEVATION ANGLE OF RAY POINT FROM TRANSMITTER
 
	    EL = ATAN2(EPS, RCE)*DEGS
	    IF(RCE.GE.1.E-6)THEN
 
C  ********* AZIMUTH ANGLE OF RAY POINT FROM TRANSMITTER
 		AZA=-999.0	! a dummy value for range and rx hit purposes
		ANGA = ATAN2(ETA, ZETA)
		AZDEV = 180. - MOD(540.D0-(AZ1-ANGA)*DEGS, 360.D0)
		IF(R(5).NE.0. .OR. R(6).NE.0.)THEN
		    ANA = ANGA - ALPH
		    SANA = SIN(ANA)
		    SPHI = SANA*STHR/STH
		    CPHI = -SIN(PID2-ANA)*SIN(PID2-(PHI-PHIR))
     &			    + SANA*SIN(PHI-PHIR)*CTHR
		    AZA = 180. - 
     &			  MOD(540.-(ATAN2(SPHI,CPHI)-ATAN2(R(6),R(5)))
     &			  *DEGS, 360.D0)
		    IF(W(300).EQ.0.)THEN
				WRITE(16, 99006)V, NWHY, H, RANGE, AZDEV, AZA, 
     &					EL, ELL, POLAR, T, 
     &					(RPRINT(NN), NN=1, NP1)
99006			FORMAT(1X, E7.1, 1X, A8, F9.3, F10.3, 
     &			       2(F8.2,F8.2), (F8.2,F8.2), 4F12.4)
		    END IF
 
C  ********* WAVE NORMAL IS VERTICAL, SO AZIMUTH DIRECTION CANNOT BE
C  ********* CALCULATED
 
		ELSE IF(W(300).EQ.0.)THEN
		    WRITE(16, 99008)V, NWHY, H, RANGE, AZDEV, EL, ELL, 
     &				    POLAR, T, (RPRINT(NN), NN=1, NP1)
99008		    FORMAT(1X, E7.1, 1X, A8, F9.3, F10.3, F8.3, 8X, 
     &			   F8.2, F8.2, (F8.2,F8.2), 4F12.4)
		END IF
 
C  ********* NEARLY DIRECTLY ABOVE OR BELOW TRANSMITTER. CAN NOT CALCULATE
C  ********* AZIMUTH DIRECTION FROM TRANSMITTER ACCURATELY
 
	    ELSE IF(W(300).EQ.0.)THEN
		WRITE(16, 99010)V, NWHY, H, RANGE, EL, ELL, POLAR, T, 
     &				(RPRINT(NN), NN=1, NP1)
99010		FORMAT(1X, E7.1, 1X, A8, F9.3, F10.3, 16X, F8.2, F8.2, 
     &		       (F8.2,F8.2), 4F12.4)
	    END IF
 
C  ********* TOO CLOSE TO TRANSMITTER TO CALCULATE DIRECTION FROM
C  ********* TRANSMITTER
 
	ELSE IF(W(300).EQ.0.)THEN
	    WRITE(16, 99012)V, NWHY, H, RANGE, ELL, POLAR, T, 
     &			    (RPRINT(NN), NN=1, NP1)
99012	    FORMAT(1X, E7.1, 1X, A8, F9.3, F10.3, 24X, F8.2, (F8.2,F8.2)
     &		   , 4F12.4)
	END IF
 
	LINES = LINES + 1
	IF(NWHY.EQ.'APOGEE  ') then
c	{
		HH = H
		AZD = AZDEV
		RR = RANGE
c	}
	END IF
	IF(NWHY.EQ.'RCVR   ') RGM = RANGE

	IF(NP.GT.3)THEN

c HH  - max height of reflection
c RGM - maximum range
c AZD - azimuth deviation from GCP

C  ********* ADDITIONAL LINE TO PRINT REMAINING DEPENDENT INTEGRATION
C  ********* VARIABLES
 
	    IF(W(300).EQ.0.)THEN
		WRITE(16, 99014)(RPRINT(NN), NN=4, NP)
99014		FORMAT(94X, 3F12.4)
	    END IF
	    LINES = LINES + 1
	END IF
	IF(CARD.EQ.0.)RETURN
 
C  ********* PUNCH A RAYSET
 
	IF(AZDEV.LT.-90.)AZDEV = AZDEV + 360.
	IF(AZA.LT.-90.)AZA = AZA + 360.
	TDEV = T - SR
	NR = 6
	IF(W(57).NE.0.)THEN
 
C  ********* PHASE PATH
 
	    NR = NR + 1
	    PDEV = R(NR) - SR
	END IF
	IF(W(58).NE.0.)THEN
 
C  ********* ABSORPTION
 
	    NR = NR + 1
	    ABSORB = R(NR)
	END IF
 
C  ********* DOPPLER SHIFT
 
	IF(W(59).NE.0.)DOPP = R(NR+1)
C     WRITE(1,4500) HPUNCH,RANGE,AZDEV,AZA,ELL,SR,TDEV,PDEV,ABSORB,DOPP,
C    +POLAR,IHOP,NWHY
99016	FORMAT(4P2F9.0, 4P3F6.0, 3PF8.0, 3P4F6.0, 2PF5.0, 2PF5.0, I1, 
     &	       A1)

	RETURN
	END
**==PROPAG.FOR
	SUBROUTINE PROPAG(DEPTH, NO)
C
C  PROPAGATES FLUID AMPLITUDES FOR A VERTICAL DISTANCE OF DEPTH BY RESOLV
C  THEM INTO A SUPERPOSITION OF PLANE WAVES AND MULTIPLYING EACH WAVE BY
C  EXP(I*KZ*DEPTH).
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /INPT  / KX, OMEGA, INTGR(16), NLAYER
	COMMON /MATRIX/ AY(8, 6), A(6, 6), NWAVE
	COMMON /OUTPT / K(6), AP(6), ARB(16), ARBC(16)
	COMMON /TRAV  / TOPATM, BOTATM, TOLMAX, TOLMIN, CURAMP(6, 2), 
     &			OLDAMP(6), ZBEG, ZEND, DEPTHO, Z, NEQNS
	COMMON /MISC  / ALF241, ALPHA2, BETA2, E1MB, E1PB, ETA3, ETA4, 
     &			ETA7, ETA11, ETA31, ETA41, ETA71, IALFD2, 
     &			IALPHA, IETALF, ISB, ISB1, ISB2, ISB3, ISBBY, 
     &			ISBXZ, SBBY2, SBBY2N, BXBY, BX2, BY2, BZ2, 
     &			GAMMA1
	COMPLEX*16 ALF241, ALPHA2, BETA2, E1MB, E1PB, ETA3, ETA4, ETA7, 
     &		   ETA11, ETA31, ETA41, ETA71, IALFD2, IALPHA, IETALF, 
     &		   ISB, ISB1, ISB2, ISB3, ISBBY, ISBXZ, SBBY2, SBBY2N
	COMPLEX*16 KX, AY, A, CURAMP, OLDAMP, K, AP, X(6), ARBC, DET
	EQUIVALENCE(INDEX, INTGR(1)), (NTEST, INTGR(3)), 
     &		    (MPRINT, INTGR(4)), (IEXTR, INTGR(6)), 
     &		    (KCHECK, INTGR(8)), (KEY, INTGR(10))
	EQUIVALENCE(X(1), ARBC(1))
	DET = AY(1, 1)*AY(2, 2) - AY(1, 2)*AY(2, 1)
	X(1) = (CURAMP(1,NO)*AY(2,2)-AY(1,2)*CURAMP(2,NO))/DET
	X(2) = (AY(1,1)*CURAMP(2,NO)-CURAMP(1,NO)*AY(2,1))/DET
	IF(NEQNS.GT.2)WRITE(6, 99001)
99001	FORMAT('0HELP! NEQNS IS GREATER THAN 2'///)
C
C  PROPAGATE THE AMPLITUDES, OMITTING THE COMMON EXPONENTIAL
C  GROWTH (EXP(Z/2H)).
C
	DO I = 1, NEQNS
	    X(I) = X(I)*EXP((0.,-1.)*KX*(K(I)-IALFD2)*DEPTH)
	END DO
	DO I = 1, NEQNS
	    CURAMP(I, NO) = (0., 0.)
	    DO J = 1, NEQNS
		CURAMP(I, NO) = CURAMP(I, NO) + AY(I, J)*X(J)
	    END DO
	END DO
	RETURN
	END
**==QPARAB.FOR
	SUBROUTINE QPARAB
C
C  PLAIN PARABOLIC OR QUASI-PARABOLIC PROFILE
C  W(104) = 0. FOR A PLAIN PARABOLIC PROFILE
C  = 1. FOR A QUASI-PARABOLIC PROFILE
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /XX2   / PX2PR
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /PERTOF/ISWITCH
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (FC, W(101)), (HM, W(102))
     &		    , (YM, W(103)), (QUASI, W(104)), (PERT, W(150))
C
	IF(ICODE.EQ.0)THEN
	    MODX(1) = 'QPARAB'
	    RETURN
	END IF
	HMAX = HM
	PXPR = 0.
	PXPTH = 0.
	PXPPH = 0.0
	PX2PR = 0.
	H = R(1) - EARTHR
	FCF2 = (FC/F)**2
	CONST = 1.
	IF(QUASI.EQ.1.)CONST = (EARTHR+HM-YM)/R(1)
	Z = (H-HM)/YM*CONST
	X = MAX(0.D0, FCF2*(1.D0-Z*Z))
	IF(X.NE.0.)THEN
	    IF(QUASI.EQ.1.)CONST = (EARTHR+HM)*(EARTHR+HM-YM)/R(1)**2
	    PXPR = -2.*Z*FCF2/YM*CONST
	    PX2PR = -2.*FCF2/YM**2*CONST**2
	END IF
	IF(PERT.NE.0. .AND. ISWITCH.NE.0)CALL ELECT1
	RETURN
	END
**==RAYPL2.FOR
	SUBROUTINE RAYPL2
C
C  SUBROUTINE RENAMED TO AVOID MULTIPLE REFERENCES
C  PETER HAYHURST 19/08/85
C
C  REPLACES SUBROUTINES RAYPLT,PLOT, AND LABPLT IF PLOTS ARE
C  NOT WANTED OR IF A PLOTTER IS NOT AVAILABLE
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /WW    / W0, W(400)
	COMMON /WWC   / ID
	CHARACTER*10 ID(10)
	EQUIVALENCE(PLT, W(81))
	PLT = 0.
C
C     ENTRY RENAMED TO AVIOD MULTIPLE REFERENCES
C     PETER HAYHURST 19/08/85
C
	ENTRY ENDPL2
	RETURN
	END
**==RAYPLT.FOR
	SUBROUTINE RAYPLT
C
C  W(81)=1. PLOTS PROJECTION OF RAYPATH ON VERTICAL PLANE
C  =2. PLOTS PROJECTION OF RAYPATH ON GROUND
C  =3. KAY OPTION - lines of constant altitude at 100,200,300,400km
c  		added KSH 10.1.91
c Modified to plot catchment area in horizontal plane (sto, 7/1/94)
c and ring perturbation locus of maximum intensity (10/1/94, sto).
c Now plots receiver position in the vertical plane (sto, 13/1/94)
c Also improved ksh code for plotting out height markers.
       
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /PLT   / XL, XR, YB, YT, RESET
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /FLG   / NTYP, NEWWR, NEWWP, PENET, LINES, IHOP, HPUNCH
c group path all along ray now collected in this common block.  tky
	common /tim/ grppth(500000)
c extend the unlabelled common to go up to group path variable (T). tky
c	COMMON R(6)/WW/W0, W(400)
	COMMON R(20), T /WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /STORAY/ RADPOS(500000), THTPOS(500000), PHIPOS(500000), NUMPOS
	COMMON /STOKVE/ RADVEC(500000), THTVEC(500000), PHIVEC(500000), NUMVEC
	COMMON /STOGND/ GNDTHT(400000), GNDPHI(400000), GNDKTH(400000), 
     & GNDKPH(400000), GNDKRA(400000), NUMGND
	CHARACTER*10 ID(10), CHGT*7, FRED*7
	EQUIVALENCE(TH, R(2)), (PH, R(3))
	EQUIVALENCE(EARTHR, W(2)), (PLAT, W(24)), (PLON, W(25)), 
     &		    (PLT, W(81)), (FACTR, W(82)), (LLAT, W(83)), 
     &		    (LLON, W(84)), (RLAT, W(85)), (RLON, W(86)), 
     &		    (TIC, W(87)), (HB, W(88)), (RXLAT,W(90)),
     &              (RXLON,W(91)), (DELLAT,W(92)), (DELLON,W(93)),
     &              (RINLAT,W(152)),(RINLON,W(153)),(RINRAD,W(154))
	REAL*8 LLAT, LLON, LTIC
	LOGICAL NEWWR, NEWWP, PENET
C  NEW W ARRAY -- REINITIALIZE
	IF(NEWWR)THEN
	    NEWWR = .FALSE.
	    RESET = 1.

C CONVERT COORDINATES OF VERTICAL PLANE FROM GEOGRAPHIC TO GEOMAGNETIC
	    SW = SIN(PLAT)
	    CW = SIN(PID2-PLAT)
	    SLM = SIN(LLAT)
	    CLM = SIN(PID2-LLAT)
	    SRM = SIN(RLAT)
	    CRM = SIN(PID2-RLAT)
	    CDPHI = SIN(PID2-(LLON-PLON))
	    PHL = ATAN2(SIN(LLON-PLON)*CLM, CDPHI*SW*CLM-CW*SLM)
	    CTHL = CDPHI*CW*CLM + SW*SLM
	    STHL = SIN(ACOS(CTHL))
	    CDPHI = SIN(PID2-(RLON-PLON))
	    PHR = ATAN2(SIN(RLON-PLON)*CRM, CDPHI*SW*CRM-CW*SRM)
	    CTHR = CDPHI*CW*CRM + SW*SRM
	    STHR = SIN(ACOS(CTHR))
	    CLR = CTHL*CTHR + STHL*STHR*SIN(PID2-(PHL-PHR))
	    SLR = SQRT(1.-CLR**2)
	END IF
	IF(PLT.EQ.2.)THEN     ! this is for the horizontal plane
	    IF(FACTR.EQ.0.)FACTR = 1.
	    ALPH1 = ATAN2(STHR*SIN(PHR-PHL), (CTHR-CTHL*CLR)/STHL)
	    XL = 0.
	    XR = EARTHR*ACOS(CLR)
	    YT = 0.5*XR/FACTR
	    YB = -YT
	ELSE           ! for both vertical plots , ie options 1 and 3
	    FACTR = 1.
	    R0 = EARTHR + HB
	    ALPHA = .5*ACOS(CLR)
	    XR = R0*SIN(ALPHA)
	    XL = -XR
	    YB = R0*SIN(PID2-ALPHA)
	    YT = YB + 2.*XR
	END IF

c collect data for IDL program, jonesutils (file=rays.dat)
        if(numpos.eq.500000)then
                return
        endif
	NUMPOS=NUMPOS+1
	RADPOS(NUMPOS)=R(1)
	THTPOS(NUMPOS)=TH
	PHIPOS(NUMPOS)=PH
c collect group path all along ray too.  tky
	grppth(NUMPOS)=T
c and for just the ground positions (this cuts down idl run times)...
	IF(NUMPOS.NE.1)THEN
		IF(ABS(R(1)-EARTHR).LT.1.0 .AND. w(305) .NE. 1)THEN
			NUMGND=NUMGND+1
			IF(NUMGND .GT. 400000)THEN
				WRITE(*,*)'ERROR - more than 400000 ground reflections'
				STOP 'ERROR in RAYPLT - more than 400000 ground reflections'
			ENDIF
			GNDTHT(NUMGND)=TH
			GNDPHI(NUMGND)=PH
c write out the k-vectors on the ground if this is required
			IF(w(307).NE.1)THEN
				GNDKRA(NUMGND)=R(4)
				GNDKTH(NUMGND)=R(5)
				GNDKPH(NUMGND)=R(6)
			ENDIF
		ENDIF
	ENDIF
* and for k-kectors (file=kvect.dat)
	NUMVEC=NUMVEC+1
	RADVEC(NUMVEC)=R(4)
	THTVEC(NUMVEC)=R(5)
	PHIVEC(NUMVEC)=R(6)
* and for upper-hybrid (time consuming)
	IF(w(308).EQ.0.0)THEN
		CALL CALCUH
	ENDIF
		
	IF(W(303).EQ.0.0)THEN
	STH = SIN(TH)
	CTH = SIN(PID2-TH)
	CR = CTHR*CTH + STHR*STH*SIN(PID2-(PHR-PH))
	CL = CTHL*CTH + STHL*STH*SIN(PID2-(PHL-PH))
	CEA = ATAN2(CR-CL*CLR, CL*SLR)
	NEW = 1
	IF(IHOP.NE.0)NEW = 0
	IF(PLT.EQ.2.)THEN
c for the horizontal plane
	    IF(CL.GT.1.)CL = 1.
	    SL = SQRT(1.-CL*CL)
	    TMP1 = STH*SIN(PH-PHL)
	    TMP2 = (CTH-CTHL*CL)/STHL
	    ALPH2 = 0.
	    IF(TMP1.NE.0. .OR. TMP2.NE.0.)ALPH2 = ATAN2(TMP1, TMP2)
c	    CALL PLOT(EARTHR*CEA, EARTHR*ASIN(SL*SIN(ALPH1-ALPH2)), NEW)
	    RETURN
	ELSE
c	    CALL PLOT(R(1)*SIN(CEA-ALPHA), R(1)*SIN(PID2-(CEA-ALPHA)), 
c     &		      NEW)
	    RETURN
	END IF
	ENDIF
C
C
C  DRAW AXES AND CALL FOR LABELING AND TERMINATION OF THIS PLOT
C
c  actually draws curved line for x axis 
c  y axis plotted in subroutine plot
c
	ENTRY ENDPLT
	IF(W(303).EQ.0.0)THEN
	TICKX = 0.01*(YT-YB)
	IF(PLT.EQ.2.)THEN	    ! horizontal plane
	    IF(W(94).EQ.1.)THEN	    ! catchment area
c		CALL HORPOS(RXLAT,RXLON,CEA,ALPH2,SL,CTHR,STHR,CTHL,
c     &                      STHL,PHR,PHL,CLR,SLR)
c		CALL PLOT(EARTHR*CEA, EARTHR*ASIN(SL*SIN(ALPH1-ALPH2)),
c     &                    1)
c		CALL CIRCLE(SNGL(XR)/200.0)
c		CALL HORPOS(RXLAT-DELLAT,RXLON-DELLON,CEA,ALPH2,SL,CTHR
c     &                      ,STHR,CTHL,STHL,PHR,PHL,CLR,SLR)
c		CALL PLOT(EARTHR*CEA, EARTHR*ASIN(SL*SIN(ALPH1-ALPH2)),
c     &                    1)
c		CALL HORPOS(RXLAT-DELLAT,RXLON+DELLON,CEA,ALPH2,SL,CTHR
c     &                      ,STHR,CTHL,STHL,PHR,PHL,CLR,SLR)
c		CALL PLOT(EARTHR*CEA, EARTHR*ASIN(SL*SIN(ALPH1-ALPH2)),
c     &                    0)
c		CALL HORPOS(RXLAT+DELLAT,RXLON+DELLON,CEA,ALPH2,SL,CTHR
c     &                      ,STHR,CTHL,STHL,PHR,PHL,CLR,SLR)
c		CALL PLOT(EARTHR*CEA, EARTHR*ASIN(SL*SIN(ALPH1-ALPH2)),
c     &                    0)
c		CALL HORPOS(RXLAT+DELLAT,RXLON-DELLON,CEA,ALPH2,SL,CTHR
c     &                      ,STHR,CTHL,STHL,PHR,PHL,CLR,SLR)
c		CALL PLOT(EARTHR*CEA, EARTHR*ASIN(SL*SIN(ALPH1-ALPH2)),
c     &                    0)
c		CALL HORPOS(RXLAT-DELLAT,RXLON-DELLON,CEA,ALPH2,SL,CTHR
c     &                      ,STHR,CTHL,STHL,PHR,PHL,CLR,SLR)
c		CALL PLOT(EARTHR*CEA, EARTHR*ASIN(SL*SIN(ALPH1-ALPH2)),
c     &                    0)
	    ENDIF
* plot the locus of max. intensity for a ring perturbation
	    IF(INDEXP.EQ.12 .AND. W(150).NE.0.0)THEN
		NEWANG=1
		RADIUS=RINRAD/EARTHR    ! convert from km to radians
c		CALL HORPOS(RINLAT,RINLON,CEA,ALPH2,
c     &                      SL,CTHR,STHR,CTHL,STHL,PHR,PHL,CLR,SLR)
c		CALL PLOT(EARTHR*CEA, EARTHR*ASIN(SL*
c     &                    SIN(ALPH1-ALPH2)),NEWANG)
c		CALL CIRCLE(SNGL(XR)/200.0)
c		CALL BROKEN(4,4,4,4)
		DO IANGLE=0,360,1
		    RANGLE=REAL(IANGLE)*PI/180.0
		    CHALAT=RADIUS*COS(RANGLE)
* includes correction for latitude
		    CHALON=RADIUS*SIN(RANGLE)/COS(RINLAT)
c		    CALL HORPOS(RINLAT-CHALAT,RINLON-CHALON,CEA,ALPH2,
c     &                      SL,CTHR,STHR,CTHL,STHL,PHR,PHL,CLR,SLR)
c		    CALL PLOT(EARTHR*CEA, EARTHR*ASIN(SL*
c     &                    SIN(ALPH1-ALPH2)),NEWANG)
		    NEWANG=0
		ENDDO
c		CALL FULL
	    ENDIF
	    DTIC = TIC*EARTHR
	    LTIC = DTIC/FACTR
	    TICY = XL + 0.01*(XR-XL)
	    NTIC = YT/LTIC
	    TIC1 = -LTIC*NTIC
c	    CALL PLOT(XL, YB, 1)
	    NTIC = 2*NTIC + 1
	    DO I = 1, NTIC
		Y = TIC1 + (I-1)*LTIC
c		CALL PLOT(XL, Y, 0)
c		CALL PLOT(TICY, Y, 0)
c		CALL PLOT(XL, Y, 0)
	    END DO
c	    CALL PLOT(XL, YT, 0)
c	    CALL PLOT(XL, 0., 1)
	    NTIC = (XR-XL)/DTIC
	    DO I = 1, NTIC
		X = I*DTIC
c		CALL PLOT(X, 0., 0)
c		CALL PLOT(X, TICKX, 0)
c		CALL PLOT(X, 0., 0)
	    END DO
c	    CALL PLOT(XR, 0., 0)
	ELSE
c for options 1 and 3, the vertical plane plots
	    IF(W(94).EQ.1.)THEN	    ! plot receiver position on ground
c		CALL HORPOS(RXLAT,RXLON,CEA,ALPH2,SL,CTHR,STHR,CTHL,
c     &                      STHL,PHR,PHL,CLR,SLR)
c		CALL PLOT(EARTHR*SIN(CEA-ALPHA), EARTHR*SIN(PID2-
c     &                    (CEA-ALPHA)),1)
c		CALL CIRCLE(SNGL(XR)/100.0)
c		CALL CIRCLE(SNGL(XR)/80.0)
	    ENDIF
	    R1 = EARTHR - TICKX
	    X = XL
	    Y = YB
c	    CALL PLOT(X, Y, 1)
	    NTIC = 2
	    IF(TIC.NE.0.)NTIC = NTIC + 2.*ALPHA/TIC
	    NLINE = MAX0(1, 100/NTIC)
	    DO I = 1, NTIC
		ANG = -ALPHA + (I-1)*TIC
c draw the tick on the x axis curve
c		CALL PLOT(R1*SIN(ANG), R1*SIN(PID2-ANG), 0)
c		CALL PLOT(X, Y, 0)
c draw the next section of this curve
		DO J = 1, NLINE
		    ANG = ANG + TIC/NLINE
		    X = EARTHR*SIN(ANG)
		    Y = EARTHR*SIN(PID2-ANG)
c		    CALL PLOT(X, Y, 0)
		END DO
	    END DO
c	    CALL PLOT(XR, YB, 0)

c for option 3 add lines at constant altitudes of 100, 200, 300 km
c draw these as dashed lines
c
c KSH 1.3.91 GHOST 80 calls have been changed so that the arguments
c are forced to single precision ( this done for runnung on SUN)
	    IF (PLT.EQ.3.) THEN
c		CALL BROKEN(5,5,5,5)
		DO IHGT=100,400,100	
		    X = XL
		    Y = YB
		    YIGHT = Y + REAL(IHGT)
		    YLABEL=YIGHT-EARTHR
		    WRITE(FRED,'(1X,I3)')IHGT
		    READ(FRED,'(A)')CHGT
		    CHGT=CHGT(1:4)//' km'
c		    CALL PLOTCS(SNGL(X),SNGL(YLABEL),CHGT)
c		    CALL PLOT(X, YIGHT, 1)
		    NTIC = 2
		    IF(TIC.NE.0.)NTIC = NTIC + 2.*ALPHA/TIC
		    NLINE = MAX0(1, 100/NTIC)
		    DO I = 1, NTIC
			ANG = -ALPHA + (I-1)*TIC
			DO J = 1, NLINE
			    ANG = ANG + TIC/NLINE
			    X = EARTHR*SIN(ANG)
			    Y = EARTHR*SIN(PID2-ANG)
			    YIGHT = Y + REAL(IHGT)
c			    CALL PLOT(X, YIGHT, 0)
			END DO
		     END DO
		    YBIGHT = YB + REAL(IGHT)
c		    CALL PLOT(XR, YBIGHT, 0)
		ENDDO

C go back to complete lines
c	        CALL FULL
 	    END IF

	END IF
	ENDIF
	
c	CALL PLTEND
	RETURN
	END
**==REACH.FOR
	SUBROUTINE REACH
C
C  CALCULATES THE STRAIGHT LINE RAY PATH BETWEEN THE EARTH
C  AND THE IONOSPHERE OR BETWEEN IONOSPHERIC LAYERS
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /RK    / STEP, E1MAX, E1MIN, E2MAX, E2MIN, FACT, 
     &			RSTART, N, MODE
	COMMON /TRAC  / SMT, GROUND, PERIGE, THERE, MINDIS, NEWRAY
	COMMON /COORD / S
	COMMON /RIN   / COLL, FIELD, SPITZ, N2, N2I, PNP(8), GAM, PGP(6)
     &			, POLAR, LPOLAR, FACTR, SPACE
	COMMON /RINC  / MODRIN
	CHARACTER*10 MODRIN(3)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(20), T, STP, DRDT(20)/WW/W0, W(400)
	COMMON /WWC   / ID
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (XMTRH, W(3)), (RCVRH, W(20))
	LOGICAL CROSS, CROSSG, CROSSR, SPACE, GROUND, PERIGE, THERE, 
     &		MINDIS, NEWRAY, RSPACE
	COMPLEX*16 PNP, GAM, PGP, POLAR, LPOLAR
	REAL*8 N2, N2I
	DATA NSTEP/500/
	CALL HASEL
	H = R(1) - EARTHR
	IF(.NOT.NEWRAY .AND. .NOT.RSPACE)CALL PRINTR('EXIT ION', 0.D0)
	NEWRAY = .FALSE.
	V = SQRT(R(4)**2+R(5)**2+R(6)**2)
C
C  ********* NORMALIZE THE WAVE NORMAL DIRECTION TO ONE
C
	R(4) = R(4)/V
	R(5) = R(5)/V
	R(6) = R(6)/V
! 	print*, '1', R(1), R(4), V
C
C  ********* NEGATIVE OF DISTANCE ALONG RAY TO CLOSEST APPROACH TO CENTER
C  ********* OF EARTH
C
	UP = R(1)*R(4)
	RADG = EARTHR**2 - R(1)**2*(R(5)**2+R(6)**2)
	DISTG = SQRT(MAX(0.D0,RADG))
C
C  ********* DISTANCE ALONG RAY TO FIRST INTERSECTION WITH OR CLOSEST
C  ********* APPROACH TO THE EARTH
C
	SG = -UP - DISTG
C
C  ********* CROSSG IS TRUE IF THE RAY WILL INTERSECT OR TOUCH THE EARTH
C
	CROSSG = UP.LT.0. .AND. RADG.GE.0.
	RADR = (EARTHR+RCVRH)**2 - R(1)**2*(R(5)**2+R(6)**2)
	DISTR = SQRT(MAX(0.D0,RADR))
C
C  ********* DISTANCE ALONG RAY TO THE FIRST INTERSECTION WITH OR CLOSEST
C  ********* APPROACH TO THE RECEIVER HEIGHT
C
	SR = DISTR - UP
	IF(UP.LT.0. .AND. DISTR.LT.-UP .AND. R(1).NE.EARTHR+RCVRH)
     &	   SR = -DISTR - UP
C
C  ********* CROSSR IS TRUE IF THE RAY WILL INTERSECT WITH OR MAKE A
C  ********* CLOSEST APPROACH TO THE RECEIVER HEIGHT
C
	CROSSR = R(4).LT.0. .OR. R(1).LT.(EARTHR+RCVRH)
	CROSS = CROSSG .OR. CROSSR
C
C  ********* MAXIMUM DISTANCE IN WHICH TO LOOK FOR THE IONOSPHERE
C
	S1 = MIN(SR, SG)
	IF(.NOT.CROSSG)S1 = SR
	IF(UP.LT.0.)THEN
	    CROSS = .TRUE.
C
C  ********* IF RAY IS GOING DOWN, S1 IS AT MOST THE DISTANCE TO A PERIGEE
C
	    S1 = MIN(S1, -UP)
	END IF
C
C  ********* CONVERT THE POSITION AND DIRECTION OF THE RAY TO CARTESIAN
C  ********* COORDINATES
C
	CALL POLCAR
	sstep = 5.
	s = sstep
	CALL CARPOL
	CALL ELECTX
	CALL RINDEX
	go to 100

	SSTEP = 100.
	S = SSTEP
	DO I = 1, NSTEP
		IF((S-SSTEP).GT.S1 .AND. CROSS)GO TO 100
C
C  ********* CONVERT POSITION AND DIRECTION TO SPHERICAL POLAR COORDINATES
C  ********* AT A DISTANCE S ALONG THE RAY
C
	  CALL CARPOL
	  CALL ELECTX
C
C  ********* FREE SPACE
C
	  IF(X.EQ.0.)THEN
			S = S + SSTEP
	  ELSE
			CALL RINDEX
C
C  ********* EFFECTIVELY FREE SPACE
C
			IF(SPACE)THEN
		    S = S + SSTEP
			ELSE
		    IF(SSTEP.LT.0.5E-4)GO TO 100
C
C  ********* RAY IN THE IONOSPHERE.  STEP BACK OUT
C
		    S = S - SSTEP
C
C  ********* DECREASE STEP SIZE
C
		    SSTEP = SSTEP/10.
		    S = S + SSTEP
			END IF
	  END IF
	END DO
	WRITE(16, 99001)NSTEP
99001	FORMAT(' EXCEEDED', I5, ' STEPS IN SUBROUTINE REACH')
	return	!odd effect with penetrating ray
c	CALL EXIT
100	CONTINUE
	IF(CROSS)S = MIN(S, S1)
C
C  ********* CONVERT POSITION AND DIRECTION TO SPHERICAL POLAR COORDINATES
C  ********* AT A DISTANCE S ALONG THE RAY
C
	CALL CARPOL
C
C  ********* AVOID THE RAY BEING SLIGHTLY UNDERGROUND
C
	R(1) = MAX(R(1), EARTHR)
C
C  ********* ONE STEP INTEGRATION
C
	DO NN = 7, N
	    R(NN) = R(NN) + S*DRDT(NN)
	END DO
	T = T + S
	CALL RINDEX
C
C  ********* AT A PERIGEE
C
	PERIGE = S.EQ.(-UP)
C
C  ********* CORRECT MINOR ERRORS
C
	IF(PERIGE)R(4) = 0.
C
C  ********* KEEP CONSISTENCY AFTER CORRECTING MINOR ERRORS
C
	DRDT(1) = R(4)
C
C  ********* ON THE GROUND
C
	GROUND = S.EQ.SG .AND. CROSSG
C
C  ********* AT THE RECEIVER HEIGHT
C
	THERE = S.EQ.SR .AND. CROSSR .AND. .NOT.PERIGE
C
C  ********* AT A CLOSEST APPROACH TO THE RECEIVER HEIGHT
C
	MINDIS = PERIGE .AND. S.EQ.SR .AND. CROSSR
	IF(.NOT.SPACE)CALL PRINTR('ENTR ION', 0.D0)
	RSPACE = SPACE
	IF(N2.EQ.0.)RETURN
	V = SQRT(R(4)**2+R(5)**2+R(6)**2)/SQRT(N2)
C
C  ********* RENORMALIZE THE WAVE NORMAL DIRECTION TO = SQRT(REAL(N**2))
C
	R(4) = R(4)/V
	R(5) = R(5)/V
	R(6) = R(6)/V
! 	print*, '2', R(1), R(4), V, N2
! 	print*,N2,V,R(4)
	RETURN
	END
**==READW.FOR
	SUBROUTINE READW
C
C  READS W ARRAY
C  A 1 IN THE FOLLOWING COLUMNS WILL MAKE THE DESCRIBED CONVERSIONS
C  COL 18   DEGREES TO RADIANS
C  COL 19   GREAT CIRCLE DISTANCE IN KM TO RADIANS
C  COL 20   NAUTICAL MILES TO KM
C  COL 21   FEET TO KM
C
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DEGS, RAD, DUM(3)
	COMMON /WW    / W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /STOSPD/ STSEC, NOOFRY, NOTRANS
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (PLT, W(81)), (PERT, W(150)),
     &             (ELDPLT,W(89))
	INTEGER DEG, FEET
	READ(15, 99001, END=200)ID
C
C     INDEXR - SELECTS METHOD OF CALCULATING REFRACTIVE INDEX
C
C     1 AHWFWC APPLETON HARTREE, FEILD AND COLLISIONS
C     2 AHWFNC APPLETON HARTREE, FIELD NO COLLISIONS
C     3 AHNFWC APPLETON HARTREE, COLLISIONS NO FIELD
C     4 AHNFNC APPLETON HARTREE, NO COLLISIONS, NO FIELD
C     5 AHSPWC APPLETON HARTREE, SPITZ, WITH FIELD AND COLLISIONS
C     6 AHSPNC APPLETON HARTREE, SPITZ, WITH FIELD, NO COLLISIONS
C     7 SWWF   SEN - WYLLER
C     8 SYNF   SEN - WYLLER NO FIELD
C
	READ(15, '(I3)', END=200)INDEXR
C
C     INDEXE - SELECTS ELECTRON DENSITY MODEL
C
C     1 TABLEX CALCULATED FROM PROFILES
C     2 CHAPX  CHAPMAN LAYER WITH TILTS RIPPLES AND GRADIENTS
C     3 VCHAPX CHAPMAN LAYER WITH VARIABLE SCALE HEIGHT
C     4 DCHAPT TWO CHAPMAN LAYERS WITH TILTS
C     5 LINEAR LINEAR ELECTRON DENSITY MODEL
C     6 QPARAB PLAIN OR QUASI PARABOLIC PROFILE
C     7 BULGE  ANALYTICAL MODEL OF EQUATORIAL F2 LAYER
C     8 EXPX   EXPONENTIAL COLLISION FREQUENCY MODEL
C
	READ(15, '(I3)', END=200)INDEXE
C
C     SELECTS ELECTRON DENSITY PERTURBATION MODEL
C
C     0 NOPERT NO PERTURBATION REQUIRED
C     1 TORUS
C     2 DTORUS
C     3 TROUGH
C     4 SHOCK
C     5 WAVE
C     6 WAVE2
C     7 CLOUD
C     8 MODTID
C     9 DOPPLE
C    10 BLOB
C    11 BLOB2	    Simulates Ne change during heating
C    12 RING	    Simulates heating in E-region at edge of heater beam
C    13 TROUGH2     A soemwhat more realistic trough model
C    14 BLOBFAI		A blob with embedded medium scale irregularities
C
	READ(15, '(I3)', END=200)INDEXP
	READ(15, '(I3)', END=200)INDEXM
	READ(15, '(I3)', END=200)INDEXC
C
C ---- SET ICODE=0 AND CALL THE FOLLOWING FIVE ROUTINES TO SET UP
C      THE REQUIRED CONSTANTS THAT WERE ORIGINALLY IN DATA STATEMENTS
C
	ICODE = 0
	CALL RINDEX
	CALL ELECTX
	CALL ELECT1
	CALL MAGY
	CALL COLFRZ
	ICODE = 1
C
99001	FORMAT(BZ, 10A8)
100	CONTINUE
	READ(15, 99002, END=200)NW, W(NW), DEG, KM, NM, FEET
C	WRITE(*,*)NW, W(NW), DEG, KM, NM, FEET
99002	FORMAT(BZ, I3, E14.7, 5I1)
	IF(NW.EQ.0)THEN
	    IF(INDEXP.EQ.0)THEN
		PERT = 0.
C	    ELSE
C		PERT = 1.
	    END IF
	    RETURN
	ELSE
	    IF(NW.LE.0 .OR. NW.GT.400)THEN
		WRITE(16, 99003)NW
99003		FORMAT('1THE SUBSCRIPT ', I3, 
     &		       ' ON THE W-ARRAY INPUT IS OUT OF BO', 
     &		       'UNDS.  ALLOWABLE VALUES ARE 1 THROUGH 400. ')
c		IF(PLT.GT.0)CALL GREND
		CALL EXIT
	    END IF
	    IF(DEG.NE.0.)W(NW) = W(NW)*RAD
	    IF(KM.NE.0)THEN
		W(NW) = W(NW)/EARTHR
		IF((INDEXP.EQ.10 .OR. INDEXP.EQ.11 .OR. INDEXP.EQ.14) 
     &	 .AND. W(150).NE.0.0 .AND. NW.EQ.155)THEN
		    W(NW)=W(NW)/COS(W(152)) ! correction of dlong for lat.
		ENDIF			    ! valid for small dlong
	    ENDIF
	    IF(NM.NE.0)W(NW) = W(NW)*1.852
	    IF(FEET.NE.0)W(NW) = W(NW)*3.048006096E-4
	    GO TO 100
	END IF
C
200	CONTINUE
C++++++++++++++++
C write electron density profile
C April 2009, lbnc
C++++++++++++++++
C	CALL NEPLOT
c	IF(ELDPLT.NE.0.0) 
c	IF(W(303).EQ.0.0 .AND. (PLT.GT.0.0 .OR. ELDPLT.NE.0.0))CALL GREND
c	CALL SECC(SECS)
	secs=0.0
	DIFF=SECS-STSEC
c	write(*,'(a,f8.2,a)')'The program took ', diff/60.0, ' minutes'
c	write(*,'(a,f8.2,a)')'At a speed of    ', (noofry*notrans)/diff,
c     &	' rays per sec'
	STOP 'NORMAL TERMINATION - END OF INPUT'
	END

! 	************************************************************************
! 	************************************************************************
	SUBROUTINE RINDEX
C
C ---- SUBROUTINE TO THE SELECT THE REQUIRED METHOD FOR CALCULATING
C      THE REFRACTIVE INDEX
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
C
	IF(INDEXR.LT.1 .OR. INDEXR.GT.8)CALL PMDSTOP
	IF(INDEXR.EQ.1)THEN
	    CALL AHWFWC
	ELSE IF(INDEXR.EQ.2)THEN
	    CALL AHWFNC
	ELSE IF(INDEXR.EQ.3)THEN
	    CALL AHNFWC
	ELSE IF(INDEXR.EQ.4)THEN
	    CALL AHNFNC
	ELSE IF(INDEXR.EQ.5)THEN
	    CALL AHSPWC
	ELSE IF(INDEXR.EQ.6)THEN
	    CALL AHSPNC
	ELSE IF(INDEXR.EQ.7)THEN
	    CALL SWWF
	ELSE IF(INDEXR.EQ.8)THEN
	    CALL SYNF
	END IF
	RETURN
	END


! 	********************************************************************
! 	********************************************************************
**==RKAM.FOR
! 	********************************************************************
! 	********************************************************************
	SUBROUTINE RKAM
C
C  NUMERICAL INTEGRATION OF DIFFERENTIAL EQUATIONS
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /RK    / SPACE, E1MAX, E1MIN, E2MAX, E2MIN, 
     &			FACT, RSTART, NN, MODE
	COMMON Y(20), T, STEP, DYDT(20)
	DIMENSION DELY(4, 20), BET(4), XV(5), FV(4, 20), YU(5, 20)
	DOUBLE PRECISION YU
	IF(RSTART.NE.0.)THEN
	    MM = 1
	    LL = 1
	    IF(MODE.EQ.1)MM = 4
	    ALPHA = T
	    EPM = 0.0
	    BET(2) = 0.5
	    BET(1) = 0.5
	    BET(3) = 1.0
	    BET(4) = 0.0
	    STEP = SPACE
	    R = 19.0/270.0
	    XV(MM) = T
	    IF(E1MIN.LE.0.)E1MIN = E1MAX/55.
	    IF(FACT.LE.0.)FACT = 0.5
	    CALL HASEL
	    DO I = 1, NN
		FV(MM, I) = DYDT(I)
		YU(MM, I) = Y(I)
	    END DO
	    RSTART = 0.
	ELSE IF(MODE.NE.1)THEN
	    GO TO 200
	END IF
C
C
C  RUNGE-KUTTA
C
100	CONTINUE
	DO K = 1, 4
	    DO I = 1, NN
		DELY(K, I) = STEP*FV(MM, I)
		Z = YU(MM, I)
		Y(I) = Z + BET(K)*DELY(K, I)
	    END DO
	    T = BET(K)*STEP + XV(MM)
	    CALL HASEL
	    DO I = 1, NN
		FV(MM, I) = DYDT(I)
	    END DO
	END DO
	DO I = 1, NN
	    DEL = (DELY(1,I)+2.0*DELY(2,I)+2.0*DELY(3,I)+DELY(4,I))/6.0
	    YU(MM+1, I) = YU(MM, I) + DEL
	END DO
	MM = MM + 1
	XV(MM) = XV(MM-1) + STEP
	DO I = 1, NN
	    Y(I) = YU(MM, I)
	END DO
	T = XV(MM)
	CALL HASEL
	IF(MODE.EQ.1)GO TO 300
	DO I = 1, NN
	    FV(MM, I) = DYDT(I)
	END DO
	IF(MM.LE.3)GO TO 100
C
C
C  ADAMS-MOULTON
C
200	CONTINUE
	DO I = 1, NN
	    DEL = STEP*(55.*FV(4,I)-59.*FV(3,I)+37.*FV(2,I)-9.*FV(1,I))
     &		  /24.
	    Y(I) = YU(4, I) + DEL
	    DELY(1, I) = Y(I)
	END DO
	T = XV(4) + STEP
	CALL HASEL
	XV(5) = T
	DO I = 1, NN
	    DEL = STEP*(9.*DYDT(I)+19.*FV(4,I)-5.*FV(3,I)+FV(2,I))/24.
	    YU(5, I) = YU(4, I) + DEL
	    Y(I) = YU(5, I)
	END DO
	CALL HASEL
	IF(MODE.GT.2)THEN
C
C
C  ERROR ANALYSIS
C
	    SSE = 0.0
	    DO I = 1, NN
		EPSIL = R*ABS(Y(I)-DELY(1,I))
		IF(MODE.EQ.3 .AND. Y(I).NE.0.)EPSIL = EPSIL/ABS(Y(I))
		IF(SSE.LT.EPSIL)SSE = EPSIL
	    END DO
	    IF(E1MAX.GT.SSE)THEN
		IF(LL.GT.1 .AND. SSE.LT.E1MIN .AND. E2MAX.GT.ABS(STEP))
     &		   THEN
		    LL = 2
		    MM = 3
		    XV(2) = XV(3)
		    XV(3) = XV(5)
		    DO I = 1, NN
			FV(2, I) = FV(3, I)
			FV(3, I) = DYDT(I)
			YU(2, I) = YU(3, I)
			YU(3, I) = YU(5, I)
		    END DO
		    STEP = 2.0*STEP
		    GO TO 100
		END IF
	    ELSE IF(ABS(STEP).GT.E2MIN)THEN
		MM = 1
		LL = 1
		STEP = STEP*FACT
		GO TO 100
	    END IF
	END IF
C
C
C  EXIT ROUTINE
C
300	CONTINUE
	LL = 2
	MM = 4
	DO K = 1, 3
	    XV(K) = XV(K+1)
	    DO I = 1, NN
		FV(K, I) = FV(K+1, I)
		YU(K, I) = YU(K+1, I)
	    END DO
	END DO
	XV(4) = XV(5)
	DO I = 1, NN
	    FV(4, I) = DYDT(I)
	    YU(4, I) = YU(5, I)
	END DO
	IF(MODE.LE.2)RETURN
	E = ABS(XV(4)-ALPHA)
	IF(E.LE.EPM)GO TO 200
	EPM = E
	RETURN
	END

	
! 	********************************************************************
! 	********************************************************************
**==S.FOR
! 	********************************************************************
! 	********************************************************************
	FUNCTION S(X)
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	DOUBLE PRECISION PIH, XD, Y, V, A, QZ, QN, Q, Z
	DATA A1/0.3183099/, A2/0.10132/, B1/0.0968/, B2/0.154/
	PIH = 1.5707963267949
C
C
C
	XA = ABS(X)
	IF(XA.LE.4.)THEN
C
C
C
	    XD = X
	    Y = PIH*XD*XD
	    V = Y*Y
	    A = Y/3.D0
	    Z = A
	    M = 15.*(XA+1.)
	    DO I = 1, M
		KZ = 2*(I-1)
		KV = 4*(I-1)
		QZ = KV + 3
		QN = (KZ+2)*(KZ+3)*(KV+7)
		Q = QZ/QN
		A = -A*Q*V
		Z = Z + A
	    END DO
	    Z = Z*XD
	    S = Z
	    RETURN
	END IF
C
C
C
	W = PIH*X*X
	XV = XA**4
	S = 0.5 - (A1-B1/XV)*COS(W)/XA - (A2-B2/XV)*SIN(W)/XA**3
	IF(X.LT.0.)S = -S
	RETURN
	END
**==SETUP.FOR
	SUBROUTINE SETUP(Z)
C
C  CALLS ATMODL TO DETERMINE PHYSICAL PARAMETERS OF THE
C  ATMOSPHERE AT ALTITUDE Z. THEN CALCULATES THE BASIC
C  DIMENSIONLESS PARAMETERS AND CERTAIN AUXILIARY
C  QUANTITIES. THEN CALLS SUBROUTINE DISPER, WHICH SOLVES THE DISPERSION
C  RELATION. THEN CALLS SUBROUTINE FACTRS, WHICH COMPUTES THE POLARIZATIO
C  FACTORS AT ALTITUDE Z.
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /AIR   / T, P, M, G, GAM, MU, LAMBDA, B, SIG
	COMMON /ATMOS / ALPHA, BETA, ETA, NU, BX, BY, BZ, GAMMA, SIGMA
	COMMON /INPT  / KX, OMEGA, INTGR(16), NLAYER
	COMMON /MISC  / ALF241, ALPHA2, BETA2, E1MB, E1PB, ETA3, ETA4, 
     &			ETA7, ETA11, ETA31, ETA41, ETA71, IALFD2, 
     &			IALPHA, IETALF, ISB, ISB1, ISB2, ISB3, ISBBY, 
     &			ISBXZ, SBBY2, SBBY2N, BXBY, BX2, BY2, BZ2, 
     &			GAMMA1
	COMMON /OUTPT / K(6), AP(6), ARB(16), ARBC(16)
	COMMON /MATRIX/ AY(8, 6), A(6, 6), NWAVE
	COMPLEX*16 ALF241, ALPHA, ALPHA2, AP, BETA, BETA2, E1MB, E1PB, 
     &		   ETA, ETA3, ETA4, ETA7, ETA11, ETA31, ETA41, ETA71, I, 
     &		   IALFD2, IALPHA, IETALF, ISB, ISB1, ISB2, ISB3, ISBBY, 
     &		   ISBXZ, K, KX, KX2, NU, SBBY2, SBBY2N, ARBC, AY, A
	REAL*8 LAMBDA, M, MU
	EQUIVALENCE(INDEX, INTGR(1)), (NTEST, INTGR(3)), 
     &		    (MPRINT, INTGR(4)), (IEXTR, INTGR(6)), 
     &		    (KCHECK, INTGR(8)), (KEY, INTGR(10))
	DATA I, R/(0., 1.), 8.31432E7/
	CALL ATMODL(Z*1.E-5, INDEX)
	H = R*T/(M*G)
	RHO = P/(H*G)
	KX2 = KX*KX
	ALPHA = 1./(H*KX)
	BETA = OMEGA*OMEGA/(G*H*KX2)
	GAMMA = GAM
	ALPHA2 = ALPHA*ALPHA
	ALF241 = ALPHA2/4. + 1.
	BETA2 = BETA*BETA
	E1MB = 1. - BETA
	E1PB = 1. + BETA
	GAMMA1 = GAMMA - 1.
	IALPHA = I*ALPHA
	IALFD2 = IALPHA/2.
	IF(INDEX.NE.0)THEN
	    NU = I*LAMBDA*T*KX2/(OMEGA*P)
	    ETA = I*OMEGA*MU/(P+P+P)
	    E1MB = E1MB + ETA
	    E1PB = E1PB + ETA
	    ETA3 = ETA + ETA + ETA
	    ETA4 = ETA3 + ETA
	    ETA7 = ETA3 + ETA4
	    ETA11 = 1. + ETA
	    ETA31 = 1. + ETA3
	    ETA41 = 1. + ETA4
	    ETA71 = 1. + ETA7
	    IETALF = IALPHA*ETA
	    IF(INDEX.NE.1)THEN
		BXBY = BX*BY
		BX2 = BX*BX
		BY2 = BY*BY
		BZ2 = BZ*BZ
		SIGMA = SIG*B*B/(RHO*OMEGA)
		ISB = I*SIGMA*BETA
		ISB1 = ISB*(1.-BX2)
		ISB2 = ISB*(1.-BY2)
		ISB3 = ISB*(1.-BZ2)
		ISBBY = ISB*BY
		ISBXZ = ISB*BX*BZ
		SBBY2 = -ISBBY*ISBBY
		SBBY2N = NU*SBBY2
	    END IF
	END IF
	CALL DISPER(NWAVE)
	CALL FACTRS
	RETURN
	END
**==SHOCK.FOR
	SUBROUTINE SHOCK
C
C  A PERTURBATION TO AN ELECTRON DENSITY MODEL SIMULATING A SHOCK WAVE
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (P, W(151)), (WW, W(152)), 
     &		    (ALAT, W(153)), (ALON, W(154)), (S, W(155)), 
     &		    (H0, W(156))
	REAL*8 LAT, LON
C
	IF(ICODE.EQ.0)THEN
	    MODX(2) = ' SHOCK'
	    RETURN
	END IF
	IF(X.EQ.0. .AND. PXPR.EQ.0. .AND. PXPTH.EQ.0. .AND. PXPPH.EQ.0.)
     &	   RETURN
	IF(P.EQ.0. .OR. WW.EQ.0.)RETURN
	H = R(1) - EARTHR
	RHOC = S*(H-H0) - WW
	LON = R(3) - ALON
	LAT = PID2 - R(2) - ALAT
	COSLON = COS(LON)
	COSLAT = COS(LAT)
	U = COSLON*COSLAT
	RHO = R(1)*ACOS(U)
	DIF = RHOC - RHO
	CON = -9./WW**2
	CONS = P*EXP(CON*DIF**2)
	CONST = 1. + CONS
	CON = 2.*CON*CONS*DIF
	PXPR = PXPR*CONST + X*CON*(S-RHO/R(1))
	CONS = R(1)*(1./SQRT(1.-U**2))
	PXPTH = PXPTH*CONST + X*CON*CONS*COSLON*SIN(LAT)
	PXPPH = PXPPH*CONST - X*CON*CONS*COSLAT*SIN(LON)
	X = X*CONST
	RETURN
	END
**==STEP.FOR
	SUBROUTINE STEP(DEPTH, NO)
C
C  ......STEPS SOLUTION ACROSS NO LAYERS OF THE SPECIFIED DEPTH, STARTING
C  AT ZBEG. THE INITIAL VALUES ARE AVAILABLE IN OLDAMP. CURAMP HOLDS
C  THE MOST RECENT VALUE OF THE SOLUTIONS (AT ZEND).
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /MATRIX/ AY(8, 6), A(6, 6), NWAVE
	COMMON /TRAV  / TOPATM, BOTATM, TOLMAX, TOLMIN, CURAMP(6, 2), 
     &			OLDAMP(6), ZBEG, ZEND, DEPTHO, Z, NEQNS
	COMPLEX*16 CURAMP, OLDAMP, AY, A
	DO I = 1, NEQNS
	    CURAMP(I, NO) = OLDAMP(I)
	END DO
	HDEPTH = .5*DEPTH
	ZEND = ZBEG
	DO M = 1, NO
	    Z = ZEND + HDEPTH
	    CALL SETUP(Z)
	    CALL PROPAG(DEPTH, NO)
	    ZEND = ZEND + DEPTH
	END DO
	RETURN
	END
**==SWWF.FOR
	SUBROUTINE SWWF
C
C  CALCULATES THE REFRACTIVE INDEX AND ITS GRADIENT USING THE
C  SEN-WYLLER FORMULA -- WITH FIELD
C  NEEDS SUBROUTINE FSW AND FUNCTIONS C AND S.
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /RIN   / COLL, FIELD, SPITZ, N2, PNPR, PNPTH, PNPPH, 
     &			PNPVR, PNPVTH, PNPVPH, NNP, PNPT, GAM, PGP(6), 
     &			POLAR, LPOLAR, FACTR, SPACE
	COMMON /RINC  / MODRIN
	CHARACTER*10 MODRIN(3)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /YY    / Y, PYPR, PYPTH, PYPPH, YR, PYRPR, PYRPT, PYRPP, 
     &			YTH, PYTPR, PYTPT, PYTPP, YPH, PYPPR, PYPPT, 
     &			PYPPP
	COMMON /YYC   / MODY
	CHARACTER*10 MODY
	COMMON /ZZ    / Z, PZPR, PZPTH, PZPPH
	COMMON /ZZC   / MODZ
	CHARACTER*10 MODZ
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(VR, R(4)), (VTH, R(5)), (VPH, R(6))
	EQUIVALENCE(RAY, W(1)), (EARTHR, W(2))
	LOGICAL SPACE
c 	COMPLEX*16 D2GA
C KSH 25.1.91 sun compile warning : local variable D2GA not used
        COMPLEX*16 N2, PNPR, PNPTH, PNPPH, PNPVR, PNPVTH, PNPVPH, NNP, 
     &		   PNPT, GAM, PGP, POLAR, LPOLAR, I, U, RAD, D, PNPPS, 
     &		   PNPX, PNPY, PNPZ, UX, UX2, ALPHA, BETA, GAMMA, A, B, 
     &		   C, TEMP1, TEMP2, TEMP3, ALPOAL, BEPOBE, GAPOGA, CB2, 
     &		   N2M1, U2, DAL, DBET, DGAM, DADY, DADZ, DBDY, 
     &		   DBDZ, DCDY, DCDZ, DUDZ, DT1DX, DT1DY, DT1DZ, DT1DPS, 
     &		   DT2DX, DT2DY, DT2DZ, DT2DPS, DRADDX, DRADDY, DRADDZ, 
     &		   DRDDPS, DDDX, DDDY, DDDZ, DDDPS, UPX
C     DATA COLL/1./,FIELD/1./,SPITZ/0./,I/(0.,1.)/,ABSLIM/1.E-5/,  GAM/(
C    +0.,0.)/,LPOLAR/(0.,0.)/,                                   Y/0./,P
C    +YPR/0./,PYPTH/0./,PYPPH/0./,YR/0./,PYRPR/0./,        PYRPT/0./,PYR
C    +PP/0./,YTH/0./,PYTPR/0./,PYTPT/0./,             PYTPP/0./,YPH/0./,
C    +PYPPR/0./,PYPPT/0./,PYPPP/0./,Z/0./,PZPR/0./,PZPTH/0./,PZPPH/0./
C     DATA MODRIN/'      SEN-','WYLLER FOR','MULA'/
C
C     REPLACE ABOVE COMMON BLOCKS
C
	IF(ICODE.EQ.0)THEN
	    MODRIN(1) = '      SEN-'
	    MODRIN(2) = 'WYLLER FOR'
	    MODRIN(3) = 'MULA      '
	    COLL = 1.0
	    FIELD = 1.0
	    SPITZ = 0.0
	    I = (0.0, 1.0)
	    ABSLIM = 1.E-5
	    GAM = (0.0, 0.0)
	    LPOLAR = (0.0, 0.0)
	    Y = 0.0
	    PYPR = 0.0
	    PYPTH = 0.0
	    PYPPH = 0.0
	    YR = 0.0
	    PYRPR = 0.0
	    PYRPT = 0.0
	    PYRPP = 0.0
	    YTH = 0.0
	    PYTPR = 0.0
	    PYTPT = 0.0
	    PYTPP = 0.0
	    YPH = 0.0
	    PYPPR = 0.0
	    PYPPT = 0.0
	    PYPPP = 0.0
	    Z = 0.0
	    PZPR = 0.0
	    PZPTH = 0.0
	    PZPPH = 0.0
	    RETURN
	END IF
C
C
C
	PXPT = 0.0
	PXPPH = 0.0
	PXPTH = 0.0
	PXPR = 0.0
	X = 0.0
	CALL ELECTX
	CALL MAGY
	OPY = 1. + Y
	OMY = 1. - Y
	CALL COLFRZ
	Z2 = Z*Z
	CALL FSW(1./Z, ALPHA, DAL)
	ALPOAL = DAL/ALPHA
	CALL FSW(OMY/Z, BETA, DBET)
	BEPOBE = DBET/BETA
	CALL FSW(OPY/Z, GAMMA, DGAM)
	GAPOGA = DGAM/GAMMA
	U = Z/ALPHA
	DUDZ = (1.+ALPOAL/Z)/ALPHA
	U2 = U*U
	UX = U - X
	UPX = U + X
	B = ALPHA/BETA
	DBDY = B*BEPOBE/Z
	DBDZ = -B*(ALPOAL-OMY*BEPOBE)/Z2
	C = ALPHA/GAMMA
	DCDY = -C*GAPOGA/Z
	DCDZ = -C*(ALPOAL-OPY*GAPOGA)/Z2
	A = .5*(B+C) - 1.
	DADY = .5*(DBDY+DCDY)
	DADZ = .5*(DBDZ+DCDZ)
	TEMP3 = (1.-B*C)*U2 + A*U*UPX
	V2 = VR**2 + VTH**2 + VPH**2
	VDOTY = VR*YR + VTH*YTH + VPH*YPH
	YL2 = VDOTY**2/V2
	YT2 = Y**2 - YL2
	Y2 = Y*Y
	S2PSI = YT2/Y2
	C2PSI = YL2/Y2
	UX2 = UX*UX
	CB2 = (C-B)**2
	TEMP1 = TEMP3*S2PSI
	DT1DX = A*U*S2PSI
	DT1DY = (U*UPX*DADY-U2*(B*DCDY+C*DBDY))*S2PSI
	DT1DZ = (2.*U*DUDZ*(1.-B*C+A)+A*X*DUDZ-U2*(B*DCDZ+C*DBDZ)
     &		+U*UPX*DADZ)*S2PSI
C
C  (1/YLYT) D/DPSI(TEMP1)
C
	DT1DPS = 2.*TEMP1/YT2
	TEMP2 = U2*CB2*UX2*C2PSI
	DT2DX = -2.*UX*U2*CB2*C2PSI
	DT2DY = 2.*U2*UX2*C2PSI*(C-B)*(DCDY-DBDY)
	DT2DZ = 2.*U2*UX2*C2PSI*(C-B)*(DCDZ-DBDZ)
     &		 + 2.*TEMP2*(1./U+1./UX)*DUDZ
C
C  (1/YLYT) D/DPSI(TEMP2)
C
	DT2DPS = -2.*TEMP2/YL2
	RAD = RAY*SQRT(TEMP1**2+TEMP2)
	DRADDX = (TEMP1*DT1DX+.5*DT2DX)/RAD
	DRADDY = (TEMP1*DT1DY+.5*DT2DY)/RAD
	DRADDZ = (TEMP1*DT1DZ+.5*DT2DZ)/RAD
C
C  (1/YLYT) D/DPSI(RAD)
C
	DRDDPS = (TEMP1*DT1DPS+.5*DT2DPS)/RAD
	D = 2.*U*UX*(1.+A) - TEMP1 + RAD + 2.*A*U*X*S2PSI
	DDDX = -2.*U - DT1DX + DRADDX + 2.*A*U*S2PSI
	DDDY = 2.*U*UX*DADY - DT1DY + DRADDY + 2.*U*S2PSI*DADY
	DDDZ = 2.*(1.+A)*DUDZ*(U+UX) + 2.*U*UX*DADZ - DT1DZ + DRADDZ + 
     &	       2.*X*S2PSI*(A*DUDZ+U*DADZ)
C
C  (1/YLYT) D/DPSI(D)
C
	DDDPS = -DT1DPS + DRDDPS + 2.*A*U*X/Y2
	N2M1 = -2.*X*(UX+U*A*S2PSI)/D
	N2 = 1. + N2M1
C
C  N D/DX(N)
C
	PNPX = .5*N2M1*(1./X-DDDX/D) + X/D
C
C  N D/DY(N)
C
	PNPY = -X*U*S2PSI/D*DADY - .5*N2M1/D*DDDY
C
C  N D/DZ(N)
C
	PNPZ = -X*(1.+A*S2PSI)/D*DUDZ - X*U*S2PSI/D*DADZ - 
     &	       .5*N2M1/D*DDDZ
C
C  (N/YLYT) D/DPSI(N)
C
	PNPPS = -X*U*A/(D*Y2) - .5*N2M1/D*DDDPS
	YLV = VDOTY/V2
C
C  (YLYT) D/DR(PSI)
C
	PPSPR = YL2/Y*PYPR - (VR*PYRPR+VTH*PYTPR+VPH*PYPPR)*YLV
C
C  (YLYT) D/DTHETA(PSI)
C
	PPSPTH = YL2/Y*PYPTH - (VR*PYRPT+VTH*PYTPT+VPH*PYPPT)*YLV
C
C  (YLYT) D/DPHI(PSI)
C
	PPSPPH = YL2/Y*PYPPH - (VR*PYRPP+VTH*PYTPP+VPH*PYPPP)*YLV
	PNPR = PNPX*PXPR + PNPY*PYPR + PNPZ*PZPR + PNPPS*PPSPR
	PNPTH = PNPX*PXPTH + PNPY*PYPTH + PNPZ*PZPTH + PNPPS*PPSPTH
	PNPPH = PNPX*PXPPH + PNPY*PYPPH + PNPZ*PZPPH + PNPPS*PPSPPH
	PNPVR = PNPPS*(VR*YL2/V2-YLV*YR)
	PNPVTH = PNPPS*(VTH*YL2/V2-YLV*YTH)
	PNPVPH = PNPPS*(VPH*YL2/V2-YLV*YPH)
	NNP = N2 - (2.*X*PNPX+Y*PNPY+Z*PNPZ)
	PNPT = PNPX*PXPT
	POLAR = I*(TEMP1-RAD)*Y*SQRT(V2)/(U*UX*(C-B)*VDOTY)
	COSPSI = VDOTY/(Y*SQRT(V2))
	LPOLAR = (.5*I*(C-B)*POLAR+A*COSPSI)*SQRT(S2PSI)
     &		 /(POLAR*(UX*(1.+.5*I*(C-B)*COSPSI*POLAR)+A*(U-X*C2PSI))
     &		 )
	SPACE = DREAL(N2).EQ.1. .AND. ABS(DIMAG(N2)).LT.ABSLIM
	RETURN
	END
**==SYNF.FOR
	SUBROUTINE SYNF
C
C  CALCULATES THE REFRACTIVE INDEX AND ITS GRADIENT USING THE
C  SEN-WYLLER FORMULA -- NO FIELD
C  NEEDS SUBROUTINES FGSW AND FSW AND FUNCTIONS C AND S.
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /RIN   / COLL, FIELD, SPITZ, N2, PNPR, PNPTH, PNPPH, 
     &			PNPVR, PNPVTH, PNPVPH, NNP, PNPT, GAM, PGP(6), 
     &			POLAR, LPOLAR, FACTR, SPACE
	COMMON /RINC  / MODRIN
	CHARACTER*10 MODRIN(3)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /ZZ    / Z, PZPR, PZPTH, PZPPH
	COMMON /ZZC   / MODZ
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 MODZ
	LOGICAL SPACE
c 	COMPLEX*16 DF
C KSH 25.1.91 sun compile warning local variable DF not used
	COMPLEX*16 N2, PNPR, PNPTH, PNPPH, PNPVR, PNPVTH, PNPVPH, NNP, 
     &		   PNPT, GAM, PGP, POLAR, LPOLAR, PNPX, PNPZ, F1, 
     &		   G1, DG1
C     DATA COLL/1./,FIELD/0./,SPITZ/0./,POLAR/(0.,1.)/,LPOLAR/(0., 0.)/,
C    +                                                             GAM/(
C    +0.,0.)/,ABSLIM/1.E-5/,PNPVR/0./,PNPVTH/0./,PNPVPH/0./,  Z/0./,PZPR
C    +/0./,PZPTH/0./,PZPPH/0./
C     DATA MODRIN/'      SEN-','WYLLER FOR','MULA'/
C
C     TO REPLACE THE ABOVE DATA STATEMENTS
C
	IF(ICODE.EQ.0)THEN
	    MODRIN(1) = '      SEN-'
	    MODRIN(2) = 'WYLLER FOR'
	    MODRIN(3) = 'MULA      '
	    COLL = 1.0
	    FIELD = 0.0
	    SPITZ = 0.0
	    POLAR = (0.0, 1.0)
	    LPOLAR = (0.0, 0.0)
	    GAM = (0.0, 0.0)
	    ABSLIM = 1.E-5
	    PNPVR = 0.0
	    PNPVTH = 0.0
	    PNPVPH = 0.0
	    Z = 0.0
	    PZPR = 0.0
	    PZPTH = 0.0
	    PZPPH = 0.0
	    RETURN
	END IF
C
C
C
	PXPT = 0
	PXPPH = 0.0
	PXPTH = 0.0
	PXPR = 0.0
	X = 0.0
	CALL ELECTX
	CALL COLFRZ
	CALL FGSW(1./Z, F1, DF1, G1, DG1)
	N2 = 1. - X*G1
	PNPX = -.5*G1
	PNPZ = .5*X*DG1/Z**2
	PNPR = PNPX*PXPR + PNPZ*PZPR
	PNPTH = PNPX*PXPTH + PNPZ*PZPTH
	PNPPH = PNPX*PXPPH + PNPZ*PZPPH
	NNP = N2 - (2.*X*PNPX+Z*PNPZ)
	PNPT = PNPX*PXPT
	SPACE = DREAL(N2).EQ.1. .AND. ABS(DIMAG(N2)).LT.ABSLIM
	RETURN
	END
**==TABLEX.FOR
	SUBROUTINE TABLEX
C
C  CALCULATES ELECTRON DENSITY AND GRADIENT FROM PROFILES HAVING
C  THE SAME FORM AS THOSE USED BY CROFTS RAY TRACING PROGRAM
C  MAKES AN EXPONENTIAL EXTRAPOLATION DOWN USING THE BOTTOM TWO POINTS
C  NEEDS SUBROUTINE GAUSEL
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	DIMENSION HPC(250), FN2C(250), ALPHA(250), BETA(250), GAMMA(250)
     &		  , DELTA(250), SLOPE(250), MAT(4, 5)
	CHARACTER*8 IDENTY(10)
	COMMON /CONST / PI, PIT2, PID2, DEGS, RAD, K, DUM(2)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /XX2   / PX2PR
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /PERTOF/ISWITCH
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (READFN, W(100)), 
     &		    (PERT, W(150))
	REAL*8 MAT, K
C
	IF(ICODE.EQ.0)THEN
	    MODX(1) = 'TABLEX'
	    RETURN
	END IF
C
	IF(READFN.EQ.0.)GO TO 300
	READFN = 0.
	READ(15, 99001, END=100)IDENTY
99001	FORMAT(BZ, 10A8)
100	CONTINUE
	READ(15, 99002, END=200)NOC, (HPC(I), FN2C(I), I=1, NOC)
99002	FORMAT(BZ, I4/(F8.2,E12.4))
200	CONTINUE
	WRITE(16, 99003)CHAR(12),IDENTY
99003	FORMAT(A1, 10X, 10A8)
	WRITE(16, 99004)(HPC(I), FN2C(I), I=1, NOC)
99004	FORMAT(12X, 'HEIGHT', 7X, 'ELECTRON DENSITY'/(1X,F20.10,E20.10))
	IF(FN2C(1).NE.0.)A = LOG(FN2C(2)/FN2C(1))/(HPC(2)-HPC(1))
	FN2C(1) = K*FN2C(1)
	FN2C(2) = K*FN2C(2)
	SLOPE(1) = A*FN2C(1)
	SLOPE(NOC) = 0.
	NMAX = 1
	DO I = 2, NOC
	    IF(FN2C(I).GT.FN2C(NMAX))NMAX = I
	    IF(I.NE.NOC)THEN
		FN2C(I+1) = K*FN2C(I+1)
		DO J = 1, 3
		    M = I + J - 2
		    MAT(J, 1) = 1.
		    MAT(J, 2) = HPC(M)
		    MAT(J, 3) = HPC(M)**2
		    MAT(J, 4) = FN2C(M)
		END DO
		CALL GAUSEL(MAT, 4, 3, 4, NRANK)
		IF(NRANK.LT.3)GO TO 400
		SLOPE(I) = MAT(2, 4) + 2.*MAT(3, 4)*HPC(I)
	    END IF
	    DO J = 1, 2
		M = I + J - 2
		MAT(J, 1) = 1.
		MAT(J, 2) = HPC(M)
		MAT(J, 3) = HPC(M)**2
		MAT(J, 4) = HPC(M)**3
		MAT(J, 5) = FN2C(M)
		L = J + 2
		MAT(L, 1) = 0.
		MAT(L, 2) = 1.
		MAT(L, 3) = 2.*HPC(M)
		MAT(L, 4) = 3.*HPC(M)**2
		MAT(L, 5) = SLOPE(M)
	    END DO
	    CALL GAUSEL(MAT, 4, 4, 5, NRANK)
	    IF(NRANK.LT.4)GO TO 400
	    ALPHA(I) = MAT(1, 5)
	    BETA(I) = MAT(2, 5)
	    GAMMA(I) = MAT(3, 5)
	    DELTA(I) = MAT(4, 5)
	END DO
	HMAX = HPC(NMAX)
	NH = 2
300	CONTINUE
	H = R(1) - EARTHR
	F2 = F*F
	PXPR = 0.
	PX2PR = 0.
	PXPPH = 0.
	PXPTH = 0.0
	IF(H.LT.HPC(1))THEN
	    NH = 2
	    X = 0.
	    IF(FN2C(1).NE.0.)THEN
		X = FN2C(1)*EXP(A*(H-HPC(1)))/F2
		PXPR = A*X
		PX2PR = A*PXPR
	    END IF
	ELSE IF(H.GE.HPC(NOC))THEN
	    X = FN2C(NOC)/F2
	ELSE
	    NSTEP = 1
	    IF(H.LT.HPC(NH-1))NSTEP = -1
350	    CONTINUE
	    IF(HPC(NH-1).LE.H .AND. H.LT.HPC(NH))THEN
		X = (ALPHA(NH)+H*(BETA(NH)+H*(GAMMA(NH)+H*DELTA(NH))))
     &		    /F2
		PXPR = (BETA(NH)+H*(2.*GAMMA(NH)+H*3.*DELTA(NH)))/F2
		PX2PR = (2.*GAMMA(NH)+6.*H*DELTA(NH))/F2
	    ELSE
		NH = NH + NSTEP
		GO TO 350
	    END IF
	END IF
	IF(PERT.NE.0. .AND. ISWITCH.NE.0)CALL ELECT1
	RETURN
400	CONTINUE
	WRITE(16, 99005)I, HPC(I)
99005	FORMAT(' THE', I4, 
     &	     'TH POINT IN THE ELECTRON DENSITY PROFILE HAS T''HE HEIGHT'
     &	     , F8.2, ' KM, WHICH IS THE SAME AS ANOTHER POINT.')
	CALL EXIT
	END
**==TABLEZ.FOR
	SUBROUTINE TABLEZ
C
C  CALCULATES COLLISION FREQUENCY AND ITS GRADIENT FROM PROFILES
C  HAVING THE SAME FORM AS THOSE USED BY CROFTS RAY TRACING PROGRAM
C  MAKES AN EXPONENTIAL EXTRAPOLATION DOWN USING THE BOTTOM TWO POINTS
C  NEEDS SUBROUTINE GAUSEL
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	DIMENSION HPC(100), FN2C(100), ALPHA(100), BETA(100), GAMMA(100)
     &		  , DELTA(100), MAT(4, 5), SLOPE(100)
	DIMENSION IDENTY(10)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /ZZ    / Z, PZPR, PZPTH, PZPPH
	COMMON /ZZC   / MODZ
	CHARACTER*10 MODZ
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (READNU, W(250))
	REAL*8 MAT
C
	IF(ICODE.EQ.0)THEN
	    MODZ = 'TABLEZ'
	    RETURN
	END IF
	IF(READNU.EQ.0.0)GO TO 300
	READNU = 0.
	READ(15, 99001, END=100)IDENTY
99001	FORMAT(BZ, 10A8)
100	CONTINUE
	READ(15, 99002, END=200)NOC, (HPC(I), FN2C(I), I=1, NOC)
99002	FORMAT(BZ, I4/(F8.2,E12.4))
200	CONTINUE
	WRITE(16, 99003)IDENTY
99003	FORMAT('1', 10X, 10A8)
	WRITE(16, 99004)(HPC(I), FN2C(I), I=1, NOC)
99004	FORMAT(12X, 'HEIGHT', 7X, 
     &	       'COLLISION FREQUENCY '/(1X,F20.10,E20.10))
	IF(FN2C(1).NE.0.)A = LOG(FN2C(2)/FN2C(1))/(HPC(2)-HPC(1))
	FN2C(1) = FN2C(1)/PIT2*1.E-6
	FN2C(2) = FN2C(2)/PIT2*1.E-6
	SLOPE(1) = A*FN2C(1)
	SLOPE(NOC) = 0.
	DO I = 2, NOC
	    IF(I.NE.NOC)THEN
		FN2C(I+1) = FN2C(I+1)/PIT2*1.E-6
		DO J = 1, 3
		    M = I + J - 2
		    MAT(J, 1) = 1.
		    MAT(J, 2) = HPC(M)
		    MAT(J, 3) = HPC(M)**2
		    MAT(J, 4) = FN2C(M)
		END DO
		CALL GAUSEL(MAT, 4, 3, 4, NRANK)
		IF(NRANK.LT.3)GO TO 500
		SLOPE(I) = MAT(2, 4) + 2.*MAT(3, 4)*HPC(I)
	    END IF
	    DO J = 1, 2
		M = I + J - 2
		MAT(J, 1) = 1.
		MAT(J, 2) = HPC(M)
		MAT(J, 3) = HPC(M)**2
		MAT(J, 4) = HPC(M)**3
		MAT(J, 5) = FN2C(M)
		L = J + 2
		MAT(L, 1) = 0.
		MAT(L, 2) = 1.
		MAT(L, 3) = 2.*HPC(M)
		MAT(L, 4) = 3.*HPC(M)**2
		MAT(L, 5) = SLOPE(M)
	    END DO
	    CALL GAUSEL(MAT, 4, 4, 5, NRANK)
	    IF(NRANK.LT.4)GO TO 500
	    ALPHA(I) = MAT(1, 5)
	    BETA(I) = MAT(2, 5)
	    GAMMA(I) = MAT(3, 5)
	    DELTA(I) = MAT(4, 5)
	END DO
	JUP = 2
300	CONTINUE
	H = R(1) - EARTHR
	IF(H.GE.HPC(1))THEN
	    IF(H.GE.HPC(NOC))GO TO 400
	    NSTEP = 1
	    IF(H.LT.HPC(JUP-1))NSTEP = -1
350	    CONTINUE
	    IF(HPC(JUP-1).GT.H .OR. H.GE.HPC(JUP))THEN
		JUP = JUP + NSTEP
		IF(JUP.GE.2)THEN
		    IF(JUP.GE.NOC)GO TO 400
		    GO TO 350
		END IF
	    ELSE
		Z = (ALPHA(JUP)+H*(BETA(JUP)+H*(GAMMA(JUP)+H*DELTA(JUP))
     &		    ))/F
		PZPR = (BETA(JUP)+H*(2.*GAMMA(JUP)+H*3.*DELTA(JUP)))/F
		RETURN
	    END IF
	END IF
	JUP = 2
	Z = FN2C(1)*EXP(A*(H-HPC(1)))/F
	PZPR = A*Z
	RETURN
400	CONTINUE
	JUP = NOC
	Z = FN2C(NOC)/F
	PZPR = 0.
	RETURN
500	CONTINUE
	WRITE(16, 99005)I, HPC(I)
99005	FORMAT(' THE', I4, 
     &	  'TH POINT IN THE COLLISION FREQUENCY PROFILE HA''S THE HEIGHT'
     &	  , F8.2, ' KM, WHICH IS THE SAME AS ANOTHER POINT.')
	CALL EXIT
	END
**==TEST.FOR
	SUBROUTINE TEST(W1, W2)
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CTEST / EL, RANGE, ELB, ELS, RX, HH, RR, AZD, RGM, IEND, JX
	DATA MINUS/0/
	IF(ABS(RANGE-W1).LE.W2)THEN
	    IEND = 1
	ELSE IF((RANGE-W1).LT.0.0)THEN
	    IF(MINUS.NE.1)THEN
		ELS = -ELS
		MINUS = 1
	    END IF
	ELSE IF(MINUS.EQ.1)THEN
	    MINUS = 0
	    ELS = -0.5*ELS
	ELSE
	    E = RX - RANGE
	    IF((RANGE-E).LE.W1)THEN
		X = RANGE - W1
		IF(X.LT.0.1*E)THEN
		    ELS = ELS*0.05
		ELSE IF(X.LT.0.3*E)THEN
		    ELS = ELS*0.1
		ELSE IF(X.LT.0.5*E)THEN
		    ELS = ELS*0.15
		ELSE IF(X.LT.0.7*E)THEN
		    ELS = ELS*0.2
		ELSE
		    ELS = ELS*0.25
		END IF
	    END IF
	END IF
	RETURN
	END
**==TORUS.FOR
	SUBROUTINE TORUS
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (C0, W(151)), (A, W(152)), 
     &		    (B, W(153)), (BETA, W(154)), (H0, W(155))
	REAL*8 LAMBDA
	DATA PDPP/0./
C
	IF(ICODE.EQ.0)THEN
	    MODX(2) = ' TORUS'
	    RETURN
	END IF
	IF(X.EQ.0. .AND. PXPR.EQ.0. .AND. PXPTH.EQ.0. .AND. PXPPH.EQ.0.)
     &	   RETURN
	IF(C0.EQ.0.)RETURN
	R0 = EARTHR + H0
	Z = R(1) - R0
	LAMBDA = R0*(R(2)-PID2)
	SINBET = SIN(BETA)
	COSBET = COS(BETA)
	P = LAMBDA*COSBET + Z*SINBET
	Y = Z*COSBET - LAMBDA*SINBET
	DELTA = C0*EXP(-(P/A)**2-(Y/B)**2)
	DEL1 = DELTA + 1.
	PDPR = -2.*DELTA*(P*SINBET/A**2+Y*COSBET/B**2)
	PDPT = -2.*DELTA*(P*R0*COSBET/A**2-Y*R0*SINBET/B**2)
	PXPR = PXPR*DEL1 + X*PDPR
	PXPTH = PXPTH*DEL1 + X*PDPT
	PXPPH = PXPPH*DEL1 + X*PDPP
	X = X*DEL1
	RETURN
	END

	
! 	************************************************************************************************
! 	************************************************************************************************
	SUBROUTINE TRACE
C
C  CALCULATES THE RAY PATH
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	DIMENSION ROLD(20), DROLD(20)
	COMMON /RK    / STEP, E1MAX, E1MIN, E2MAX, E2MIN, FACT, 
     &			RSTART, N, MODE
	COMMON /FLG   / NTYP, NEWWR, NEWWP, PENET, LINES, IHOP, HPUNCH
	COMMON /TRAC  / SMT, GROUND, PERIGE, THERE, MINDIS, NEWRAY
	COMMON /RIN   / COLL, FIELD, SPITZ, N2, PNP(8), GAM, PGP(6), 
     &			POLAR, LPOLAR, FACTR, SPACE
	COMMON /RINC  / MODRIN
	CHARACTER*10 MODRIN(3)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(20), T, STP, DRDT(20)/WW/W0, W(400)
	COMMON /WWC   / ID
	CHARACTER*10 ID(10)
	COMMON /PLT   / XL,XR,YB,YT,RESET
	LOGICAL SPACE, HOME, WASNT, GROUND, PERIGE, THERE, 
     &		MINDIS, NEWWR, NEWWP, PENET, NEWRAY, WAS
	COMPLEX*16 N2, PNP, GAM, PGP, POLAR, LPOLAR
	REAL*8 MAXSTP
	EQUIVALENCE(EARTHR, W(2)), (RCVRH, W(20)), (HOP, W(22)), 
     &		    (MAXSTP, W(23)), (SKIP, W(71)), (RAYSET, W(72)), 
     &		    (PLT, W(81)), (HTOP, W(28))

	NHOP = HOP
	NSKIP = SKIP
	MAXS = MAXSTP
	RSTART = 1.
	CALL HASEL
	HOME = DRDT(1)*(R(1)-EARTHR-RCVRH).GE.0.
C
C  ********* IHOP=0 TELLS PRINTR TO PRINT HEADING AND PUNCH A TRANSMITTER
C  ********* RAYSET AND TELLS RAYPLT TO START A NEW RAY
C
	IHOP = 0
	CALL PRINTR('XMTR    ', 0.D0)
	IF(PLT.NE.0.)CALL RAYPLT
	HTMAX = 0.
	NEWRAY = .TRUE.
	THERE = R(1) - EARTHR.EQ.RCVRH
C
C  ********* LOOP ON NUMBER OF HOPS
C
	IHOP = 1
100	CONTINUE
	PENET = .FALSE.
	APHT = RCVRH
C
C  ********* LOOP ON MAXIMUM NUMBER OF STEPS PER HOP
C
	DO J = 1, MAXS
		H = R(1) - EARTHR
		IF(ABS(H-RCVRH).GT.ABS(APHT-RCVRH))APHT = H
		HTMAX = MAX(H, HTMAX)
		IF(SPACE)THEN
			CALL REACH
		RSTART = 1.
		H = R(1) - EARTHR
		IF(ABS(H-RCVRH).GT.ABS(APHT-RCVRH))APHT = H
			HTMAX = MAX(H, HTMAX)
			IF(SPACE)THEN
				IF(PERIGE)CALL PRINTR('PERIGEE ', 0.D0)
				IF(THERE)GO TO 200
				IF(MINDIS)GO TO 150
				IF(GROUND)GO TO 250
				IF(PLT.NE.0.)CALL RAYPLT
				IF(PERIGE)GO TO 400
			END IF
		END IF
		DO L = 1, N
			ROLD(L) = R(L)
			DROLD(L) = DRDT(L)
		END DO
		TOLD = T
		WAS = THERE
		CALL RKAM
		H = R(1) - EARTHR
		THERE = .FALSE.
		WASNT = .NOT.HOME
		HOME = DRDT(1)*(H-RCVRH).GE.0.
		TMP = (DRDT(1)-DROLD(1))*(T-TOLD)
		SMT = 0.
	    IF(TMP.NE.0.)SMT = 0.5*(R(1)-ROLD(1)+0.5*TMP)**2/ABS(TMP)
	    IF(((H-RCVRH)*(ROLD(1)-EARTHR-RCVRH).LT.0..AND..NOT.WAS)
     &		.OR. (WAS.AND.DRDT(1)*DROLD(1).LT.0..AND.HOME))THEN
C
C  ********* RAY CROSSED RECEIVER HEIGHT
C
			CALL BACKUP(RCVRH)
			THERE = .TRUE.
			GO TO 200
* sto, 14/6/94. abs(h-rcvrh).lt.30.0 added in order to prevent the program
*from finishing early when due to small fluctuations in input Ne profiles
*and strange Ne perturbations ray direction can change.
* Modified (sto, 02/07/02) to allow user input to change condition. By
* default w(48) is set to EARTHR otherwise it gives the difference in height
* between ray and receiver before grazing is allowed.
* if this new line is used then the test data does not terminate correctly
	    ELSE IF(HOME .AND. WASNT .AND. ABS(H-RCVRH).LT.w(48))THEN
C
C  ********* RAY MAY HAVE MADE A CLOSEST APPROACH
C
	write(16,'(I4, A, 2e12.4)')J, ' Closest aproach ', H, RCVRH

		CALL GRAZE(RCVRH)
		IF(THERE)GO TO 200
	    ELSE IF(H.LT.0. .OR. DRDT(1).GT.0. .AND. DROLD(1)
     &		    .LT.0. .AND. SMT.GT.H)THEN
C
C  ********* RAY WENT UNDERGROUND
C
		CALL BACKUP(0.D0)
		GO TO 250
	    ELSE
		IF(DROLD(1).LT.0. .AND. DRDT(1).GT.0.)
     &		   CALL PRINTR('PERIGEE ', 0.D0)
		IF(DROLD(1).GT.0. .AND. DRDT(1).LT.0.)
     &		   CALL PRINTR('APOGEE  ', 0.D0)
		IF(DROLD(2)*DRDT(2).LT.0.)CALL PRINTR('MAX LAT ', 0.D0)
		IF(DROLD(3)*DRDT(3).LT.0.)CALL PRINTR('MAX LONG', 0.D0)
		DO I = 4, 6
		    IF(ROLD(I)*R(I).LT.0.)CALL PRINTR('WAVE REV', 0.D0)
		END DO
		GO TO 300
	    END IF
150	    CONTINUE
	    DRDT(1) = 0.
	    HPUNCH = R(1) - EARTHR
	    CALL PRINTR('MIN DIST', RAYSET)
	    IF(PLT.NE.0.)CALL RAYPLT
	    IF(IHOP.GE.NHOP)RETURN
	    IHOP = IHOP + 1
	    CALL PRINTR('MIN DIST', RAYSET)
	    GO TO 500
200	    CONTINUE
	    R(1) = EARTHR + RCVRH
	    HTMAX = MAX(RCVRH, HTMAX)
	    HPUNCH = APHT
	    CALL PRINTR('RCVR    ', RAYSET)
	    IF(PLT.NE.0.)CALL RAYPLT
	    IF(RCVRH.NE.0.)GO TO 500
	    IF(IHOP.GE.NHOP)RETURN
	    IHOP = IHOP + 1
	    APHT = RCVRH
C
C  ********* GROUND REFLECT
C
250	    CONTINUE
	    R(1) = EARTHR
	    IF(ABS(RCVRH).GT.ABS(APHT-RCVRH))APHT = 0.
	    R(4) = ABS(R(4))
	    DRDT(1) = ABS(DRDT(1))
	    RSTART = 1.
	    HPUNCH = HTMAX
	    CALL PRINTR('GRND REF', RAYSET)
	    HTMAX = 0.
	    IF(RCVRH.NE.0.)THEN
		H = 0.
		THERE = .FALSE.
	    ELSE
		THERE = .TRUE.
		HPUNCH = APHT
		CALL PRINTR('RCVR    ', RAYSET)
		GO TO 500
	    END IF
C
C  *********
C
300	    CONTINUE
	    IF(PLT.NE.0.)CALL RAYPLT

C put in a bodge KSH 17.1.91
c	    HMAX = 900.0
c this was needed when using the double chapman model, and the code for
c that subroutine has been amended instead
* original code
c 	    IF(H.GT.HMAX .AND. H.GT.RCVRH .AND. DRDT(1).GT.0.)GO TO 600
* htop is the maximum height which rays should be plotted to, w(28)
 	    IF(H.GT.HMAX .AND. H.GT.HTOP .AND. DRDT(1).GT.0.)GO TO 600
	    IF(MOD(J,NSKIP).EQ.0)CALL PRINTR('        ', 0.D0)
400	END DO
C
C  ********* EXCEEDED MAXIMUM NUMBER OF STEPS
C
	HPUNCH = H
	CALL PRINTR('STEP MAX', RAYSET)
	RETURN
C
C  *********
C
500	CONTINUE
	HOME = .TRUE.
	IHOP = IHOP + 1
	IF(IHOP.GT.NHOP)GO TO 700
	GO TO 100
C
C  ********* RAY PENETRATED
C
600	CONTINUE
	PENET = .TRUE.
	HPUNCH = H
	CALL PRINTR('PENETRAT', RAYSET)
700	CONTINUE
	RETURN
	END


! 	************************************************************************************************
! 	************************************************************************************************
	SUBROUTINE TRAVRS
C
C  GIVEN THE FLUID DYNAMICAL AMPLITUDES IN OLDAMP(I) AT A SPECIFIED
C  ALTITUDE (E.G., TOPATM, THE ALTITUDE OF THE "TOP" OF THE ATMOSPHERE),
C  SUBROUTINE TRAVRS CALCULATES THE FLUID AMPLITUDES AT ANOTHER SPECIFIED
C  ALTITUDE (E.G., BOTATM) AND PUTS THEM IN OLDAMP(I). THE ALGORITHM USED
C  BASED ON THE PROCEDURE OF APPROXIMATING THE SMOOTHLY VARYING ATMOSPHER
C  BY A SERIES OF HOMOGENOUS LAYERS (S.H. FRANCIS, J.GEOPHYS.RES., VOL. 7
C  P. 2278FF, 1973).
C  ......IEXTR=0 FOR HALVING, DOUBLING AND EXTRAPOLATION
C  IEXTR=1 FOR NO HALVING, NO DOUBLING, NO EXTRAPOLATION
C  IEXTR=2 FOR HALVING AND DOUBLING BUT NO EXTRAPOLATION
C  IEXTR>2 FOR EXTRAPOLATION BUT NO HALVING OR DOUBLING.
C
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /INPT  / KX, OMEGA, INTGR(16), NLAYER
	COMMON /OUTPT / K(6), AP(6), ARB(16), ARBC(16)
	COMMON /TRAV  / TOPATM, BOTATM, TOLMAX, TOLMIN, CURAMP(6, 2), 
     &			OLDAMP(6), ZBEG, ZEND, DEPTHO, Z, NEQNS
	COMPLEX*16 K, KX, CURAMP, OLDAMP, AP, ARBC
	EQUIVALENCE(INDEX, INTGR(1)), (NTEST, INTGR(3)), 
     &		    (MPRINT, INTGR(4)), (IEXTR, INTGR(6)), 
     &		    (KCHECK, INTGR(8)), (KEY, INTGR(10))
	IEND = 0
	DEPTH = DEPTHO
	HDEPTH = DEPTH/2.
	ZBEG = TOPATM
	IF(DEPTH.GT.0.)ZBEG = BOTATM
C
C  .......BEGIN TRAVERSAL OF ATMOSPHERE, STOPPING AT THE END OF THE
C  ATMOSPHERE, OR WHEN NLAYER SLABS HAVE BEEN USED.
C
	DO N = 1, NLAYER
C
C  .......CHECK TO SEE IF END OF ATMOSPHERE IS NEAR
C
	    IF(DEPTH.LT.0.)THEN
		IF(BOTATM-ZBEG.LT.DEPTH)GO TO 50
		DEPTH = BOTATM - ZBEG
	    ELSE
		IF(TOPATM-ZBEG.GT.DEPTH)GO TO 50
		DEPTH = TOPATM - ZBEG
	    END IF
	    IEND = 1
	    HDEPTH = .5*DEPTH
C
C  .......ADVANCE ONE LAYER
C
50	    CONTINUE
	    CALL STEP(DEPTH, 1)
	    IF(IEXTR.NE.1)THEN
		CALL STEP(HDEPTH, 2)
		IF(IEXTR.LE.2 .AND. IEND.LE.0)THEN
C
C  .......COMPUTE RELATIVE DIFFERENCE BETWEEN FULL AND HALF DEPTH SOLUTION
C
		    ERROR = 0.
		    DO I = 1, NEQNS
			ERROR = MAX(ERROR, ABS(CURAMP(I,2)-CURAMP(I,1))
     &				/ABS(CURAMP(I,2)+CURAMP(I,1)))
		    END DO
		    ERROR = ERROR + ERROR
		    IF(ERROR.LE.TOLMIN)THEN
C
C  .......DOUBLE THE DEPTH; ERROR IS TOO SMALL
C
			IF(DEPTH.LE.1.E7)THEN
			    HDEPTH = DEPTH
			    DEPTH = DEPTH + DEPTH
			END IF
		    END IF
		    IF(ERROR.GE.TOLMAX)THEN
			IF(ABS(DEPTH).GE.1.E5)THEN
C
C  .......HALVE THE DEPTH; ERROR IS TOO LARGE.
C
			    DEPTH = HDEPTH
C
C  ......REDO THE SLAB
C
			    HDEPTH = .5*DEPTH
			    GO TO 50
			ELSE
			    WRITE(6, 99001)DEPTH, ERROR, TOLMAX, ZBEG
99001			    FORMAT(
     &				  '0ERROR > TOLERANCE EVEN WITH DEPTH ='
     &				  , -5PF6.2, ' KM'/' ERROR =', 0PF7.4, 
     &				  ', TOLERANCE =', 
     &				  0PF7.4/' COMPUTATION HAS REACHED Z =', 
     &				  -5PF7.2, ' KM')
			END IF
		    END IF
		END IF
	    END IF
C
C  ......UPDATE OLDAMP AND ZBEG TO PREPARE FOR NEXT SLAB
C
	    DO I = 1, NEQNS
		OLDAMP(I) = CURAMP(I, 1)
		IF(IEXTR.NE.1 .AND. IEXTR.NE.2)OLDAMP(I) = CURAMP(I, 2)
     &		    + (CURAMP(I,2)-CURAMP(I,1))/3.
	    END DO
	    ZBEG = ZEND
	    IF(MPRINT.NE.0)THEN
		IF(MOD(N,MPRINT).EQ.0)CALL OUTPUT
	    END IF
	    IF(IEND.GT.0)RETURN
	END DO
	WRITE(6, 99002)
99002	FORMAT(
     &'0----NLAYER STEPS WERE PERFORMED, BUT THE TRAVERSE IS NOTCOMPLETE
     &.----')
	RETURN
	END
**==TROUGH.FOR
	SUBROUTINE TROUGH
C
C  A PERTURBATION TO AN ELECTRON DENSITY MODEL
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(A, W(151)), (B, W(152)), (ALAT, W(153)), 
     &		    (FACTOR, W(154))
C
	IF(ICODE.EQ.0)THEN
	    MODX(2) = 'TROUGH'
	    RETURN
	END IF
	IF(X.EQ.0. .AND. PXPR.EQ.0. .AND. PXPTH.EQ.0. .AND. PXPPH.EQ.0.)
     &	   RETURN
	IF(A.EQ.0.)RETURN
	ANGLE = R(2) + ALAT - PID2
	WIDTH = B
	IF(ANGLE.GT.0.)WIDTH = FACTOR*B
	ANGLE = ANGLE/WIDTH
	DELTA = A*EXP(-ANGLE**2)
	DEL1 = DELTA + 1.
	PXPR = PXPR*DEL1
	PXPTH = PXPTH*DEL1 - 2.*X*ANGLE*DELTA/WIDTH
	PXPPH = PXPPH*DEL1
	X = X*DEL1
	RETURN
	END
**==VCHAPX.FOR
	SUBROUTINE VCHAPX
C
C  CHAPMAN LAYER WITH VARIABLE SCALE HEIGHT
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /XX2   / PX2PR
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /PERTOF/ISWITCH
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (FC, W(101)), (HM, W(102))
     &		    , (CHI, W(103)), (PERT, W(150))
C
	IF(ICODE.EQ.0)THEN
	    MODX(1) = 'VCHAPX'
	    RETURN
	END IF
	HMAX = HM
	PXPR = 0.
	X = 0.0
	PXPPH = 0.
	PXPTH = 0.0
	PX2PR = 0.
	H = R(1) - EARTHR
	IF(H.GT.0.)THEN
	    TAU = (HM/H)**CHI
	    X = (FC/F)**2*SQRT(TAU)*EXP(0.5*(1.-TAU))
	    PXPR = .5*X*(TAU-1.)*CHI/H
	    PX2PR = PXPR*(CHI/2./H*(TAU-1.)-1./H) - X*CHI**2*TAU/2./H**2
	END IF
	IF(PERT.NE.0. .AND. ISWITCH.NE.0)CALL ELECT1
	RETURN
	END
**==WAVE.FOR
	SUBROUTINE WAVE1
C
C  PERTURBATION TO AN ALPHA-CHAPMAN ELECTRON DENSITY MODEL
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (Z0, W(151)), (SH, W(152)), 
     &		    (DELTA, W(153)), (VSUBX, W(154)), (LAMBDX, W(155)), 
     &		    (LAMBDZ, W(156)), (TP, W(157))
	REAL*8 LAMBDX, LAMBDZ
C
	IF(ICODE.EQ.0)THEN
	    MODX(2) = ' WAVE '
	    RETURN
	END IF
	IF(X.EQ.0. .AND. PXPR.EQ.0. .AND. PXPTH.EQ.0. .AND. PXPPH.EQ.0.)
     &	   RETURN
	IF(DELTA.EQ.0. .OR. SH.EQ.0.)RETURN
	IF(W(150).EQ.0.)RETURN
	
	H = R(1) - EARTHR
	EXPQ = EXP(-((H-Z0)/SH)**2)
	TMP = PIT2*(TP+(PID2-R(2))*EARTHR/LAMBDX+H/LAMBDZ)
	SINW = SIN(TMP)
	COSW = SIN(PID2-TMP)
	CONS = 1.0 + DELTA*EXPQ*COSW
	IF(H.NE.0.)PXPR = PXPR*CONS - 
     &			  X*DELTA*EXPQ*(2.0/SH**2*(H-Z0)*COSW+
     &			  PIT2/LAMBDZ*SINW)
	PXPTH = PXPTH*CONS + X*DELTA*PIT2*EARTHR/LAMBDX*SINW*EXPQ
	PXPPH = PXPPH*CONS
	PXPT = 0.
	IF(VSUBX.NE.0.)PXPT = -PIT2*VSUBX/LAMBDX*X*DELTA*EXPQ*SINW
	X = X*CONS

c	write(*,*) DELTA*PIT2*EARTHR/LAMBDX*SINW*EXPQ

	RETURN
	END
**==WAVE2.FOR
	SUBROUTINE WAVE2
C
C  PERTURBATION TO AN ANY ELECTRON DENSITY MODEL
C
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (Z0, W(151)), (SH, W(152)), 
     &		    (DELTA, W(153)), (VSUBX, W(154)), (LAMBDX, W(155)), 
     &		    (LAMBDZ, W(156)), (TP, W(157)), (TH00, W(158)), 
     &		    (THC, W(159)), (VGX, W(161))
	REAL*8 LAMBDX, LAMBDZ
C
	IF(ICODE.EQ.0)THEN
	    MODX(2) = ' WAVE2'
	    RETURN
	END IF
	IF(X.EQ.0. .AND. PXPR.EQ.0. .AND. PXPTH.EQ.0. .AND. PXPPH.EQ.0.)
     &	   RETURN
	IF(DELTA.EQ.0. .OR. SH.EQ.0.)RETURN
	H = R(1) - EARTHR
	TH0 = TH00 + LAMBDX*TP*VGX/VSUBX/EARTHR
	EXPR = EXP(-((H-Z0)/SH)**2)
	EXPTH = EXP(-((R(2)-TH0)/THC)**2)
	WW = PIT2*(TP+(PID2-R(2))*EARTHR/LAMBDX+H/LAMBDZ)
	SINW = SIN(WW)
	COSW = COS(WW)
	E = DELTA*EXPR*EXPTH
	CONS = 1.0 + E*COSW
	PXPR = PXPR*CONS - X*E*2.*(COSW*(H-Z0)/SH**2+PI/LAMBDZ*SINW)
	PXPTH = PXPTH*CONS + 2.*E*X*(PI*EARTHR*SINW/LAMBDX-(R(2)-TH0)
     &		/THC**2*COSW)
	PXPPH = PXPPH*CONS
	PXPT = X*(2.0*E*VGX/EARTHR*COSW*(R(2)-TH0)
     &	       /THC**2-PIT2*VSUBX*E/LAMBDX*SINW)
	X = X*CONS
	RETURN
	END
**==BLOB.FOR
**	P.H 28/4/93	A 3D Gausssian blob

c	= A exp(-[pi/2 - theta - lambda]2) exp(-[phi - nu]2) exp(-[R - R0 - z]2)
c                [        Wt           ]        [   Wp   ]        [    H     ]

c	150 = 1 to use perturbation
c	151 = A relative amplitude of blob
c	152 = Latitude of blob
c	153 = Longitude of blob
c	154 = Latitudinal blob width
c	155 = Longitudinal blob width
c	156 = Height of blob
c	157 = Scale height of blob
	SUBROUTINE BLOB
C
C  A PERTURBATION TO AN ELECTRON DENSITY MODEL
C
	IMPLICIT DOUBLE PRECISION(A-H,O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /FLG   / NTYP, NEWWR, NEWWP, PENET, LINES, IHOP, HPUNCH
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (A, W(151)), (ALAT, W(152)), (ALON, W(153)),
     &  (WLAT, W(154)), (WLON, W(155)), (HEIGHT, W(156)), (SH, W(157)),
     &  (PLAT,W(24)),(PLON,W(25))
	REAL BLAT,BLON,BHT
        LOGICAL NEWWP,NEWWR,PENET

	IF(ICODE.EQ.0)THEN
	    MODX(2) = ' BLOB '
	    RETURN
	END IF
	IF(X.EQ.0. .AND. PXPR.EQ.0. .AND. PXPTH.EQ.0. .AND. PXPPH.EQ.0.)
     &	   RETURN
	IF(W(150) .EQ. 0.0) RETURN

C trap errors

	IF (A      .EQ. 0.0)	RETURN
	IF (SH     .LE. 0.0)	STOP 'ERROR IN BLOB'
	IF (WLAT   .LE. 0.0)	STOP 'ERROR IN BLOB'
	IF (WLON   .LE. 0.0)	STOP 'ERROR IN BLOB'
	IF (HEIGHT .LE. 0.0)	STOP 'ERROR IN BLOB'

	GALAT=PID2-ALAT
	GALON=ALON-PI
	IF(R(3) .LT. 0.) THEN
		GALON=ALON-PIT2
	ENDIF
c Following code converts from geographic to geomagnetic coordinates
        IF(NEWWP)THEN
           SP=SIN(PLAT)
           CP=COS(PLAT)
           SDPH=SIN(ALON-PLON)
           CDPH=COS(ALON-PLON)
           SL=SIN(ALAT)
           CL=COS(ALAT)
           GALAT=ACOS(CDPH*CP*CL+SP*SL)
           GALON=ATAN2(SDPH*CL, CDPH*SP*CL-CP*SL)
        ENDIF

	BLAT = R(2) - GALAT
	BLAT = BLAT / WLAT
C	write(*,*) R(2), GALAT, WLAT

	BLON = (R(3) - GALON)
	BLON = BLON / WLON
	BLON = 1.

	BHT  = (HEIGHT - (R(1)-EARTHR))
	BHT  = BHT / SH
C	write(*,*) R(1)-EARTHR, HEIGHT, SH

	DELTA = A * EXP(-BLAT**2) * EXP(-BLON**2) * EXP(-BHT**2)
	DEL1  = DELTA + 1.0

	PXPR  = PXPR  * DEL1 - 2.0*X*BHT *DELTA / SH
	PXPTH = PXPTH * DEL1 - 2.0*X*BLAT*DELTA / WLAT
	PXPPH = PXPPH * DEL1 - 2.0*X*BLON*DELTA / WLON
	X = X * DEL1
C	IF(abs(delta) .gt. 0.15)THEN
C		WRITE(*,*)'blob values: glat,wlat,height,sh,delta',galat,wlat,height,sh,delta,newwp
C	ENDIF

	RETURN
	END
**==BLOB2.FOR
**	STO 11/10/93	A 3D sum of two Gausssians blob
**			This has been written to model the Ne perturbation
**			found during heating (Stocker et al. 1992)

c  = A exp(-[pi/2 - theta - lambda]2) exp(-[phi - nu]2) exp(-[R - R0 - z]2)
c           [        Wt           ]        [   Wp   ]        [    H     ]
c  + B exp [wt2] exp [wp2] exp(-[r - r0 -z1]2)
c 
c	150 = 1 to use perturbation
c	151 = A relative amplitude of gaussian 1
c	152 = Latitude of both gaussians
c	153 = Longitude of both gaussians
c	154 = Latitudinal blob width (both)
c	155 = Longitudinal blob width (both)
c	156 = Height of blob 1
c	157 = Scale height of blob 1
c	158 = Amplitude of gaussian 2
c	159 = Height of blob 2
c	160 = Scale height of blob 2
	SUBROUTINE BLOB2
C
C  A PERTURBATION TO AN ELECTRON DENSITY MODEL
C
	IMPLICIT DOUBLE PRECISION(A-H,O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /FLG   / NTYP, NEWWR, NEWWP, PENET, LINES, IHOP, HPUNCH
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (A, W(151)), (ALAT, W(152)), (ALON, W(153)),
     &  (WLAT, W(154)), (WLON, W(155)), (HEIGHT, W(156)), (SH, W(157)),
     &  (PLAT,W(24)),(PLON,W(25)),(A2,W(158)),(HGT2,W(159)),(SH2,W(160))
	REAL BLAT,BLON,BHT
        LOGICAL NEWWP,NEWWR,PENET

	IF(ICODE.EQ.0)THEN
	    MODX(2) = ' BLOB2 '
	    RETURN
	END IF
	IF(X.EQ.0. .AND. PXPR.EQ.0. .AND. PXPTH.EQ.0. .AND. PXPPH.EQ.0.)
     &	   RETURN
	IF(W(150) .EQ. 0.0) RETURN

C trap errors
	IF (A      .EQ. 0.0)	RETURN
	IF (SH     .LE. 0.0)	STOP 'ERROR IN BLOB2'
	IF (WLAT   .LE. 0.0)	STOP 'ERROR IN BLOB2'
	IF (WLON   .LE. 0.0)	STOP 'ERROR IN BLOB2'
	IF (HEIGHT .LE. 0.0)	STOP 'ERROR IN BLOB2'
c Following code converts from geographic to geomagnetic coordinates
        IF(NEWWP)THEN
           SP=SIN(PLAT)
           CP=COS(PLAT)
           SDPH=SIN(ALON-PLON)
           CDPH=COS(ALON-PLON)
           SL=SIN(ALAT)
           CL=COS(ALAT)
           GALAT=ACOS(CDPH*CP*CL+SP*SL)
           GALON=ATAN2(SDPH*CL, CDPH*SP*CL-CP*SL)
        ENDIF

	BLAT = R(2) - GALAT
	BLAT = BLAT / WLAT

	BLON = (R(3) - GALON)
	BLON = BLON / WLON

	BHT  = (HEIGHT - (R(1)-EARTHR))
	BHT  = BHT / SH

	BHT2 = (HGT2 - (R(1)-EARTHR))
	BHT2 = BHT2 / SH2

	DELTA = A * EXP(-BLAT**2) * EXP(-BLON**2) * EXP(-BHT**2)
	DELTA2 = A2 * EXP(-BLAT**2) * EXP(-BLON**2) * EXP(-BHT2**2)
	DEL1  = DELTA + DELTA2 + 1.0

	PXPR  = PXPR  * DEL1 - 2.0*X*BHT *DELTA / SH - 2.0*X*BHT2*DELTA2/SH2
	PXPTH = PXPTH * DEL1 - 2.0*X*BLAT*DELTA / WLAT - 2.0*BLAT*DELTA2/WLAT
	PXPPH = PXPPH * DEL1 - 2.0*X*BLON*DELTA / WLON - 2.0*BLON*DELTA2/WLON
	X = X * DEL1

	RETURN
	END
**==RING.FOR
**	sto 2/12/93	A 3D toroidal perturbation centred on an
**			given position. Designed to simulate the
**			effect of radio wave heating the E-region 
**			when the centre of the heater beam penetrates
**			to the F region.

* For a point P,
*
* = A * exp (- ( (r - r0) / s1)**2) * exp (- ( (h - h0)/ s2)**2))
*
* where r0 is the ring radius, h0 the ring height, s1 and s2 the
* horizontal and vertical scale sizes, h is the point height
* and r the point distance from the ring centre and given by
*
* r**2 = h0**2 * ( (t -tc)**2 + ( (p - pc) * cos tc)**2)

c	150 = 1 to use perturbation
c	151 = Max. amplitude of ring
c	152 = Latitude of ring centre
c	153 = Longitude of ring centre
c	154 = ring radius
c	155 = ring height
c	156 = horizontal scale size
c	157 = vertical scale size
	SUBROUTINE RING
C
C  A PERTURBATION TO AN ELECTRON DENSITY MODEL
C
	IMPLICIT DOUBLE PRECISION(A-H,O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /FLG   / NTYP, NEWWR, NEWWP, PENET, LINES, IHOP, HPUNCH
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)),(A, W(151)),(ALAT, W(152)),(ALON, W(153)),
     &  (RRAD, W(154)),(RHGT, W(155)),(HSCALE, W(156)),(VSCALE, W(157)),
     &  (PLAT,W(24)),(PLON,W(25))
	REAL BLAT,BLON,BHT
        LOGICAL NEWWP,NEWWR,PENET

	IF(ICODE.EQ.0)THEN
	    MODX(2) = ' RING '
	    RETURN
	END IF
	IF(X.EQ.0. .AND. PXPR.EQ.0. .AND. PXPTH.EQ.0. .AND. PXPPH.EQ.0.)
     &	   RETURN
	IF(W(150) .EQ. 0.0) RETURN

C trap errors
	IF (A.EQ.0.0) RETURN
	IF (RRAD.LE. 0.0 .OR. VSCALE.LE.0.0 .OR. RHGT.LE.0.0 .OR.
     &      HSCALE.LE.0.0) STOP 'ERROR IN BLOB'

c Following code converts from geographic to geomagnetic coordinates
        IF(NEWWP)THEN
           SP=SIN(PLAT)
           CP=COS(PLAT)
           SDPH=SIN(ALON-PLON)
           CDPH=COS(ALON-PLON)
           SL=SIN(ALAT)
           CL=COS(ALAT)
           GALAT=ACOS(CDPH*CP*CL+SP*SL)
           GALON=ATAN2(SDPH*CL, CDPH*SP*CL-CP*SL)
        ENDIF

	BLAT = R(2) - GALAT		! i.e. deltaTheta
	BLON = (R(3) - GALON)		! i.e. deltaPhi
	BHT  = (RHGT - (R(1)-EARTHR))	! i.e. deltaHeight
	BHT  = BHT / VSCALE

	HZERO = RHGT + EARTHR
* approximate distance from point to centre of ring
* note - latitude correction is sin since galat is colatitude
* corrected 10/1/94, sto.
	DTOPT = BLAT*BLAT + BLON*BLON*SIN(GALAT)*SIN(GALAT)

	DELTA = A * EXP(-(HZERO*SQRT(DTOPT)-RRAD)**2/(HSCALE*HSCALE))
     &            * EXP(-BHT**2)
	DEL1  = DELTA + 1.0

	PXPR  = PXPR  * DEL1 - 2.0*X*BHT *DELTA / VSCALE
	PXPTH = PXPTH * DEL1 - 2.0*X*DELTA*HZERO*BLAT/(HSCALE*HSCALE)*
     &          (HZERO*SQRT(DTOPT)-RRAD)/SQRT(DTOPT)
	PXPPH = PXPPH * DEL1 - 2.0*X*DELTA*HZERO*SIN(GALAT)*SIN(GALAT)*
     &        BLON/(HSCALE*HSCALE)*(HZERO*SQRT(DTOPT)-RRAD)/SQRT(DTOPT)
	X = X * DEL1

	RETURN
	END
**==TROUGH2.FOR
* Written by sto (20/8/96)
*   This is a slightly more realistic trough model than the one which
* came with the original code. In this version which allows the model
* of Halcrow and Nisbet (1977) to be approximated to there are three
* main regions. 1) Inside the trough where the plasma freq (f) is
* reduced to Amax of its original value. 2) At the boundaries of the
* trough where the perturbation varies as a half-Gaussian with distance
* from the edge of the trough. 3) Outside the trough where the
* perturbation is zero.
*   The edges of the trough run parallel to lines of latitude and
* longitude (i.e. it is a rectangle when plotted in lat and long).
*
* Modified to include height variation, (sto, 16/4/02)
*
*   The input variables are
*		W151 		= Amax (peak perturbation)
*		W152,153	= Left and right longitude, respectively
*		W154,155	= Upper and lower latitude boundaries
*		W156,157	= Longitudinal and latitudinal scale lengths
*       W158,159	= Centre and scale height of perturbation (set 158 to
*					  0 or -ve to switch off height variation)
* 
*   The regions referred to below are defined as follows,
*		1: 		inside the trough boundaries (pert=Amax)
*		2,3:	adjacent to the latitude boundaries (to and bot)
*		4,5:	adjacent to the longitude boundaries (left & right)
*		6-9:	at a corner (top left, top right, bot left, bot right)
	SUBROUTINE TROUGH2
C
C  A PERTURBATION TO AN ELECTRON DENSITY MODEL
C
	IMPLICIT DOUBLE PRECISION(A-H,O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /FLG   / NTYP, NEWWR, NEWWP, PENET, LINES, IHOP, HPUNCH
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)),(A, W(151)),(TLLON, W(152)),
     &  (TRLON, W(153)),(TTLAT1, W(154)),(TBLAT1, W(155)),
     &  (LONSCA, W(156)),(LATSCA, W(157)),(HGTCEN, W(158)),
     &  (HGTSCA, W(159)),(PLAT, W(24)),(PLON, W(25)),
     &	(PTHGT, R(1)),(PTLAT, R(2)),(PTLON, R(3))
	REAL*8 LATSCA, LONSCA
	LOGICAL NEWWP,NEWWR,PENET

	IF(ICODE.EQ.0)THEN
	    MODX(2) = ' TROUGH2 '
	    RETURN
	END IF
* error traps
	IF(X.EQ.0. .AND. PXPR.EQ.0. .AND. PXPTH.EQ.0. .AND. PXPPH.EQ.0.)
     &	   RETURN
	IF(W(150).EQ.0.0 .OR. W(151).EQ.0.0) RETURN
	IF(TLLON.GT.TRLON) STOP 'TROUGH: left long is greater than right long'
	IF(TTLAT1.LT.TBLAT1) STOP 'TROUGH: top lat is below botton lat'
	IF(LONSCA.LE.0.0) STOP 'TROUGH: Longitudinal scale is nagative'
	IF(LATSCA.LE.0.0) STOP 'TROUGH: Latitudinal scale is negative'
***NEW CODE
	IF(HGTCEN.GT.0.0 .AND. HGTSCA.LE.0.0)
     &	STOP 'TROUGH: Height scale is negative and centre is +ve'
     
* Note input coordinates are effectively GEOMAGNETIC, so...
* convert lats to co-lats
	TBLAT=PID2-TTLAT1
	TTLAT=PID2-TBLAT1
		
* find region in which the point lies (see comments above)...
	IF(PTLAT.LE.TTLAT .AND. PTLAT.GE.TBLAT .AND. PTLON.GE.TLLON
     &	.AND. PTLON.LE.TRLON)THEN
			IREGON=1
	ELSEIF(PTLAT.GT.TTLAT .AND. PTLON.GE.TLLON .AND.
     & PTLON.LE.TRLON)THEN
			IREGON=2
			REFLAT=TTLAT
	ELSEIF(PTLAT.LT.TBLAT .AND. PTLON.GE.TLLON .AND.
     & PTLON.LE.TRLON)THEN
			IREGON=3
			REFLAT=TBLAT
	ELSEIF(PTLON.LT.TLLON .AND. PTLAT.LE.TTLAT .AND.
     & PTLAT.GE.TBLAT)THEN
			IREGON=4
			REFLON=TLLON
	ELSEIF(PTLON.GT.TRLON .AND. PTLAT.LE.TTLAT .AND.
     & PTLAT.GE.TBLAT)THEN
			IREGON=5
			REFLON=TRLON
	ELSEIF(PTLAT.GT.TTLAT .AND. PTLON.LT.TLLON)THEN
			IREGON=6
			REFLAT=TTLAT
			REFLON=TLLON
	ELSEIF(PTLAT.GT.TTLAT .AND. PTLON.GT.TRLON)THEN
			IREGON=7
			REFLAT=TTLAT
			REFLON=TRLON
	ELSEIF(PTLAT.LT.TBLAT .AND. PTLON.LT.TLLON)THEN
			IREGON=8
			REFLAT=TBLAT
			REFLON=TLLON
	ELSEIF(PTLAT.LT.TBLAT .AND. PTLON.GT.TRLON)THEN
			IREGON=9
			REFLAT=TBLAT
			REFLON=TRLON
	ENDIF

* work out how far from centre height we are (if using model, otherwise 0)
	IF(HGTCEN.LE.0.0)THEN
		DHGT=0.0
		HGTSCA=1.0
	ELSE
		DHGT=(PTHGT-EARTHR-HGTCEN)/HGTSCA
	ENDIF
	
	IF(IREGON.EQ.1)THEN
		DELTA=A * EXP(-DGHT**2)
		DEL1=1.0+A
		X=X*DEL1
		PXPR=PXPR*DEL1 - 2.0*X*DHGT*DELTA/HGTSCA
		PXPTH=PXPTH*DEL1
		PXPPH=PXPPH*DEL1
	ELSEIF(IREGON.EQ.2 .OR. IREGON.EQ.3)THEN
		DLAT=(PTLAT-REFLAT)/LATSCA
		DELTA=A * EXP(-DLAT**2) * EXP(-DGHT**2)
		DEL1=1.0+DELTA
		PXPR=PXPR*DEL1 - 2.0*X*DHGT*DELTA/HGTSCA
		PXPTH=PXPTH*DEL1 - 2.0*X*DLAT*DELTA/LATSCA
		PXPPH=PXPPH*DEL1
		X=X*DEL1
	ELSEIF(IREGON.EQ.4 .OR. IREGON.EQ.5)THEN
		DLON=(PTLON-REFLON)/LONSCA
		DELTA=A * EXP(-DLON**2) * EXP(-DGHT**2)
		DEL1=1.0+DELTA
		PXPR=PXPR*DEL1 - 2.0*X*DHGT*DELTA/HGTSCA
		PXPTH=PXPTH*DEL1
		PXPPH=PXPPH*DEL1 - 2.0*X*DLON*DELTA/LONSCA
		X=X*DEL1
	ELSEIF(IREGON.GE.6)THEN
		DLAT=(PTLAT-REFLAT)/LATSCA
		DLON=(PTLON-REFLON)/LONSCA
		DELTA=A * EXP(-DLAT**2) * EXP(-DLON**2) * EXP(-DGHT**2)
		DEL1=1.0+DELTA
		PXPR=PXPR*DEL1 - 2.0*X*DHGT*DELTA/HGTSCA
		PXPTH=PXPTH*DEL1 - 2.0*X*DLAT*DELTA/LATSCA
		PXPPH=PXPPH*DEL1 - 2.0*X*DLON*DELTA/LONSCA
		X=X*DEL1
	ENDIF

*	write(*,'(I4,3F9.5)')iregon,del1, dlat,dlon
	   	
	RETURN
	END
**==BLOBFAI.FOR
**	P.H 28/4/93	A 3D Gausssian blob
**  AJS 27/06/02 Modified to include sinusoidal irregularities.
c	= A exp(-[pi/2 - theta - lambda]2) exp(-[phi - nu]2) exp(-[R - R0 - z]2)
c            [        Wt           ]        [   Wp   ]        [    H     ]
c       cos(2pi(theta+inclination)) cos(2pi(phi+inclination))

c	150 = 1 to use perturbation
c	151 = A relative amplitude of blob
c	152 = Latitude of blob
c	153 = Longitude of blob
c	154 = Latitudinal blob width
c	155 = Longitudinal blob width
c	156 = Height of blob
c	157 = Scale height of blob
c   158 = Latitudinal separation of irregularities
c	159 = Longitudinal separation of irregularities
c	160 = Angle of inclination (lat)
c	161 = Angle of inclination (long)
	SUBROUTINE BLOBFAI
C
C  A PERTURBATION TO AN ELECTRON DENSITY MODEL
C
	IMPLICIT DOUBLE PRECISION(A-H,O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /FLG   / NTYP, NEWWR, NEWWP, PENET, LINES, IHOP, HPUNCH
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (A, W(151)), (ALAT, W(152)), (ALON, W(153)),
     &  (WLAT, W(154)), (WLON, W(155)), (HEIGHT, W(156)), (SH, W(157)),
     &  (SEPLAT, W(158)), (SEPLON, W(159)), (ANGLAT, W(160)),
     &  (ANGLON, W(161)), (PLAT,W(24)), (PLON,W(25))
	REAL BLAT,BLON,BHT, INCLAT, INCLON
	LOGICAL NEWWP,NEWWR,PENET

	IF(ICODE.EQ.0)THEN
	    MODX(2) = ' BLOB '
	    RETURN
	END IF
	IF(X.EQ.0. .AND. PXPR.EQ.0. .AND. PXPTH.EQ.0. .AND. PXPPH.EQ.0.)
     &	   RETURN
	IF(W(150) .EQ. 0.0) RETURN

C trap errors
	IF (A .EQ. 0.0) RETURN
	IF (SH .LE. 0.0) STOP 'ERROR IN BLOB'
	IF (WLAT .LE. 0.0 .OR. WLON.LE.0.0) STOP 'ERROR IN BLOB'
	IF (HEIGHT .LE. 0.0) STOP 'ERROR IN BLOB'
	IF (SEPLAT .LE. 0.0 .OR. SEPLON.LE.0.0) STOP 'ERROR IN BLOB'

c Following code converts from geographic to geomagnetic coordinates
	IF(NEWWP)THEN
		SP=SIN(PLAT)
		CP=COS(PLAT)
		SDPH=SIN(ALON-PLON)
		CDPH=COS(ALON-PLON)
		SL=SIN(ALAT)
		CL=COS(ALAT)
		GALAT=ACOS(CDPH*CP*CL+SP*SL)
		GALON=ATAN2(SDPH*CL, CDPH*SP*CL-CP*SL)
	ENDIF

	BLAT = (R(2) - GALAT)/WLAT
	BLON = (R(3) - GALON)/WLON
	BHT  = (HEIGHT - (R(1)-EARTHR))/SH

c CL represents correction for latitude (longitudinal lines converge)
	FLAT = (R(2) - GALAT) * COS(ANGLAT)/SEPLAT
	FLON = (R(3) - GALON) * COS(ANGLON)/(SEPLON/CL)
	INCLAT=(HEIGHT - (R(1)-EARTHR))*SIN(ANGLAT)/(SEPLAT*EARTHR)
	INCLON=(HEIGHT - (R(1)-EARTHR))*SIN(ANGLON)/(SEPLON*EARTHR/CL)
	
	DELTA_BLOB = A * EXP(-BLAT**2) * EXP(-BLON**2) * EXP(-BHT**2)
	CFAILAT=COS(PIT2*(FLAT+INCLAT))
	CFAILON=COS(PIT2*(FLON+INCLON))
	DELTA_FAI =  CFAILAT * CFAILON
	DELTA=DELTA_BLOB*DELTA_FAI
	DEL1  = DELTA + 1.0

	SFAILAT=SIN(PIT2*(FLAT+INCLAT))
	SFAILON=SIN(PIT2*(FLON+INCLON))

	PXPR  = PXPR  * DEL1 - 2.0*X*BHT *DELTA/SH
     & + PIT2*X*DELTA_BLOB*CFAILON*SFAILAT*SIN(ANGLAT)/(SEPLAT*EARTHR)
     & + PIT2*X*DELTA_BLOB*CFAILAT*SFAILON*SIN(ANGLON)/
     &  (SEPLON*EARTHR/CL)

	PXPTH = PXPTH * DEL1 - 2.0*X*BLAT*DELTA/WLAT - PIT2 *
     &	X*DELTA_BLOB*CFAILON*SFAILAT*COS(ANGLAT)/SEPLAT
     
	PXPPH = PXPPH * DEL1 - 2.0*X*BLON*DELTA/WLON - PIT2 *
     &	X*DELTA_BLOB*CFAILAT*SFAILON*COS(ANGLON)/(SEPLON/CL)

	X = X * DEL1

	RETURN
	END
**==HORPOS.FOR
	SUBROUTINE HORPOS(RXLAT,RXLON,CEA,ALPH2,SL,CTHR,STHR,CTHL,
     &                      STHL,PHR,PHL,CLR,SLR)

* This routine converts the coordinates rxlat,rxlon from geographic
* to magnetic and thence to plotting cordinates. Outout is cea, alph2
* and sl. See rayplt to see how this output is used.
* Other input is defined in rayplt.

	IMPLICIT DOUBLE PRECISION(A-H,O-Z)
	COMMON /CONST/ PI, PIT2, PID2, DUM(5)
	COMMON R(6)/WW/W0, W(400)
	EQUIVALENCE(PLAT,W(24)),(PLON,W(25))

* convert from geographic to geomagnetic coordinates
        SP=SIN(PLAT)
        CP=COS(PLAT)
        SDPH=SIN(RXLON-PLON)
        CDPH=COS(RXLON-PLON)
        SL=SIN(RXLAT)
        CL=COS(RXLAT)
        GRLAT=ACOS(CDPH*CP*CL+SP*SL)
        GRLON=ATAN2(SDPH*CL, CDPH*SP*CL-CP*SL)

* convert to plotting coordinates?
	STH = SIN(GRLAT)
	CTH = SIN(PID2-GRLAT)
	CR = CTHR*CTH + STHR*STH*SIN(PID2-(PHR-GRLON))
	CL = CTHL*CTH + STHL*STH*SIN(PID2-(PHL-GRLON))
	CEA = ATAN2(CR-CL*CLR, CL*SLR)
	IF(CL.GT.1.)CL = 1.
	SL = SQRT(1.-CL*CL)
	TMP1 = STH*SIN(GRLON-PHL)
	TMP2 = (CTH-CTHL*CL)/STHL
	ALPH2 = 0.
	IF(TMP1.NE.0. .OR. TMP2.NE.0.)ALPH2 = ATAN2(TMP1, TMP2)

	RETURN
	END


! ****************************************************************************
! ****************************************************************************
	SUBROUTINE c2geog(rinlat, rinlon, outlat, outlon)
! ****************************************************************************
! ****************************************************************************
* converts geomagnetic co-ordinates to geographic ones
* inlat and inlon are the input geomagnetic, outlat and outlon are
* the resulting geographic. Note longitudes close to the geographic
* pole have largish errors -in angle, but small in distance of course
* Input must be latitude and NOT co-latitude
* For a change this has been converted from IDL to FORTRAN (sto, 21/8/96)

	IMPLICIT DOUBLE PRECISION(A-H, O-Z)

	COMMON /CONST / PI, PIT2, PID2, DEGS, RAD, K, C, LOGTEN
	COMMON R(20), T, STP, DRDT(20)/WW/W0, W(400)
	REAL*8 K, LOGTEN
	
* geographic pole in geomagnetic coordinates
	glat=w(24)
	glon=180.0*PI/180.0
	plon=w(25)

	sp=SIN(glat)
	cp=COS(glat)
	sdph=SIN(rinlon-glon)
	cdph=COS(rinlon-glon)
	sl=SIN(rinlat)
	cl=COS(rinlat)
	outlat=ACOS(cdph*cp*cl+sp*sl)
	outlon=ATAN2(sdph*cl, cdph*sp*cl-cp*sl) + plon - glon

* check for negative longitudes - add 360 degrees (i.e. 2pi)
	IF(outlon .LT. 0.0) outlon=outlon+2.0*PI

	RETURN	
	END

	SUBROUTINE RRAYIP(numinp,frqinp,elvinp,aziinp)

* This routine reads in the specified ray information - for instance if
* a loop is NOT required but some specific instances are.

	REAL*8 frqinp(6000), elvinp(6000), aziinp(6000)
	CHARACTER*5 dummy
	
* read in the header (two lines of text)	
	READ(59,'(A)')dummy
	READ(59,'(A)')dummy
	numinp=1
	
  9	READ(59,'(5X,3F9.4)',END=19)frqinp(numinp),aziinp(numinp),elvinp(numinp)
	numinp=numinp+1
	IF(numinp.EQ.6001) STOP 'More than 6000 ray-positions - check RRAYIP'
	GOTO 9
	
 19	CONTINUE
 	numinp=numinp-1
 	IF(numinp.EQ.0) STOP 'No input ray data exists - check rayinp.dat'

	RETURN
	END	

	SUBROUTINE FNDBER(TXRAD,TXTHT,TXPHI,RXRAD,RXTHT,RXPHI,BARING)
* Given two sets of coordinates, (txrad,txtht,txphi) and (rxrad,rxtht,rxphi)
* in calculation cordinates (i.e. geomagnetic), this routine calculates
* the bearing as seen from the receiver. It is called where DOA information
* is required

	IMPLICIT DOUBLE PRECISION(A-H,O-Z)
	COMMON /CONST / PI, PIT2, PID2, DEGS, RAD, DUM(3)
	COMMON /WW/ W0, W(400)

* error traps...
	IF(TXTHT.EQ.RXTHT .AND.TXPHI.EQ.RXPHI) RETURN
	
	R=W(2)	! radius of earth

* convert coordinates to geographic
	CALL C2GEOG(PID2-TXTHT,TXPHI,TXLAT,TXLON)
	CALL C2GEOG(PID2-RXTHT,RXPHI,RXLAT,RXLON)

* find latitudes from colats (rclat,tclat are colats)...
	TLAT=PID2-TXLAT
	RLAT=PID2-RXLAT
	RCLAT=RXLAT
	RLONG=RXLON
	TCLAT=TXLAT
	TLONG=TXLON
	
* calculate path distance (km)
	COSDRAD1=DCOS(TCLAT)*DCOS(RCLAT)
	COSDRAD2=DSIN(TCLAT)*DSIN(RCLAT)*DCOS(RLONG-TLONG)
	COSDRAD=COSDRAD1+COSDRAD2
	DRAD=DACOS(COSDRAD)
	D=R*DRAD
	
* now to calculate the bearing of Rx wrt Tx (Btr) and Tx wrt Rx (Brt)
* one bearings is measured anticlockwise from north and will be converted
* to a clockwise bearing
* note that the sum of Brt and Btr is not normally equal to PI
	IF(RLAT.GT.-PID2.AND.RLAT.LT.PId2.AND.TLAT.GT.-PID2.AND.TLAT.LT.PID2) THEN
		COSBTR=(COS(RCLAT)-(COS(TCLAT)*COS(DRAD)))/(SIN(TCLAT)*SIN(DRAD))
		Btr=DACOS(COSBTR)
		COSBRT=(COS(TCLAT)-(COS(RCLAT)*COS(DRAD)))/(SIN(RCLAT)*SIN(DRAD))
		Brt=DACOS(COSBRT)
	ELSE IF(RLAT.EQ.PID2) THEN
		Brt=PI
		Btr=0.0
	ELSE IF(RLAT.EQ.-PID2) THEN
		Brt=0.0
		Btr=PI		
	ELSE IF(TLAT.EQ.PID2) THEN
		Brt=0.0
		Btr=PI
	ELSE IF(TLAT.EQ.-PID2) THEN
		Brt=PI
		Btr=0.0
	ENDIF
C if we have one or more polar terminals then the following applies
			
* is tx is west of rx or otherwise (corrected, sto, 30/8/96)
	IF((TLONG.GT.RLONG .AND. TLONG-RLONG.LT.PI) .OR. 
     &	(TLONG.LT.RLONG .AND. RLONG-TLONG.GT.PI)) THEN
		BTR=2*PI-BTR
	ELSEIF((RLONG.GT.TLONG .AND. RLONG-TLONG.LT.PI) .OR.
     &	(RLONG.LT.TLONG .AND. TLONG-RLONG.GT.PI)) THEN
		BRT=2*PI-BRT
	ENDIF
	
	BARING=BRT
		
	RETURN
	END

	SUBROUTINE WRITERAYS
*
*	Routine to write out the ray data and k-vector data to unformatted
* file. Headers are written elsewhere.
* THIS VERSION HAS BEEN SUPERCEDED BY WRITEUNF (see below) which
* gives a 15% speed improvement and 10% storage reduction
*
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
* stores ray information for writing to idl-optimised file (rays.dat)
	COMMON /STORAY/ RADPOS(500000), THTPOS(500000), PHIPOS(500000), NUMPOS
* stores k-vector information for weriting to kvect.dat
	COMMON /STOKVE/ RADVEC(500000), THTVEC(500000), PHIVEC(500000), NUMVEC
	COMMON /WW/W0, W(400)
	
	IF(w(301).EQ.0.0)THEN
     		WRITE(57,'(I4)')NUMPOS
     		WRITE(57,'(8F9.3)')(RADPOS(KK), KK=1,NUMPOS)
     		WRITE(57,'(8F9.5)')(THTPOS(KK), KK=1,NUMPOS)
		WRITE(57,'(8F9.5)')(PHIPOS(KK), KK=1,NUMPOS)
	ENDIF
	NUMPOS=0	! reset numpos for next ray
	
* write out k-vector data for kvect.dat
	IF(w(304).EQ.0.0)THEN
     	WRITE(60,'(I4)')NUMVEC
     	WRITE(60,'(8F9.5)')(RADVEC(KK), KK=1,NUMVEC)
     	WRITE(60,'(8F9.5)')(THTVEC(KK), KK=1,NUMVEC)
     	WRITE(60,'(8F9.5)')(PHIVEC(KK), KK=1,NUMVEC)
	ENDIF
	NUMVEC=0	! reset numvec for the next ray

	RETURN
	END

	SUBROUTINE WRITEUNF
*
*	Routine to write out the ray data, k-vector data and upper-hybrid data
* to unformatted files. Headers are written elsewhere.
* THIS ROUTINE SUPERCEDES WRITERAYS - it is faster and stores the data more
* compactly (and with greater precision).
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DUM(5)
* stores ray information for writing to idl-optimised file (rays.dat)
	COMMON /STORAY/ RADPOS(500000), THTPOS(500000), PHIPOS(500000), NUMPOS
* stores k-vector information for writing to kvect.dat
	COMMON /STOKVE/ RADVEC(500000), THTVEC(500000), PHIVEC(500000), NUMVEC
* stores upper-hybrid information for writing to uhybrid.dat
	COMMON /STOHYB/ UPPHYB(500000), NUMHYB
	COMMON R(6)/WW/W0, W(400)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /PERTOF/ISWITCH
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /lasse/ haswritten
	COMMON /IRI_BLK/ edensARR(500,500),edensFLAG, edensPOS(2,500),dayNe(24), 
     &		    dip(2,500)
c group path all along ray now collected in this common block,
c then spat out to unit 57.  tky
	common /tim/ grppth(500000)
	REAL*8 XAMBIE(4000), LATPOS(2000), LONPOS(2000)
	EQUIVALENCE (FREQ,W(6)), (TXLAT,W(4)), (TXLON,W(5))
	
	IF(w(301).EQ.0.0)THEN
     	WRITE(57)NUMPOS
     	WRITE(57)(RADPOS(KK), KK=1,NUMPOS),(THTPOS(KK), KK=1,NUMPOS),
     &    (PHIPOS(KK), KK=1,NUMPOS),(grppth(KK), KK=1,NUMPOS)
	ENDIF
	NUMPOS=0	! reset numpos for next ray
	
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++
C open file to write electron density profile in
C April 2009, lbnc
C++++++++++++++++
	IF(haswritten .eq. 0)THEN 
		IF(edensFLAG.eq.0)THEN
			WRITE(*,*)'Electron densities not loaded'
			return
		ENDIF
		OPEN(64,FILE='out/edens.dat', status='unknown')
! 		write(*,*)'Writting el. dens.'
		F2 = FREQ*FREQ
		XTMP = 0.
		ITMP = 0
		NUMPROF = 2500
 		DPROF = 5
		ALAT = 3.14/2.-TXLAT
		ALON = TXLON
		NALT = 500
		SALT = 60.
		DALT = 1.
		EALT = 560.
! 		write(*,*)ALAT,ALON,TXLAT,TXLON,nalt
		WRITE(64,99110)500
99110	FORMAT(I3)
		WRITE(64,99111)SALT,EALT,DALT,NALT
99111	FORMAT(3(1X,F5.1),I5)
		DO KM=1, NUMPROF, DPROF
			ITMP=(KM-1)/DPROF
C			Position on azimuth of ray
			LATPOS(ITMP) = PID2 - edensPOS(1,ITMP)*PI/180.
			LONPOS(ITMP) = edensPOS(2,ITMP)*PI/180.
			write(64,99112)(edensARR(GG,ITMP), GG=1,500)
99112		FORMAT(500E25.11)
		ENDDO
		write(64,99113)(LATPOS(GG), GG=1,500)
99113		FORMAT(500E19.11)
		write(64,99114)(LONPOS(GG), GG=1,500)
99114		FORMAT(500E19.11)
		CLOSE(64)
! 		write(*,*)'File edens.dat saved.'
		OPEN(65,FILE='day_edens.dat',status='unknown')
		write(65,99115)250.,edensPOS(1,250),edensPOS(2,250)
99115		FORMAT(3E19.11)
		write(65,99116)(dayNe(DD), DD=1,24)
99116		FORMAT(24E25.11)
		CLOSE(65)
		OPEN(66,FILE='dip.dat',status='unknown')
		WRITE(66,99110)500
		write(66,99117)(dip(1,GG), GG=1,500)
99117		FORMAT(500E19.11)
		write(66,99117)(dip(2,GG), GG=1,500)
		write(66,99113)(LATPOS(GG), GG=1,500)
		write(66,99114)(LONPOS(GG), GG=1,500)
		CLOSE(66)
	ENDIF
	haswritten=1
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	
* write out k-vector data for kvect.dat
	IF(w(304).EQ.0.0)THEN
     	WRITE(60)NUMVEC
     	WRITE(60)(RADVEC(KK), KK=1,NUMVEC),(THTVEC(KK), KK=1,NUMVEC),
     &    (PHIVEC(KK), KK=1,NUMVEC)
	ENDIF
	NUMVEC=0	! reset numvec for the next ray

* write out upper-hybrid information
	IF(w(308).EQ.0.0)THEN
		WRITE(62)NUMHYB
     	WRITE(62)(UPPHYB(KK), KK=1,NUMHYB)
	ENDIF
	NUMHYB=0	! reset numhyb for next ray
	
	RETURN
	END

	SUBROUTINE CALCUH

* routine to calculate the difference from the upper-hybrid matching
* condition for each point on the ray.
	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
* stores upper-hybrid information for writing to uhybrid.dat
	COMMON /STOHYB/ UPPHYB(500000), NUMHYB
	COMMON R(6)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /YY    / Y(16)
	
* can assume that current values in R are those required (which has been the
* case in all tests run)

* can also assume that values of X and Y(1) have been calculated for the
* current position (again all tests show this is so)
* In which case...

        if(numhyb.eq.500000)then
                return
        endif
	NUMHYB=NUMHYB+1
	UPPHYB(NUMHYB)=1.0-X-Y(1)*Y(1)
	
	RETURN
	END

	SUBROUTINE GROUNDOUT
* routine writes out the positions of the ground reflected waves and
* the k-vector if these options have been chosen.

	IMPLICIT DOUBLE PRECISION(A-H, O-Z)

	COMMON /WW/ W0, W(400)
	COMMON /STOGND/ GNDTHT(400000), GNDPHI(400000), GNDKTH(400000), 
     & GNDKPH(400000), GNDKRA(400000), NUMGND
	
* write out positions of ground reflections to groundpos (only if
* w(305) has not been set to 1
	IF(w(305).NE.1)THEN
		WRITE(61)NUMGND
		WRITE(61)(GNDTHT(ii), ii=1,NUMGND)
		WRITE(61)(GNDPHI(ii), ii=1,NUMGND)
	ENDIF

* write out k-vectors at ground reflection positions - if w(307) not set to 1
	IF(w(305).NE.1 .AND. w(307).NE.1)THEN
		WRITE(63)NUMGND
		WRITE(63)(GNDKTH(ii), ii=1,NUMGND)
		WRITE(63)(GNDKPH(ii), ii=1,NUMGND)
		WRITE(63)(GNDKRA(ii), ii=1,NUMGND)
	ENDIF

	RETURN
	END

	SUBROUTINE SUMHEAD(fname, fbeg, nfreq, fstep, azbeg, naz, azstep, elbeg, 
     &	nbeta, elstep, degs)
* routine to write out the headers to summary.dat and sigstr.dat

	IMPLICIT DOUBLE PRECISION(A-H, O-Z)

	COMMON /WW/ W0, W(400)
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	EQUIVALENCE(EARTHR, W(2))
	character*80 fname
	
	write(56,'(a)')fname
	write(56,'(3f8.3,6f8.2)')fbeg,fbeg+(nfreq-1)*fstep,fstep,
     &	    azbeg*degs,(azbeg+(naz-1)*azstep)*degs,azstep*degs,
     &	    elbeg*degs,(elbeg+(nbeta-1)*elstep)*degs,elstep*degs
* write out RING parameters to summary, if appropriate
	IF(INDEXP.EQ.12 .AND. W(150).NE.0.0)THEN
	    WRITE(56,'(A,F8.3,6F8.2)')'RING    ',W(151),W(152)*DEGS,
     &		W(153)*DEGS,(W(II),II=154,157)
	ELSE IF(INDEXP.EQ.10 .AND. W(150).NE.0.0)THEN
	    WRITE(56,'(A,F8.3,6F8.2)')'BLOB    ',W(151),W(152)*DEGS,
     &		W(153)*DEGS,W(154)*EARTHR,W(155)*EARTHR*COS(W(152)),
     &          (W(II),II=156,157)
	ENDIF
* output start and end, frequency, az. and el. to sigstr.dat
	write(55,'(3f8.3,6f8.2)')fbeg,fbeg+(nfreq-1)*fstep,fstep,
     &	    azbeg*degs,(azbeg+(naz-1)*azstep)*degs,azstep*degs,
     &	    elbeg*degs,(elbeg+(nbeta-1)*elstep)*degs,elstep*degs
* write out RING parameters, if appropriate
	IF(INDEXP.EQ.12 .AND. W(150).NE.0.0)THEN
	    WRITE(55,'(A,F8.3,6F8.2)')'RING    ',W(151),W(152)*DEGS,
     &		W(153)*DEGS,(W(II),II=154,157)
	ELSE IF(INDEXP.EQ.10 .AND. W(150).NE.0.0)THEN
	    WRITE(55,'(A,F8.3,6F8.2)')'BLOB    ',W(151),W(152)*DEGS,
     &		W(153)*DEGS,W(154)*EARTHR,W(155)*EARTHR*COS(W(152)),
     &          (W(II),II=156,157)
	ELSE IF(INDEXP.EQ.13 .AND. W(150).NE.0.0)THEN
	    WRITE(55,'(A,F8.3,8F8.2)')'TROUGH2 ',W(151),
     &          (W(II)*DEGS,II=152,157), W(158), W(159)
	ELSE
		WRITE(55,*)'No special perturbation model'
	ENDIF

	RETURN
	END

	SUBROUTINE SCATRAY(radk,thtk,phik,gndkth,gndkph,gndkra,
     &	scattype,relpow)

* this routine returns the relative power of a ray (dB) which has been scattered
* from the land (scattype=1) or sea (scattype=2). The functions are derived
* from Figure 8 of Miya and Kanaya, Radio propagation prediction considering
* scattering wave on earth's surface, Rep. Ion. Res., 9(1), pp1-15, 1955.

	IMPLICIT DOUBLE PRECISION(A-H, O-Z)

	IF(scattype.NE.1 .AND. scattype.NE.2)THEN
		relpow=0.0
	ELSE
* find angle between vectors from dot product, a.b=ab cos(theta)
		rmaginc=SQRT(gndkth*gndkth+gndkph*gndkph+gndkra*gndkra)
		rmagout=SQRT(radk*radk+thtk*thtk+phik*phik)
		cangle=(gndkra*radk+gndkth*thtk+gndkph*phik)/(rmaginc*rmagout)
c		angle=ACOSD(cangle)
		angle=DACOS(cangle)
* now work out the relative level (in dB) - see notes above
		IF(scattype.EQ.1.0)THEN
			relpow=-angle/6.0
		ELSE IF(scattype.EQ.2.0)THEN
			relpow=-0.52*angle
		ENDIF
c	write(*,'(8F9.4)')radk,thtk,phik,gndkra,gndkth,gndkph,angle,relpow
	ENDIF
	
	RETURN
	END

! *************************************************************************
! *************************************************************************
**==IRI.FOR===
! *************************************************************************
! *************************************************************************
	SUBROUTINE IRI

	IMPLICIT DOUBLE PRECISION(A-H, O-Z)
	COMMON /CONST / PI, PIT2, PID2, DEGS, RAD, DUM(3)
	COMMON /XX    / X, PXPR, PXPTH, PXPPH, PXPT, HMAX
	COMMON /XXC   / MODX
	CHARACTER*10 MODX(2)
	COMMON /XX2   / PX2PR
	COMMON R(6)/WW/W0, W(400)
	COMMON /WWC   / ID
	COMMON /COMIND/ ICODE, INDEXR, INDEXE, INDEXP, INDEXM, INDEXC
	COMMON /PERTOF/ISWITCH
	COMMON /RUNDATE/ iyyyy,imd,hour
	COMMON /IRI_BLK/ edensARR(500,500),edensFLAG,edensPOS(2,500),dayNe(24), 
     &		    dip(2,500)
	common /gradients/ dedh,dedlat,dedlon
	double precision dedh,dedlat,dedlon
	CHARACTER*10 ID(10)
	EQUIVALENCE(EARTHR, W(2)), (F, W(6)), (FC1, W(101)), 
     &		    (HM1, W(102)), (SH1, W(103)), (FC2, W(104)), 
     &		    (HM2, W(105)), (SH2, W(106)), (C, W(107)), 
     &		    (AZBEG, W(11)), (AZEND, W(12)), (AZSTEP, W(13)), 
     &		    (COSYS, W(109)), (E, W(108)), (PERT, W(150))
	EQUIVALENCE (FREQ,W(6)), (TXLAT,W(4)), (TXLON,W(5))

	DIMENSION iriout(500),dipout(2)
	DOUBLE PRECISION iriout,dipout
	dimension irioutTest(500)
	DOUBLE PRECISION irioutTest
	REAL hour,thtmp,dlat,dlon,tdiff,old_hour
	double precision latitude,longitude,Hin,lonDeg,latDeg
	integer iyyyy,imd
C Index for lookup in electron density array
	integer vind,latind,lonind,lind
	double precision neazu,neazd,edens,ne0
	REAL Rpol,Requ

	Rpol = 6356.7523142
	Requ = 6378.137


	IF(ICODE.EQ.0)THEN
		MODX(1) = 'IRI'
c       call initialize in irifun.for
		call initialize
 		write(*,'(A,$)') 'Year(yyyy),Month&day(mmdd),hour(UT: add 25.0)'
		read(*,*) iyyyy,imd,hour
 		!print *,'using date: ',iyyyy,imd,hour
		RETURN
	END IF
C Generate electron density array
C and position array
	IF(edensFLAG.eq.0)THEN
		vbeg = 60.
		vend = 560.
		edensPOS(1,1)=TXLAT*180./PI
		IF(edensPOS(1,1).gt.90.OR.edensPOS(1,1).lt.-90)THEN
			edensPOS(1,1) = 180. - edensPOS(1,1)
		ENDIF
		edensPOS(2,1)=TXLON*180./PI
		IF(edensPOS(2,1).lt.0)THEN
			edensPOS(2,1) = edensPOS(2,1) + PIT2*180./PI
		ENDIF
		latitude = edensPOS(1,1)
		longitude = edensPOS(2,1)
		CALL IRIBRIDGE(vbeg,vend,latitude,longitude,iriout,dipout)
		do j=1,500
			edensARR(j,1) = iriout(j)
		enddo
		dip(1,1)=dipout(1)
		dip(2,1)=dipout(2)
C		Lat/lon loop
		do n=2,500
			thtmp = edensPOS(1,n-1)* PI/180.
			latDeg = PI/180.*Requ*Rpol*Requ*Rpol/ 
     &     SQRT(Requ*Requ*COS(thtmp)*COS(thtmp) +  
     &     Rpol*Rpol*SIN(thtmp)*SIN(thtmp))/ 
     &     (Requ*Requ*COS(thtmp)*COS(thtmp) +  
     &     Rpol*Rpol*SIN(thtmp)*SIN(thtmp))
			edensPOS(1,n) = edensPOS(1,n-1) + 
     &     5./latDeg*COS(AZBEG)
			if (abs(azbeg/RAD).eq.90.) then
				edensPOS(1,n) = edensPOS(1,n-1)
			endif
			IF(edensPOS(1,n).gt.90.OR.edensPOS(1,n).lt.-90)THEN
				edensPOS(1,n) = 180. - edensPOS(1,n)
			ENDIF
			thtmp = edensPOS(1,n)* PI/180.
			lonDeg = PI/180.*Requ*Requ*COS(thtmp) /  
     &     SQRT(Requ*Requ*COS(thtmp)*COS(thtmp) +  
     &     Rpol*Rpol*SIN(thtmp)*SIN(thtmp))
			edensPOS(2,n) = edensPOS(2,n-1) + 
     &     5./lonDeg*SIN(AZBEG)
 			IF(edensPOS(2,n).lt.0)THEN
				edensPOS(2,n) = edensPOS(2,n) + PIT2*180./PI
			ENDIF
			latitude = edensPOS(1,n)
			longitude = edensPOS(2,n)
			CALL IRIBRIDGE(vbeg,vend,latitude,longitude,iriout,dipout)
C			Altitude loop
			do j=1,500
				edensARR(j,n) = iriout(j)
			enddo
			dip(1,n)=dipout(1)
			dip(2,n)=dipout(2)
		ENDDO
C Save daily electron density at 250 km altitude in middle of range
		old_hour = hour
		vbeg = 250.
		vend = vbeg+1.
		DO d=1,24
			hour = REAL(d-1)
			CALL IRIBRIDGE(vbeg,vend,edensPOS(1,250),edensPOS(2,250),iriout,dipout)
			dayNe(d)=iriout(1)
		enddo
		hour=old_hour
		edensFLAG=1
	ENDIF

	EARTHE = EARTHR*E
	IF(cosys .EQ. 1)THEN 
		theta2 = r(2) - PID2
	ELSE
		CALL c2geog(PID2-r(2), r(3), templat, templon)
		theta2=templat-PID2
	ENDIF
	HMAX = HM1 + EARTHE*THETA2
	PXPTH = 0.
	PXPR = 0.0
	X = 0.0
	PXPPH = 0.
	PX2PR = 0.
	Hin=0.
	Hin = R(1) - EARTHR

	if(hin.lt.65.or.hin.gt.2000)then
	   return
	endif

	edens = 0.0

	longitude = 180.0/pi*r(3)
	latitude = -180.0/pi*theta2

	
	if(longitude.lt.0)then
	   longitude = longitude+PIT2*180./PI
	endif
C Look-up in table (vertical, latitudinal, longitudinal limits)
	vind = int(Hin-60.)+1
	latind = 1
	do i=2,500
			if (edensPOS(1,i).ge.latitude) then
				latind = i-1
				EXIT
			endif
	enddo
	lonind = 1
	do i=2,500
			if (edensPOS(2,i).ge.longitude) then
				lonind = i-1
				EXIT
			endif
	enddo
C If there was a problem, say so and aobrt
	if (vind.gt.500.or.vind.lt.1) then 
		edens = 0.
		return
	endif
C find azimuthal index (approx.)
	lind = max(latind,lonind)
	neazu = (edensPOS(2,lonind+1) - longitude)/(edensPOS(2,lonind+1) - edensPOS(2,lonind))*edensARR(vind+1,lind) +
     &	 (longitude - edensPOS(2,lonind))/(edensPOS(2,lonind+1) - edensPOS(2,lonind))*edensARR(vind+1,lind+1)
	neazd = (edensPOS(2,lonind+1) - longitude)/(edensPOS(2,lonind+1) - edensPOS(2,lonind))*edensARR(vind,lind) +
     &	 (longitude - edensPOS(2,lonind))/(edensPOS(2,lonind+1) - edensPOS(2,lonind))*edensARR(vind,lind+1)
	if (azbeg/RAD.eq.90.) then
		neazu = edensARR(vind+1,lind)
		neazd = edensARR(vind,lind)
	endif
	edens = (Hin - ((vind-1)*1.+60.))/1.*neazu + (vind*1.+60. - Hin)/1.*neazd
	edens = edens*80.5e-12/(FREQ*FREQ)
	dedh = (edensARR(vind+1,lind)-edensARR(vind-1,lind))/2.*80.5e-12/(FREQ*FREQ)
	dedlat = neaz*80.5e-12/(FREQ*FREQ)
	dedlon = neaz*80.5e-12/(FREQ*FREQ)

	if(edens.gt.0)then
	   X = dble(edens)
	   pxpr = dedh
	   pxpth = dedlat
	   pxpph = dedlon
	endif
! 	if(X.gt.1.) then
! 		print*,'*************'
! 		print*,latitude,longitude,Hin, latind, lonind, lind, vind
! 		print*,edensARR(vind+1,lind)*1e-12,edensARR(vind+1,lind+1)*1e-12
! 		print*,edensARR(vind,lind)*1e-12,edensARR(vind,lind+1)*1e-12
! 		print*,edensPOS(1,latind), edensPOS(2,lonind)
! 		print*,edensPOS(1,latind+1), edensPOS(2,lonind+1)
! 		print*,edens/80.5*(FREQ*FREQ)
! 		print*,'*************'
! 	endif

	RETURN
	END

	SUBROUTINE IRIBRIDGE (height_beg,height_end,lat_in,long_in,iriout,dipout)

	double precision height_beg,height_end,lat_in,long_in

	INTEGER           pad1(6),jdprof(77),jmag
	DIMENSION         outf(20,500),oar(50),jfi(6),iriout(500),dipout(2)
	DOUBLE PRECISION  iriout,dipout
	LOGICAL           jf(30)
	CHARACTER*2       timev(2)
	CHARACTER*3       uni(48),sopt
	CHARACTER*4       IMZ(8),MAP,xtex,coorv(2)
	CHARACTER*5       ITEXT(8)
	CHARACTER*6       dopt,pna(48)
	CHARACTER*8       bopt
	CHARACTER*9       topt,pname(6)
	CHARACTER*10      iopt
	CHARACTER*16      f1opt

	COMMON /CONST / PI, PIT2, PID2
	COMMON /RUNDATE/ iyyyy,imd,hour
	common /gradients/ dedh,dedlat,dedlon
	double precision dedh,dedlat,dedlon
	real eh_hi,eh_lo,elat_hi,elat_lo,elon_hi,elon_lo
	real hdelta,thdelta,thrdelta
C force correct types0
	REAL edens,no_edens
	REAL xlat,xlon,vbeg,vend,vstp,hour
	INTEGER iyyyy,imd


	no_edens = 0.
	dedh = 0.
	dedlat = 0.
	dedlon = 0.
	hdelta = 1.

	if (height_end.gt.2000.or.height_beg.lt.60) then
	   print *,'height out of range:',height_beg
	   iriout(1)=no_edens
	   return
	endif

	if (((height_beg+1.).eq.height_beg).or.((long_in+1.).eq.long_in).or.((lat_in+1.).eq.lat_in)) then
	   print *,'nan: height',(height_beg+1.).eq.height_beg,' lat',(lat_in+1.).eq.lat_in,' long',(long_in+1.).eq.long_in
	   print *,height_beg,lat_in,long_in
	   iriout(1)=no_edens
	   return
	endif

	jmag=0
	xlat=real(lat_in)
	xlon=real(long_in)

C calculate 3 heights in steps of hdelta for gradient in z-axis
	vbeg=real(height_beg)
	vend=real(height_end)
	vstp=real(hdelta)

	jchoice=0
	do i=1,30 
	   jf(i)=.true.
	enddo
	if(jchoice.eq.0) then
		jf(2) = .false.               ! no temperatures
		jf(3) = .false.               ! no ion composition
		jf(5) = .false.                ! URSI foF2 model
		jf(6) = .false.               ! Newest ion composition model
		jf(12) = .false.              ! suppress messages to unit 6
C		jf(21) = .true.               ! ion drift computed
! 		jf(25) = .false.              ! F10.7 user input
! 			oar(41) = 73.
		jf(23) = .false.              ! TTS Te model is standard
		jf(26) = .false.              ! STORM model turned OFF
C		jf(28) = .true.               ! spread-F computed
		jf(29) = .false.              ! New Topside options
		jf(30) = .false.              ! NeQuick topside
	endif
C Calling IRI subroutine
	call iri_sub(jf,jmag,xlat,xlon,iyyyy,imd,hour,
     &          vbeg,vend,vstp,outf,oar)

	do n=1,500
		iriout(n)=outf(1,n)
! 		write(*,*)outf(15,n),xlat,xlon,iriout(n)
	enddo
	dipout(1)=oar(25)
	dipout(2)=oar(27)
! 	write(*,*)vbeg,edens,xlat,xlon
	do n=1,500
		if (iriout(n).lt.0) then
			iriout(n)=no_edens
		endif
! 		write(*,*)(vbeg+n*hdelta),iriout(n),xlat,xlon
	enddo

	eh_lo = outf(1,1)
	eh_hi = outf(1,2)
	dedh = (eh_hi-eh_lo)/(vend-vbeg)


	return

 2357	stop
	END
