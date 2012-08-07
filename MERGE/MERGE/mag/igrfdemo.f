	PROGRAM IGRFDEMO                                                       
C ===============================================================              
C                                                                             
C	Version 1.01                                                        
C                                                                           
C	This program demonstrates the use of subroutines related               
C	to the synthesization of geomagnetic field values from                 
C	spherical harmonic models by computing sample values                
C	from the International Geomagnetic Reference Field (1985               
C	Revision).                                                             
C                                                                              
C	Subroutines:                                                           
C	    GETSHC, INTERPSHC, EXTRAPSHC, SHVAL3, DIHF                         
C                                                                              
C	A. Zunde                                                               
C	USGS, MS 964, Box 25046 Federal Center, Denver, CO  80225              
C                                                                              
C ===============================================================              
                                                                        
      REAL		I, IDOT, ITEMP                                         
	CHARACTER*11	FILMOD, FIL1, FIL2                                     
	DIMENSION	GH1(120), GH2(120), GHA(120), GHB(120),                
     1			EXT(3), FILMOD(11), DTEMOD(11)                         
	PARAMETER	(A2 = 40680925., B2 = 40408588.)                       
	DATA		IU, EXT /10, 3*0./                                     
	DATA		FILMOD /'dgrf45.dat', 'dgrf50.dat',                    
     1			'dgrf55.dat', 'dgrf60.dat', 'dgrf65.dat',              
     2			'dgrf70.dat', 'dgrf75.dat', 'dgrf80.dat',              
     3			'dgrf85.dat', 'dgrf90.dat','dgrf95.dat',
     4			'igrf00.dat', 'igrf00s.dat'/              
	DATA		DTEMOD / 1945., 1950., 1955., 1960.,                   
     1      	1965., 1970., 1975., 1980., 1985., 1990., 1995., 2000.0, 2005.0/
  
C ---------------------------------------------------------------   
C	Loop through nine dates, 1947.0 through 1992.0, in 5-year   
C	intervals.  
C	For each date:    
C	    - Set the model dates (DTE1 and DTE2) and the coef-     
C	ficient file names (FIL1 and FIL2) from the list of   
C	dates (DTEMOD) and the list of file names (DTEFIL).   
C	    - Read the coefficients into arrays GH1 and GH2.  
C	    - Interpolate to get the time-adjusted coefficients     
C	(GHA and GHB) if both models are of the main field,   
C	or, extrapolate if the first model is of the main     
C	field and the other is of the secular variation.
C	    - Calculate the geomagnetic field values for various    
C	locations.  
C 
C	GHB are the time-adjusted coefficients one year later 
C	than GHA.   
C ---------------------------------------------------------------   
  
	WRITE (*, '(//2(T34, A/)/ 2A/)') 'SAMPLE OUTPUT',
     1	'-------------', ' DATE   LAT   LON  ELEV',
     2	'    D I  H X Y Z F'     
  
	DATE = 1947. 
	DO L = 1, 12  
	    WRITE (*, '(F7.1)') DATE   
  
	    DTE1 = DTEMOD(L)     
	    FIL1 = FILMOD(L)     
	    DTE2 = DTEMOD(L+1)   
	    FIL2 = FILMOD(L+1)   
  
	    CALL GETSHC (IU, FIL1, NMAX1, ERAD, GH1, IER)
	    IF (IER .NE. 0) STOP 
	    CALL GETSHC (IU, FIL2, NMAX2, ERAD, GH2, IER)
	    IF (IER .NE. 0) STOP 
  
	    IF (L .LE. 9) THEN   
		CALL INTERPSHC (DATE, DTE1, NMAX1, GH1, DTE2,   
     1		NMAX2, GH2, NMAX, GHA)  
		CALL INTERPSHC (DATE+1., DTE1, NMAX1, GH1, DTE2,
     1		NMAX2, GH2, NMAX, GHB)  
	    ELSE     
		CALL EXTRAPSHC (DATE, DTE1, NMAX1, GH1, NMAX2,  
     1		GH2, NMAX, GHA)   
		CALL EXTRAPSHC (DATE+1., DTE1, NMAX1, GH1, NMAX2,     
     1		GH2, NMAX, GHB)   
	    ENDIF    
  
C ---------------------------------------------------------------   
C	For each date, Loop through five different combinations     
C	of latitude, longitude, and elevation.    
C	For each location:
C	    - Compute X, Y, and Z using GHA and GHB.    
C	    - Compute D, I, H, and F. 
C	    - Compute the annual change for all elements.     
C 
C	The annual change of an element is equal to the value of    
C	the element at the desired date subtracted from its value   
C	exactly one year later. 
C ---------------------------------------------------------------   
  
	    FLAT = 90.     
	    FLON = -180.   
	    ELEV = -250.   
	    DO M = 1, 5    
		FLAT = FLAT - 30. 
		FLON = FLON + 60. 
		ELEV = ELEV + 250.
		CALL SHVAL3 (1, FLAT, FLON, ELEV, ERAD, A2, B2, 
     1		NMAX, GHA, 0, EXT, X, Y, Z)   
		CALL DIHF (X, Y, Z, D, I, H, F)     

		CALL SHVAL3 (1, FLAT, FLON, ELEV, ERAD, A2, B2, 
     1		NMAX, GHB, 0, EXT, XTEMP, YTEMP, ZTEMP)   
		CALL DIHF (XTEMP, YTEMP, ZTEMP, DTEMP, ITEMP,   
     1		HTEMP, FTEMP)     
		DDOT = DTEMP - D  
		IDOT = ITEMP - I  
		HDOT = HTEMP - H  
		XDOT = XTEMP - X  
		YDOT = YTEMP - Y  
		ZDOT = ZTEMP - Z  
		FDOT = FTEMP - F  
		WRITE (*, '(5X, 3F6.0, 2F8.3, 5F8.0/ 23X, 2F8.3,
     1		5(2X, F6.1))') FLAT, FLON, ELEV, D, I, H, X, Y, 
     2		Z, F, DDOT, IDOT, HDOT, XDOT, YDOT, ZDOT, FDOT  
	    ENDDO    
	    DATE = DATE + 5.     
	ENDDO  
  
	END    
	SUBROUTINE GETSHC (IU, FSPEC, NMAX, ERAD, GH, IER)     
  
C ===============================================================   
C 
C	Version 1.01
C 
C	Reads spherical harmonic coefficients from the specified    
C	file into an array.     
C 
C	Input:
C	    IU    - Logical unit number     
C	    FSPEC - File specification
C 
C	Output:     
C	    NMAX  - Maximum degree and order of model   
C	    ERAD  - Earth's radius associated with the spherical    
C		    harmonic coefficients, in the same units as
C		    elevation    
C	    GH    - Schmidt quasi-normal internal spherical   
C		    harmonic coefficients    
C	    IER   - Error number: =  0, no error  
C				  = -2, records out of order     
C			     	  = FORTRAN run-time error number 
C 
C	A. Zunde    
C	USGS, MS 964, Box 25046 Federal Center, Denver, CO  80225   
C 
C ===============================================================   
  
	CHARACTER	FSPEC*(*)
	DIMENSION	GH(*)    
  
C ---------------------------------------------------------------   
C	Open coefficient file. Read past first header record. 
C	Read degree and order of model and Earth's radius.    
C ---------------------------------------------------------------   
  
	OPEN (IU, FILE=FSPEC, STATUS='OLD', IOSTAT=IER, ERR=999,     
     1	READONLY)    
  
	READ (IU, *, IOSTAT=IER, ERR=999)    
	READ (IU, *, IOSTAT=IER, ERR=999) NMAX, ERAD     
  
C ---------------------------------------------------------------   
C	Read the coefficient file, arranged as follows: 
C 
C					N     M     G     H 
C					----------------------    
C				    /   1     0    GH(1)  -
C				   /	1     1    GH(2) GH(3)
C				  /	2     0    GH(4)  -    
C				 /	2     1    GH(5) GH(6)  
C	    NMAX*(NMAX+3)/2 	/	2     2    GH(7) GH(8)   
C	 records		\	3     0    GH(9)  -     
C				 \.     .     .     .
C				  \	.     .     .     .    
C	    NMAX*(NMAX+2)	   \	.     .     .     .
C	    elements in GH	    \  NMAX  NMAX   .     .  
C 
C	N and M are, respectively, the degree and order of the
C	coefficient.
C ---------------------------------------------------------------   
  
	I = 0  
	DO NN = 1, NMAX    
	    DO MM = 0, NN  
		READ (IU, *, IOSTAT=IER, ERR=999) N, M, G, H    
		IF (NN .NE. N .OR. MM .NE. M) THEN  
		    IER = -2
		    GOTO 999
		ENDIF 
		I = I + 1   
		GH(I) = G   
		IF (M .NE. 0) THEN
		    I = I + 1     
		    GH(I) = H     
		ENDIF 
	    ENDDO    
	ENDDO  
  
999	CLOSE (IU)
  
	RETURN 
	END    
	SUBROUTINE INTERPSHC (DATE, DTE1, NMAX1, GH1, DTE2,    
     1			NMAX2, GH2, NMAX, GH)  
  
C ===============================================================   
C 
C	Version 1.01
C 
C	Interpolates linearly, in time, between two spherical 
C	harmonic models.  
C 
C	Input:
C	    DATE  - Date of resulting model (in decimal year) 
C	    DTE1  - Date of earlier model   
C	    NMAX1 - Maximum degree and order of earlier model 
C	    GH1   - Schmidt quasi-normal internal spherical   
C		    harmonic coefficients of earlier model     
C	    DTE2  - Date of later model     
C	    NMAX2 - Maximum degree and order of later model   
C	    GH2   - Schmidt quasi-normal internal spherical   
C		    harmonic coefficients of later model 
C 
C	Output:     
C	    GH    - Coefficients of resulting model     
C	    NMAX  - Maximum degree and order of resulting model     
C 
C	A. Zunde    
C	USGS, MS 964, Box 25046 Federal Center, Denver, CO  80225   
C 
C ===============================================================   
  
	DIMENSION	GH1(*), GH2(*), GH(*)
  
C ---------------------------------------------------------------   
C	The coefficients (GH) of the resulting model, at date 
C	DATE, are computed by linearly interpolating between the    
C	coefficients of the earlier model (GH1), at date DTE1,
C	and those of the later model (GH2), at date DTE2. If one    
C	model is smaller than the other, the interpolation is 
C	performed with the missing coefficients assumed to be 0.    
C ---------------------------------------------------------------   
  
	FACTOR = (DATE - DTE1) / (DTE2 - DTE1)     
  
	IF (NMAX1 .EQ. NMAX2) THEN     
	    K = NMAX1 * (NMAX1 + 2)    
	    NMAX = NMAX1   
	ELSE IF (NMAX1 .GT. NMAX2) THEN
	    K = NMAX2 * (NMAX2 + 2)    
	    L = NMAX1 * (NMAX1 + 2)    
	    DO I = K + 1, L
		GH(I) = GH1(I) + FACTOR * (-GH1(I)) 
	    ENDDO    
	    NMAX = NMAX1   
	ELSE   
	    K = NMAX1 * (NMAX1 + 2)    
	    L = NMAX2 * (NMAX2 + 2)    
	    DO I = K + 1, L
		GH(I) = FACTOR * GH2(I) 
	    ENDDO    
	    NMAX = NMAX2   
	ENDIF  
  
	DO I = 1, K  
	    GH(I) = GH1(I) + FACTOR * (GH2(I) - GH1(I))  
	ENDDO  
  
	RETURN 
	END    
	SUBROUTINE EXTRAPSHC (DATE, DTE1, NMAX1, GH1, NMAX2,   
     1			GH2, NMAX, GH)   
  
C ===============================================================   
C 
C	Version 1.01
C 
C	Extrapolates linearly a spherical harmonic model with a     
C	rate-of-change model.   
C 
C	Input:
C	    DATE  - Date of resulting model (in decimal year) 
C	    DTE1  - Date of base model
C	    NMAX1 - Maximum degree and order of base model    
C	    GH1   - Schmidt quasi-normal internal spherical   
C		    harmonic coefficients of base model  
C	    NMAX2 - Maximum degree and order of rate-of-change
C		    model  
C	    GH2   - Schmidt quasi-normal internal spherical   
C		    harmonic coefficients of rate-of-change model    
C 
C	Output:     
C	    GH    - Coefficients of resulting model     
C	    NMAX  - Maximum degree and order of resulting model     
C 
C	A. Zunde    
C	USGS, MS 964, Box 25046 Federal Center, Denver, CO  80225   
C 
C ===============================================================   
  
	DIMENSION	GH1(*), GH2(*), GH(*)
  
C ---------------------------------------------------------------   
C	The coefficients (GH) of the resulting model, at date 
C	DATE, are computed by linearly extrapolating the coef-
C	ficients of the base model (GH1), at date DTE1, using 
C	those of the rate-of-change model (GH2), at date DTE2. If   
C	one model is smaller than the other, the extrapolation is   
C	performed with the missing coefficients assumed to be 0.    
C ---------------------------------------------------------------   
  
	FACTOR = (DATE - DTE1)   
  
	IF (NMAX1 .EQ. NMAX2) THEN     
	    K = NMAX1 * (NMAX1 + 2)    
	    NMAX = NMAX1   
	ELSE IF (NMAX1 .GT. NMAX2) THEN
	    K = NMAX2 * (NMAX2 + 2)    
	    L = NMAX1 * (NMAX1 + 2)    
	    DO I = K + 1, L
		GH(I) = GH1(I)    
	    ENDDO    
	    NMAX = NMAX1   
	ELSE   
	    K = NMAX1 * (NMAX1 + 2)    
	    L = NMAX2 * (NMAX2 + 2)    
	    DO I = K + 1, L
		GH(I) = FACTOR * GH2(I) 
	    ENDDO    
	    NMAX = NMAX2   
	ENDIF  
  
	DO I = 1, K  
	    GH(I) = GH1(I) + FACTOR * GH2(I) 
	ENDDO  
  
	RETURN 
	END    
	SUBROUTINE SHVAL3 (IGDGC, FLAT, FLON, ELEV, ERAD, A2, B2,    
     1			   NMAX, GH, IEXT, EXT, X, Y, Z)   
  
C ================================================================  
C 
C	Version 1.01
C 
C	Calculates field components from spherical harmonic (SH)    
C	models.     
C 
C	Input:
C	    IGDGC - Indicates coordinate system used; set equal to  
C		    1 if geodetic, 2 if geocentric 
C	    FLAT  - North latitude, in degrees    
C	    FLON  - East longitude, in degrees    
C	    ELEV  - Elevation above mean sea level (IGDGC=1), or    
C		    radial distance from Earth's center (IGDGC=2)    
C	    ERAD  - Value of Earth's radius associated with the SH  
C		    coefficients, in same units as ELEV  
C	    A2,B2 - Squares of semi-major and semi-minor axes of    
C		    the reference spheroid used for transforming     
C		    between geodetic and geocentric coordinates or   
C		    components   
C	    NMAX  - Maximum degree and order of coefficients  
C	    GH    - Schmidt quasi-normal internal spherical   
C		    harmonic coefficients    
C	    IEXT  - External coefficients flag (= 0 if none)  
C	    EXT   - The three 1st-degree external coefficients
C		    (not used if iext = 0)   
C 
C	Output:     
C	    X     -  Northward component    
C	    Y     -  Eastward component     
C	    Z     -  Vertically-downward component
C 
C	Based on subroutine "IGRF" by D. R. Barraclough and   
C	S. R. C. Malin, Report No. 71/1, Institute of Geological    
C	Sciences, U.K.    
C 
C	Norman W. Peddie, U.S. Geological Survey, Mail Stop 964,    
C	Federal Center, Box 25046, Denver, Colorado 80225     
C 
C ================================================================  
  
C	The required sizes of the arrays used in this subroutine    
C	depend on the value of NMAX.  The minimum dimensions  
C	needed are indicated in the table below.  (Note that this   
C	version is dimensioned for NMAX of 14 or less). 
C 
C					    Minimum dimension     
C					--------------------------
	DIMENSION  SL(14), CL(14)	!  NMAX    
	DIMENSION  P(119), Q(119)	! (NMAX * (NMAX + 3)) / 2    
	DIMENSION  GH(224)		!  NMAX * (NMAX + 2)   
	DIMENSION  EXT(3)		!  3  
  
C ================================================================  
  
	PARAMETER  DTR = .01745329     
  
	R = ELEV     
	SLAT = SIN (FLAT * DTR)  
	IF (90. - FLAT .LT. .001) THEN 
	    AA = 89.999			    ! 300 ft from N. pole
	ELSE IF (90. + FLAT .LT. .001) THEN  
	    AA = -89.999		    ! 300 ft from S. pole
	ELSE   
	    AA = FLAT
	ENDIF  
	CLAT = COS (AA * DTR)    
	SL(1) = SIN (FLON * DTR) 
	CL(1) = COS (FLON * DTR) 
	X = 0. 
	Y = 0. 
	Z = 0. 
	SD = 0.
	CD = 1.
	N = 0  
	L = 1  
	M = 1  
	NPQ = (NMAX * (NMAX + 3)) / 2  
  
	IF (IGDGC .EQ. 1) THEN   
	    AA = A2 * CLAT * CLAT
	    BB = B2 * SLAT * SLAT
	    CC = AA + BB   
	    DD = SQRT (CC) 
	    R = SQRT (ELEV * (ELEV + 2. * DD)
     1			 + (A2 * AA + B2 * BB) / CC) 
	    CD = (ELEV + DD) / R 
	    SD = (A2 - B2) / DD * SLAT * CLAT / R  
	    AA = SLAT
	    SLAT = SLAT * CD - CLAT * SD     
	    CLAT = CLAT * CD + AA * SD 
	ENDIF  
  
	RATIO = ERAD / R   
  
	AA = SQRT (3.)     
	P(1) = 2. * SLAT   
	P(2) = 2. * CLAT   
	P(3) = 4.5 * SLAT * SLAT - 1.5 
	P(4) = 3. * AA * CLAT * SLAT   
	Q(1) = -CLAT 
	Q(2) = SLAT  
	Q(3) = -3. * CLAT * SLAT 
	Q(4) = AA * (SLAT * SLAT - CLAT * CLAT)    
  
	DO K = 1, NPQ
	    IF (N .LT. M) THEN   
		M = 0 
		N = N + 1   
		RR = RATIO**(N + 2)     
		FN = N
	    ENDIF    
	    FM = M   
	    IF (K .GE. 5) THEN   
		IF (M .EQ. N) THEN
		    AA = SQRT (1. - .5 / FM)  
		    J = K - N - 1 
		    P(K) = (1. + 1. / FM) * AA * CLAT * P(J)    
		    Q(K) = AA * (CLAT * Q(J) + SLAT / FM * P(J))
		    SL(M) = SL(M-1) * CL(1) + CL(M-1) * SL(1)   
		    CL(M) = CL(M-1) * CL(1) - SL(M-1) * SL(1)   
		ELSE  
		    AA = SQRT (FN * FN - FM * FM)   
		    BB = SQRT ((FN - 1.)**2 - FM * FM) / AA     
		    CC = (2. * FN - 1.) / AA  
		    I = K - N     
		    J = K - 2 * N + 1   
		    P(K) = (FN + 1.) * (CC * SLAT / FN * P(I) - BB    
     1		     / (FN - 1.) * P(J))
		    Q(K) = CC * (SLAT * Q(I) - CLAT / FN * P(I))
     1			   - BB * Q(J)   
		ENDIF 
	    ENDIF    
  
	    AA = RR * GH(L)
	    IF (M .EQ. 0) THEN   
		X = X + AA * Q(K) 
		Z = Z - AA * P(K) 
		L = L + 1   
	    ELSE     
		BB = RR * GH(L+1) 
		CC = AA * CL(M) + BB * SL(M)  
		X = X + CC * Q(K) 
		Z = Z - CC * P(K) 
		IF (CLAT .GT. 0.) THEN  
		    Y = Y + (AA * SL(M) - BB * CL(M)) * FM * P(K)     
     1			/ ((FN + 1.) * CLAT)   
		ELSE  
		    Y = Y + (AA * SL(M) - BB * CL(M)) * Q(K)    
     1		    * SLAT  
		ENDIF 
		L = L + 2   
	    ENDIF    
	    M = M + 1
	ENDDO  
  
	IF (IEXT .NE. 0) THEN    
	    AA = EXT(2) * CL(1) + EXT(3) * SL(1)   
	    X = X - EXT(1) * CLAT + AA * SLAT
	    Y = Y + EXT(2) * SL(1) - EXT(3) * CL(1)
	    Z = Z + EXT(1) * SLAT + AA * CLAT
	ENDIF  
  
	AA = X 
	X = X * CD + Z * SD
	Z = Z * CD - AA * SD     
  
	RETURN 
	END    
	SUBROUTINE DIHF (X, Y, Z, D, I, H, F)
  
C ===============================================================   
C 
C	Version 1.01
C 
C	Computes the geomagnetic elements D, I, H, and F from 
C	X, Y, and Z.
C 
C	Input:
C	    X   - Northward component 
C	    Y   - Eastward component  
C	    Z   - Vertically-downward component   
C 
C	Output:     
C	    D   - Declination   
C	    I   - Inclination   
C	    H   - Horizontal intensity
C	    F   - Total intensity     
C 
C	A. Zunde    
C	USGS, MS 964, Box 25046 Federal Center, Denver, CO  80225   
C 
C ===============================================================   
  
	REAL		I
	PARAMETER	SN = 0.0001    
  
C ---------------------------------------------------------------   
C	If D and I cannot be determined, set equal to 999.0.  
C ---------------------------------------------------------------   
  
	H2 = X*X + Y*Y     
	H = SQRT (H2)
	F = SQRT (H2 + Z*Z)
	IF (F .LT. SN) THEN
	    D = 999. 
	    I = 999. 
	ELSE   
	    I = ATAN2D (Z, H)    
	    IF (H .LT. SN) THEN  
		D = 999.    
	    ELSE     
		HPX = H + X 
		IF (HPX .LT. SN) THEN   
		    D = 180.
		ELSE  
		    D = 2. * ATAN2D (Y, HPX)  
		ENDIF 
	    ENDIF    
	ENDIF  
  
	RETURN 
	END    











