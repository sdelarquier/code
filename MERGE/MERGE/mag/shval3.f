c SD_BEGIN>
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
c
c       $Log: shval3.f,v $
c Revision 1.2  1994/08/05  18:40:46  bristow
c change to implicit real *8
c
c Revision 1.1  1994/08/05  18:00:47  bristow
c Initial revision
c
c  
C	The required sizes of the arrays used in this subroutine    
C	depend on the value of NMAX.  The minimum dimensions  
C	needed are indicated in the table below.  (Note that this   
C	version is dimensioned for NMAX of 14 or less). 
c
      implicit real*8 (a-h,o-z)
c
C 
C					    Minimum dimension     
C					--------------------------
	DIMENSION  SL(14), CL(14)	!  NMAX    
	DIMENSION  P(119), Q(119)	! (NMAX * (NMAX + 3)) / 2    
	DIMENSION  GH(224)		!  NMAX * (NMAX + 2)   
	DIMENSION  EXT(3)		!  3  
  
C ================================================================  
c <SD_END  
        PARAMETER  ( DTR = .01745329  )  
  
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
