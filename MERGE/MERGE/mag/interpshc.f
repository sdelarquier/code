c SD_BEGIN>
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
	implicit real*8 (a-h,o-z)
  	DIMENSION	GH1(*), GH2(*), GH(*)
  
C ---------------------------------------------------------------   
C	The coefficients (GH) of the resulting model, at date 
C	DATE, are computed by linearly interpolating between the    
C	coefficients of the earlier model (GH1), at date DTE1,
C	and those of the later model (GH2), at date DTE2. If one    
C	model is smaller than the other, the interpolation is 
C	performed with the missing coefficients assumed to be 0.    
C ---------------------------------------------------------------   
C<SD_END 
  
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
