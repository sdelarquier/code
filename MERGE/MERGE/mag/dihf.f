CSD_BEGIN>
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
C<SD_END 
	REAL		I
	PARAMETER     (	SN = 0.0001)    
  
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



