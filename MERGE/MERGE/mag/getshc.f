c SD_BEGIN>
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
c
c       $Log: getshc.f,v $
c Revision 1.2  1994/08/05  18:40:46  bristow
c change to implicit real *8
c
c Revision 1.1  1994/08/05  18:00:47  bristow
c Initial revision
c
c
C<SD_END 
	implicit real*8 (a-h,o-z)
  
	EXTERNAL	perror		! g77; 20010416; DA
	CHARACTER	FSPEC*(*)
	DIMENSION	GH(*)    
C ---------------------------------------------------------------   
C	Open coefficient file. Read past first header record. 
C	Read degree and order of model and Earth's radius.    
C ---------------------------------------------------------------   
C	OPEN (IU, FILE=FSPEC, status='OLD', IOSTAT=IER, ERR=999, READONLY)	! f90; 20000909; DA
	OPEN (IU, FILE=FSPEC, status='OLD', IOSTAT=IER, ERR=999)    
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
  
	CLOSE (IU)
  
	RETURN 
999	call perror('GETSHC: File read status: ')
	CLOSE (IU)
	RETURN
	END    
