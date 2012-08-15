c SD_BEGIN>
      subroutine igrfcall(ifrst,date,flat,flon,elev,x,y,z)

C
C     Input:
C        Date  -  Date in decimal years
C        Flat  -  Latitude of field point
C        Flon  -  Longitude of field point
C        Elev  -  Elevation above sea level
C
C     Output:     
C        X     -  Northward component    
C        Y     -  Eastward component     
C        Z     -  Vertically-downward component
C 
C       Field Values in Gauss
c
c     $Log: igrfcall.f,v $
c
c Revision 1.6    changed IGRF_PATH -> MY_IGRF_PATH (conflict with rst)
c Revision 1.5    updated for 2000 igrf and 2005 extrapolated
c                 coefficients   DA 20010619
c Revision 1.4  leic  updated for 1995 igrf and 2000 extrapolated
c                     coefficients    yxo  23/01/96
c
c Revision 1.3  1994/08/23  14:55:53  bristow
c Changes suggested by Tim Yeoman to put data file
c in an environment variable, and to fix nonstandard
c if statment.
c
c Revision 1.2  1994/08/05  18:40:46  bristow
c change to implicit real *8
c
c Revision 1.1  1994/08/05  18:00:47  bristow
c Initial revision
c
c <SD_END

      implicit real*8 (a-h,o-z)

      real*8            date,flat,flon,elev,x,y,z
      Integer*2         ifrst,i,L
      EXTERNAL		trim		! f90; 20000909; DA
      integer*4	 	trim 
      integer*4	 	strlen 
	CHARACTER*11	FILMOD
        CHARACTER*40    FIL1, FIL2,path                                     
	DIMENSION	GH1(120), GH2(120), GHA(120), GHB(120),                
     1			EXT(3), FILMOD(13), DTEMOD(13)                         
	PARAMETER	(A2 = 40680925., B2 = 40408588.)                       
	DATA		IU, EXT /10, 3*0./                                     
	DATA		FILMOD /'dgrf45.dat', 'dgrf50.dat',                    
     1			'dgrf55.dat', 'dgrf60.dat', 'dgrf65.dat',              
     2			'dgrf70.dat', 'dgrf75.dat', 'dgrf80.dat',              
     3			'dgrf85.dat', 'dgrf90.dat','dgrf95.dat',              
     4                  'igrf00.dat', 'igrf00s.dat'/
	DATA		DTEMOD / 1945., 1950., 1955., 1960.,                   
     1      	1965., 1970., 1975., 1980., 1985., 1990., 1995., 2000.0, 2005.0/

        SAVE
	IER=0
        if( ifrst .ne. 1 ) goto 100

        if( date .lt. 1945. .or. date .gt. 2005. ) goto 1000

        L = 1
        do i=1,11
           if( date .ge. dtemod(i) ) L=i
        enddo
	call getenv_('MY_IGRF_PATH',path)
	strlen = trim(path)
	    DTE1 = DTEMOD(L)     
	    FIL1 = ' '
	    FIL1(1:strlen) = path
	    FIL1(strlen+1:strlen+12) = FILMOD(L)     
	    DTE2 = DTEMOD(L+1)   
	    FIL2 = ' '
	    FIL2(1:strlen) = path
	    FIL2(strlen+1:strlen+12) = FILMOD(L+1)     
  
	    CALL GETSHC (IU, FIL1, NMAX1, ERAD, GH1, IER)
	    IF (IER .NE. 0) STOP
	    CALL GETSHC (IU, FIL2, NMAX2, ERAD, GH2, IER)
	    IF (IER .NE. 0) STOP
  
	    IF (L .LE. 10) THEN   
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

 100        CALL SHVAL3 (1, FLAT, FLON, ELEV, ERAD, A2, B2, 
     1		NMAX, GHA, 0, EXT, X, Y, Z)   


            return
 1000       print*,"DATE OUT OF RANGE"

            return
            END



