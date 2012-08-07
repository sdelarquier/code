c SD_BEGIN>
c******************************************************************************
	subroutine MAGCMP(tm4,frho4,flat4,flon4,bx4,by4,bz4,b4,ifrst)

c Calculates the components and magnitude of the geomagnetic field.
c TM is the year (4-digit, may be decimal), FRHO4,FLAT4,FLON4 are the
c geocentric coordinates of the field point (deg), BX4,BY4,BZ4 are the
c geomagnetic field components in the local horizon coordinate system
c (southward, eastward, upward) and B4 is the field magnitude.
c
c IFRST is simply equal to 1 on the first call (intializes the magnetic
c field model in s/r igrfcall).
c
c All field quantities are in units of nanotesla.
c (The conversion to Gauss is b[G} = b[nT]x10-5)   (1T = 104+G). 
c
c Note that the components returned from s/r igrfcall are referenced to
c (northward,eastward,downward).
c
c Last written: 20011218 DA 
c******************************************************************************
C<SD_END 
	implicit   none
	real*4     tm4,frho4,flat4,flon4,bx4,by4,bz4,b4
	real*8     tm8,frho8,flat8,flon8,bx8,by8,bz8,b8
	integer*2  ifrst
	save
	
	tm8     =    tm4
	frho8   =  frho4 - 6372.
	flat8   =  flat4
	flon8   =  flon4

	if (tm8.lt.1945.or.tm8.gt.2005) print 101,tm8
101	format(' Unusual year passed to s/r magcmp:  TM = ',f8.1)

        call igrfcall(ifrst,tm8,flat8,flon8,frho8,bx8,by8,bz8)

	b8   =  sqrt(bx8*bx8+by8*by8+bz8*bz8)

	bx4  = -bx8
	by4  =  by8
	bz4  = -bz8
	b4   =  b8

	return
	end

