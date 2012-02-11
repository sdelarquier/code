PROGRAM igrfOUT
    
    implicit none
    integer(4)        :: ilat,ilon
    integer(4)        :: ryear
    real(4)           :: lati,longi,dip,magbr,modip
    real(4)           :: BNORTH,BEAST,BDOWN,BABS,DIPDEC
    real(4)           :: diparr(360,180),decarr(360,180)

    read(5,*),ryear
    
    lati = -90.
    longi = 0.
    do ilat = 1, 180, 1
	do ilon = 1, 360, 1
	    call igrf_dip(lati,longi,ryear,300.0,dip,magbr,modip)
	!     Dip declination (+East, -West)
	    call feldg(lati,longi,300.0,BNORTH,BEAST,BDOWN,BABS)
	    DIPDEC = ATAN(BEAST/BNORTH)*180./(ATAN(1.0)*4.)
	    print *, dip, dipdec
	    diparr(ilon,ilat) = dip
	    decarr(ilon,ilat) = dipdec
	end do
    end do
    
    OPEN(66,FILE='igrfOUT.dat',status='unknown')
99117	FORMAT(360F8.3)
    do ilat = 1,180,1
	write(66,99117)(diparr(nlon,ilat), nlon=1,360)
	write(66,99117)(wout(nlon,ilat), nlon=1,360)
    end do
    CLOSE(66)
    
    
END