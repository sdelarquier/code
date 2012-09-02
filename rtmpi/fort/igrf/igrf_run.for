! Main interface for IGRF model
! 
! Versions:
!     March 2011: uses IGRF-11 coefficients
! 
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! input:    ryear - decimal year (year+month/12.0-0.5 or 
!                      year+day-of-year/365 or ../366 if leap year)
!             vbeg - initial altitude
!             vend - final altitude
!             vstep - altitude step value
!             lati - latitude
!             longi - longitude
! 
! output:    igrfOUT.dat - binary file
!                     Number of elements is 1(I5) + 3 x nelem(F8.3):
!                         * number of elements in each array (nelem)
!                         * magnetic field amplitude (in Gauss)
!                         * magnetic field dip angle
!                         * magnetic field declination angle (+East)
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! Written by S. de Larquier (March 2011)
        PROGRAM igrf_run

        implicit none
        integer(4)                          :: nvar,nsteps,xlcode
        real(4)                             :: ryear,vbeg,vend,vstep
        real(4)                             :: lati,longi,alti
        real(4)                             :: babs,dip,dipdec,xl,dipl
        real(4),ALLOCATABLE,dimension(:)    :: diparr,decarr,fabs,Larr
        real(4),ALLOCATABLE,dimension(:,:,:):: cgm, geo
        character*13                        :: fmt
        real(4),dimension(:)                :: dat(11,4),PLA(4),PLO(4)

        read(5,*),ryear,vbeg,vend,vstep,lati,longi
!         print*,ryear,vbeg,vend,vstep,lati,longi
        
        if (vstep .ne. 0.) then
            nsteps = int((vend-vbeg)/vstep) + 1
        else
            nsteps = 1
        endif
        if (nsteps .gt. 99999) then
            print*, 'Too many steps: ',nsteps
            return
        endif
        allocate(diparr(nsteps), decarr(nsteps), fabs(nsteps), 
     &              cgm(nsteps,3,2), geo(nsteps,3,2), Larr(nsteps))
        alti = vbeg
        do nvar = 1,nsteps
            ! Get dip, declination, field strength and L-sshell
            call igrf_sub(lati,longi,ryear,alti,xl,xlcode,
     &                dipl,babs,dip,dipdec)
            ! Get cgm coordinates
            dat(1,1) = lati
            dat(2,1) = longi
            call geocgm01(1,int(ryear),alti,dat,PLA,PLO)
            print*, alti
            print*, dat(1,1), dat(2,1)
            print*, dat(1,2), dat(2,2)
            print*, dat(1,3), dat(2,3) 
            print*, dat(1,4), dat(2,4)
            !print*, lati, longi, alti, dip, dipdec, babs, dipl, xl
            diparr(nvar) = dip
            decarr(nvar) = dipdec
            fabs(nvar) = babs
            Larr(nvar) = xl
            cgm(nvar,1,1) = dat(3,1) ! Given point
            cgm(nvar,1,2) = dat(4,1) ! Given point
            cgm(nvar,2,1) = dat(3,3) ! Footprint
            cgm(nvar,2,2) = dat(4,3) ! Footprint
            cgm(nvar,3,1) = dat(3,4) ! Conjugate Footprint
            cgm(nvar,3,2) = dat(4,4) ! Conjugate Footprint
            geo(nvar,1,1) = dat(1,1) ! Given point
            geo(nvar,1,2) = dat(2,1) ! Given point
            geo(nvar,2,1) = dat(1,3) ! Footprint
            geo(nvar,2,2) = dat(2,3) ! Footprint
            geo(nvar,3,1) = dat(1,4) ! Conjugate Footprint
            geo(nvar,3,2) = dat(2,4) ! Conjugate Footprint
        !    Update variable
            alti = vbeg + nvar*vstep
        end do

        OPEN(66,FILE='/tmp/igrfOUT.dat',status='unknown')
        write(66,'(I5)')nsteps
        write(fmt,100)nsteps
100        FORMAT('(',I5,'F8.3)')
        write(66,fmt)(fabs(nvar), nvar=1,nsteps)
        write(66,fmt)(diparr(nvar), nvar=1,nsteps)
        write(66,fmt)(decarr(nvar), nvar=1,nsteps)
        write(66,fmt)(Larr(nvar), nvar=1,nsteps)
        write(66,fmt)(cgm(nvar,1,1), nvar=1,nsteps)
        write(66,fmt)(cgm(nvar,1,2), nvar=1,nsteps)
        write(66,fmt)(cgm(nvar,2,1), nvar=1,nsteps)
        write(66,fmt)(cgm(nvar,2,2), nvar=1,nsteps)
        write(66,fmt)(cgm(nvar,3,1), nvar=1,nsteps)
        write(66,fmt)(cgm(nvar,3,2), nvar=1,nsteps)
        write(66,fmt)(geo(nvar,1,1), nvar=1,nsteps)
        write(66,fmt)(geo(nvar,1,2), nvar=1,nsteps)
        write(66,fmt)(geo(nvar,2,1), nvar=1,nsteps)
        write(66,fmt)(geo(nvar,2,2), nvar=1,nsteps)
        write(66,fmt)(geo(nvar,3,1), nvar=1,nsteps)
        write(66,fmt)(geo(nvar,3,2), nvar=1,nsteps)
        CLOSE(66)
        
        
        END
