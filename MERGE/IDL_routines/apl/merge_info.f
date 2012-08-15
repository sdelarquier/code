	subroutine MERGE_INFO
c	*********************
c
c**********************************************************************
c This routine sets up the different options of the merge process
c These options are kept in the inclusion 'info.inc'
c
c	$Log:	merge_info.f,v $
c Revision 1.3  95/01/11  13:17:37  13:17:37  senior (Catherine SENIOR)
c Now opens the output file with a name of the form YYMMDDX.mrg (stnd
c or not). X is the symbol for the pair given by the routine PAIR_NAME
c 
c Revision 1.2  94/11/23  16:27:09  16:27:09  senior (Catherine SENIOR)
c Allows for standard options.
c 
c Revision 1.1  94/09/05  12:57:02  12:57:02  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.3 $
c**********************************************************************

	include 	'info.inc'
	include		'radops.par'
	include 	'raddat386_1_3.inc'
	equivalence	(parms,parm_list)
	integer*4 	st,et,CNV_MDHMS_SEC
	integer*4	FITROPEN, READ_DATREC, FIT_CLOSE
	integer*4 	iostat
	integer*2 	kst
	character*1  	ans
	character*1	NUL, TAB
	character*1	x, PAIR_NAME
	character*20	r, RADAR_NAME
	character*50	outfile
	character*7	out
	character*50 	fidat,fidat1,fidat2
	character*50	fname1, fname2
	logical*1	afname1(50), afname2(50)
	logical*1	stnd
	logical*1	bnul /0/
	logical*1	btab /9/
	data unit_table/16/
	equivalence	(afname1,fname1), (afname2,fname2)
	equivalence	(NUL,bnul), (TAB, btab)

c___________________________

	print*,' testing - Mar 4/96!'


c... Opens data files ...
	outfile = ' '

 10	print *,' Enter Radar1 file name (with no extension)'
	read '(a)', fidat1
	out(1:6) = fidat1(1:6)
	do i=50,1,-1
	  if(fidat1(i:i) .eq. ' ') fidat1(i:i) = NUL
	  if(fidat1(i:i) .eq. TAB) fidat1(i:i) = NUL
	enddo
	fname1 = fidat1
	ifileptr(1) = FITROPEN(afname1)
	if(ifileptr(1) .le.0) then
	   print *,' This file does not exist '
	   print *,' Try again '
	   go to 10
	endif
	iostat = 1
	rec_time = 0
	iostat = READ_DATREC(ifileptr(1))
	if(iostat .ne. 1) print *,' iostat = ', iostat
	st1 = parms386.st_id

 20 	print *,' Enter Radar2 file name (with no extension)'
	read '(a)', fidat2
	do i=50,1,-1
	  if(fidat2(i:i) .eq. ' ') fidat2(i:i) = NUL
	  if(fidat2(i:i) .eq. TAB) fidat2(i:i) = NUL
	enddo
	fname2 = fidat2
	ifileptr(2) = FITROPEN(afname2)
	if(ifileptr(2) .le. 0) then
	   print *,' This file does not exist '
	   print *,' Try again '
	   go to 20
	endif
	iostat = 1
	rec_time = 0
	iostat = READ_DATREC(ifileptr(2))
	if(iostat .ne. 1) print *,' iostat = ', iostat
	st2 = parms386.st_id

	call PAIR(st1,st2,ier)	!Checks that radars are compatible

	if(ier.eq.1) then	!Radar1 should be west of Radar2
	  print *,' Radar1 should be west of Radar2 '
	  print *,' Files will be closed, exchanged, and re-openned '
	  print *,' '
	  do k=1,2
	    iostat = FIT_CLOSE(%val(ifileptr(k)))
	  enddo
	  kst = st1		!Exchanges station IDs
	  st1 = st2
	  st2 = kst
	  fidat  = fidat1	!Exchanges files
	  fidat1 = fidat2
	  fidat2 = fidat
	  fname1 = fidat1
	  fname2 = fidat2
	  ifileptr(1) = FITROPEN(afname1)
	  ifileptr(2) = FITROPEN(afname2)
	else if(ier.eq.2) then
	  print *,' '
	  print *,' This is not a nominal pair of radars, but '
	  print *,' the fields of view overlap '
	  print *,' Do you want to continue?'
	  read '(a)', ans
	  if(ans.eq.'n'.or.ans.eq.'N') stop
	  print *,' '
	else if(ier.eq.3) then
	  print *,' '
	  print *,' Radars are not in the same hemisphere '
	  print *,' Try again'
	  print *,' '
	  go to 10
	else if(ier.eq.4) then
	  print *,' '
	  print *,' Incompatible radar pair: the fields of view '
	  print *,' do not overlap. Try again'
	  print *,' '
	  go to 10
	endif
	x = PAIR_NAME(st1,st2)
	out(7:7) = x


 55	format(a)
        outfile(1:7)  =  out                          !open info file (JMR)
        first         =  8
        last          =  first + 3
        outfile(first:last) = '.inf'
        open(3,file=outfile,status='unknown',form='formatted')
        print*,' The name of the info file is ',outfile(1:last)
        write(3,551) fidat1
        write(3,552) fidat2
        if (ier.eq.2) write(3,553) ans
 551	format(a,20x,'!Western radar .fit file')
 552	format(a,20x,'!Eastern radar .fit file')
 553	format(a,20x,'!not a nominal radar pair - process')


c... Choice of start and end time ...

	print *,' '
	print *,' Enter year (yy)'
	read *, syr
	write(3,554) syr
 554	format(i2,19x,'!year (2-digits)')
	eyr = syr
   2	print *,' Enter start time (mm,id,ih,mn,is)'
	read *, smo,sday,shr,smin,ssec
	write(3,555) smo,sday,shr,smin,ssec
 555	format(5(i2,1x),6x,'!start time')
	print *, 'Enter  end  time (mm,id,ih,mn,is)'
	read *, emo,eday,ehr,emin,esec
	write(3,556) emo,eday,ehr,emin,esec
 556	format(5(i2,1x),6x,'!end time')
	st = CNV_MDHMS_SEC(syr,smo,sday,shr,smin,ssec)
	et = CNV_MDHMS_SEC(eyr,emo,eday,ehr,emin,esec)
	if(et.lt.st) then
	  print *,' end time should be larger than start time '
	  go to 2
	endif

c... Choice of Standard Options ...

	idur = 2			!Standard Options
	tperiod = 192
        tstep   = 192                                        !JMR
	do i=1,2
	   f_low(i) = 8
	   f_high(i) = 20
	enddo
	ialt = 1
	height=400.
	height_r = 325.
	igs = 2
	iereg = 1
	ifltr = 1
	inter = 0
	idiv = 0
	iext = 0
	ires = 0
	pwr_min = 3.
	vel_min = 25.
	vel_max = 3000.
	velerr_max = 50.		!End of Standard Options

	stnd = .false.
	print *,' '
	print *, ' Do you want Standard Options ?...'
	read '(a)', ans
	write(3,557) ans
 557	format(a,20x,'!Standard options?')
	if(ans.eq.'y'.or.ans.eq.'Y') then
	   stnd = .true.
	   go to 99
	endif
	   

c... Averaged or individual maps ...

	print *,' '
	print *,' Averaging maps: Options...'
	print *,'	1 : map for each scan '
	print *,'	2 : average maps '
   3	print *,' Enter choice'
  	read *, idur
        write(3,558) idur
 558	format(i1,20x,'!averaging time: 1:single scan  2:more than 1 scan')
	if(idur.ne.1.and.idur.ne.2) go to 3
	tperiod = 100.
        tstep   = 100.                                               !JMR
	if(idur.eq.2) then
	  print *,' Enter averaging time (sec) '
	  read *, tperiod
	  write(3,559) tperiod
	  print *,' Enter time step (sec) '                          !JMR
	  read *, tstep                                              !JMR
          write(3,560) tstep
	endif
 559	format(i5,16x,'!averaging period (sec)')
 560	format(i5,16x,'!time step (sec)')

c... Choice of extreme frequencies for merging data from the two radars ...

	print *,' '
	print *,' Extreme frequencies are 8 and 20 MHz...'
	print *,' Do you want to change them? (y or n)'
	read '(a)', ans
        write(3,561) ans
 561	format(a,20x,'!Change extreme frequencies (y/n)')
	if(ans.eq.'y'.or.ans.eq.'Y') then
	  print *,' Enter radar1 extreme frequencies...'
	  print *,'	low limit 	'
	  read *, f_low(1)
	  write(3,*) f_low(1)
	  print *,'	high limit 	'
	  read *, f_high(1)
	  write(3,*) f_high(1)
	  print *,' Enter radar2 extreme frequencies...'
	  print *,'	low limit 	'
	  read *, f_low(2)
	  write(3,*) f_low(2)
	  print *,'	high limit 	'
	  read *, f_high(2)
	  write(3,*) f_high(2)
	endif

c... Altitude of virtual height and method for setting it ...

	print *,' '
   4	print *,' Setting the virtual height: Options... '
	print *,'	1 : Constant virtual height '
	print *,'	2 : Model for virtual height '
	print *,'	3 : Use elevation angle information '
	print *,' Enter choice '
	read *, ialt
        write(3,571) ialt
 571	format(i1,20x,'!virtual height option 1: constant 2: model 3: elevation')
	if(ialt.eq.2.or.ialt.eq.3) then
	  print *,' Option not ready '
	  print *,' Constant virtual height will be assumed'
	  ialt = 1
	endif
	if(ialt.eq.1) then
	  print *,' Altitude of virtual height is 400. km...'
	  print *,' Do you want to change it? (y or n) '
	  read '(a)',ans
	  write(3,581) ans
 581	  format(a,20x,'!change virtual height from 400 km (y or n)')
 582	  format(i4,16x,!altitude of virtual height (km)')
	  if(ans.eq.'y'.or.ans.eq.'Y') then
	    print *,' Altitude of virtual height ? '
	    read *, height
	    write(3,582) height
	  endif
	endif

					!real stnd height
	height_r = 100. + (height-100.) * 0.75

c... Processing of ground scatter ...

	print *,' '
   5	print *,' Processing ground scatter echoes: Options...'
	print *,'	1 : Include all echoes identified'
	print *,'	    as ground scatter by FITACF '
	print *,'	2 : Exclude all echoes identified'
	print *,'	    as ground scatter by FITACF '
	print *,'	3 : Include echoes if identified as ground'
	print *,'	    scatter by FITACF for one of the radars'
	print *,'	    but not for the other one '
	print *,'	4 : Other (to be defined) '
	print *,' Enter choice '
	read *, igs
	write(3,591) igs
 591	format(i1,20x,'!Options for processing g-s 1: incl 2: excl')
	if(igs.ge.3) then
	  print *,' Option not ready '
	  go to 5
	endif

c... E-region echoes ...

	print *,' '
   6	print *,' Processing E-region echoes: Options... '
	print *,'	1 : Include E-region echoes '
	print *,'	2 : Exclude E-region echoes by setting a'
	print *,'	    minimum range for F-region echoes'
	print *,'	3 : Exclude E-region echoes by using the'
	print *,'	    elevation angle information'
	print *,' Enter choice '
	read *, iereg
	write(3,592) iereg
 592	format(i1,20x,'!processing of E-region echoes 1:incl 2:excl')
 593	format(a,20x,'!change mim range for F-region echoes from 900 km (y/n)')
 594	format(i4,17x,'!minimum range for E region echoes (km)')
	frng_min = 900.
	if(iereg.eq.2) then
	  print *,' Minimum range for F-region echoes is 900 km...'
	  print *,' Do you want to change it? (y or n) '
	  read '(a)', ans
	  write(3,593) ans
	  if(ans.eq.'y'.or.ans.eq.'Y') then
	    print *,' Enter minimum range for F-region echoes '
	    read *, frng_min
            write(3,594) frng_min
	  endif
	else if(iereg.eq.3) then
	  print *,' Option not ready '
	  go to 6
	endif

c... Processing choices ...

	print *,' '
	print *,' Radial velocity smoothing ? [Y] '
	read '(a)', ans
        write(3,651) ans
 651	format(a,20x,'!radial velocity smoothing (y/n)')
	if(ans.eq.'n'.or.ans.eq.'N') ifltr = 0                     !JMR...
	if(ans.eq.'y'.or.ans.eq.'Y') then
	  print*,' Enter filter strength: permissive/medium/strict 0/1/2'
	  read*,ifltr
	  write(3,652) ifltr
        endif
	print*,' Now we have ans = ',ans,' and ifltr = ',ifltr     !...JMR
 652	format(i1,20x,'!filter strength perm/med/strict 0/1/2')

c	print *,' '
c	print *,' Interpolation of radial velocities to '
c	print *,' isolated missing points ? [N]'
c	read '(a)', ans
c	if(ans.eq.'y'.or.ans.eq.'Y') inter = 1

	print *,' Divfree analysis for relaxation? [N] '
	read '(a)', ans
	write(3,655) ans
 655	format(a,20x,'!div-free analysis for relaxation (y/n)')
	if(ans.eq.'y'.or.ans.eq.'Y') then
	  idiv = 1
	  if( (st2.eq.11) .or. (st2.eq.12) ) then
	    print *,' The present divergence-free method is not valid'
	    print *,' for these radars because the geographic pole is'
	    print *,' within the field of view of the radars.'
	    print *,' The divergence-free analysis will not be done.'
	    idiv = 0
	  endif
	endif

	if(idiv.eq.1) then
	  print *,' Divfree analysis for extension of the map ? [N] '
	  read '(a)', ans
	  write(3,657) ans
	  if(ans.eq.'y'.or.ans.eq.'Y') iext = 1 
	endif
 657	format(a,20x,'!divfree analysis for extension of the map (y/n)')

c  7	print *,' Increase spatial resolution ? [N] '
c	read '(a)', ans
c	if(ans.eq.'y'.or.ans.eq.'Y') ires = 1

c... Limit values for parameters ...

	print *,' '
	print *,' Minimum power is 3. db...'
	print *,' Do you want to change it? (y or n)'
	read '(a)', ans
	write(3,661) ans
 661	format(a,20x,'!change minimum power from 3 dB (y/n)')
	if(ans.eq.'y'.or.ans.eq.'Y') then
	  print *,' Minimum power ? '
	  read *, pwr_min
	  write(3,662) pwr_min
	endif
 662	format(i2,19x,'!minimum power (dB)')

	print *,' Minimum velocity is 25 m/s...'
	print *,' Do you want to change it? (y or n)'
	read '(a)', ans
	write(3,663) ans
 663	format(a,20x,'!change min velocity from 25 m/s (y/n)')
	if(ans.eq.'y'.or.ans.eq.'Y') then
	  print *,' Minimum velocity ? '
	  read *, vel_min
	  write(3,664) vel_min
	endif
 664	format(i4,17x,'!minimum velocity (m/s)')

	print *,' Maximum velocity is 3000. m/s...'
	print *,' Do you want to change it? (y or n)'
	read '(a)', ans
	write(3,665) ans
 665	format(a,20x,'!change maximum velocity from 3000 m/s (y/n)')
	if(ans.eq.'y'.or.ans.eq.'Y') then
	  print *,' Maximum velocity ? '
	  read *, vel_max
	  write(3,666) vel_max
	endif
 666	format(i5,16x,'!maximum velocity (m/s)')

	print *,' Maximum error on velocity is 50. m/s...'
	print *,' Do you want to change it? (y or n)'
	read '(a)', ans
	write(3,667) ans
 667	format(a,20x,'!change maximum velocity error from 50 m/s (y/n)')
	if(ans.eq.'y'.or.ans.eq.'Y') then
	  print *,' Maximum error velocity ? '
	  read *, velerr_max
	  write(3,668) velerr_max	  
	endif
 668	format(i3,18x,'!maximum velocity error (m/s)')

 99	print *,' '
	r = RADAR_NAME(st1)
	print '(a)', 'Radar1: ', r
	r = RADAR_NAME(st2)
	print '(a)', 'Radar2: ', r
	print *,' '


        close(3)

c Opens the output file
	outfile(1:7)  = out
	if (stnd) then
	   outfile(8:12) = '_stnd'
	   first = 13
	else
	   first = 8
	endif
	last = first + 3
	outfile(first:last) = '.mrg'
	print *, ' '
	print *, 'The name of the output file is: ', outfile(1:last)
	print *, ' '
	open(3,file=outfile,status='unknown',form='unformatted')
c	open(3,file=outfile(1:last),status='unknown',form='unformatted')

c Opens the table.dat file
	outfile(1:7)  = out
	last = first + 3
	outfile(first:last) = '.tab'
	open(unit_table,file=outfile,status='unknown',form='unformatted')

	print*,' out     = ',out
	print*,' stnd    = ',stnd
	print*,' first   = ',first,' last = ',last
	print*,' outfile = ',outfile
	
	                                                           !JMR LOS vel
	outfile(first:last) = '.los'                               !output 
	open(9,file=outfile,status='unknown',form='unformatted')   !file
c	open(9,file=outfile(1:last),status='unknown',form='unformatted')


	return
	end





