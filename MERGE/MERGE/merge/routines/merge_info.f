	subroutine MERGE_INFO( unit_dat, unit_los, unit_prt)   ! DA
c	*********************
c
c**********************************************************************
c This routine sets up the different options of the merge process
c These options are kept in the inclusion 'info.inc'
c
c $Log: merge_info.f,v $
c
c Revision 2.0  2000/01/11 DA
c Changed everything to accept four digit years ONLY !!!
c
c Revision 1.9  1997/10/06 DA
c Allowed filtering levels 1-3 for median_filter
c
c Revision 1.8  1996/10/01 14:58:00  senior
c The way to answer the different questions has been changed:
c Now, the user may enter 'ctrl^D' if he chooses the standard
c option.
c
c Revision 1.7  1996/09/19 13:26:04  senior
c - Changed to keep istnd in the header (see also info.inc)
c   istnd indicates if standard or non-standard options were chosen
c - Changed the name of the output file. The directory into which
c   this output file goes must be specified by the SD_MRGOPEN_PATH
c   environment variable
c - Gives the possibility of merging single scans from the 2 radars.
c
c Revision 1.6  1996/09/11 10:06:28  senior
c Changed standard options for tperiod and velerr_max
c
c Revision 1.5  95/01/31  17:17:15  17:17:15  senior (Catherine SENIOR)
c Define frng_min in standard parameters
c Change the output file coding (Use the 2 station letters 
c rather than a single coded letter
c 
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
c	$Revision: 1.8 $
c**********************************************************************

	include 	'info.inc'
	include		'radops.par'
	include 	'raddat386_1_3.inc'
	equivalence	(parms,parm_list)
	integer*4 	st,et,CNV_MDHMS_SEC
	integer*4	FITROPEN, READ_DATREC,FIT_CLOSE
	integer*4 	iostat
	integer*2 	kst
	integer*2 	unit_dat, unit_los, unit_prt, unit_table     ! DA
	character*1  	ans
	character*1	NUL, TAB
	character*1	x, PAIR_NAME
	character*1	let1, let2, let
	character*20	r, RADAR_NAME
	character*50	outf, outfile, MRG_NAME
	character*10	out				! 20000111 DA
	character*50 	fidat,fidat1,fidat2
	character*50	fname1, fname2
	logical*1	afname1(50), afname2(50)
	logical*1	bnul /0/
	logical*1	btab /9/
	data unit_table/16/                      ! DA
	equivalence	(afname1,fname1), (afname2,fname2)
	equivalence	(NUL,bnul), (TAB, btab)

c___________________________


c... Opens data files ...

 10	print *,' Enter Radar1 file name (with no extension)'
	read '(a)', fidat1
	out(1:8) = fidat1(1:8)		! 20000111 DA
	let1 = fidat1(11:11)		! 20000111 DA
	do i=50,1,-1
	  if(fidat1(i:i) .eq. ' ') fidat1(i:i) = NUL
	  if(fidat1(i:i) .eq. TAB) fidat1(i:i) = NUL
	enddo
	fname1 = fidat1
	ifileptr(1) = fitropen(afname1)
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
	let2 = fidat2(11:11)		! 20000111 DA
	do i=50,1,-1
	  if(fidat2(i:i) .eq. ' ') fidat2(i:i) = NUL
	  if(fidat2(i:i) .eq. TAB) fidat2(i:i) = NUL
	enddo
	fname2 = fidat2
	ifileptr(2) = fitropen(afname2)
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
	    iostat = fit_close(%val(ifileptr(k)))
	  enddo
	  kst = st1		!Exchanges station IDs
	  st1 = st2
	  st2 = kst
	  let = let1
	  let1 = let2
	  let2 = let
	  fidat  = fidat1	!Exchanges files
	  fidat1 = fidat2
	  fidat2 = fidat
	  fname1 = fidat1
	  fname2 = fidat2
	  ifileptr(1) = fitropen(afname1)
	  ifileptr(2) = fitropen(afname2)
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
c	x = PAIR_NAME(st1,st2)
c	out(7:7) = x
	out(9:9) = let1		! 20000111 DA
	out(10:10) = let2	! 20000111 DA

c... Choice of start and end time ...

	print *,' '
	print *,' Enter year (yyyy)'
	read *, syr
	eyr = syr
   2	print *,' Enter start time (mm,id,ih,mn,is)'
	read *, smo,sday,shr,smin,ssec
	print *, 'Enter  end  time (mm,id,ih,mn,is)'
	read *, emo,eday,ehr,emin,esec
	st = cnv_mdhms_sec(syr,smo,sday,shr,smin,ssec)
	et = cnv_mdhms_sec(eyr,emo,eday,ehr,emin,esec)
	if(et.lt.st) then
	  print *,' end time should be larger than start time '
	  go to 2
	endif

c... Choice of Standard Options ...

	idur_stnd	= 2			!Standard Options
	tperiod_stnd	= 240.			!averages two 2-min scans
	f_low_stnd	= 8
	f_high_stnd	= 20
	ialt_stnd	= 1
	height_stnd	= 400.
	frng_min_stnd	= 900.
	igs_stnd	= 2
	iereg_stnd	= 1
	ifltr_stnd	= 1
	inter_stnd	= 0
	idiv_stnd	= 0
	iext_stnd	= 0
	ires_stnd	= 0
	pwr_min_stnd	= 3.
	vel_min_stnd	= 25.
	vel_max_stnd	= 3000.
	velerr_max_stnd	= 100.
	if( (fit_vers1 .lt. 250) .and. (fit_vers2 .lt. 250) ) velerr_max_stnd = 50.
	istnd = 1
				!End of Standard Options

	print *,' '
	print *, ' Do you want Standard Options (y/n) ?...'
	read '(a)', ans
	if(ans.eq.'y'.or.ans.eq.'Y') then
		istnd 		= 0
		idur 		= idur_stnd
	 	tperiod 	= tperiod_stnd
		f_low(1)  	= f_low_stnd
		f_low(2)	= f_low_stnd
		f_high(1)	= f_high_stnd
		f_high(2)	= f_high_stnd
		ialt 		= ialt_stnd
		height		= height_stnd
		frng_min 	= frng_min_stnd
		igs		= igs_stnd
		iereg 		= iereg_stnd
		ifltr		= ifltr_stnd
		inter 		= inter_stnd
		idiv		= idiv_stnd
		iext		= iext_stnd
		ires		= ires_stnd
		pwr_min		= pwr_min_stnd
		vel_min		= vel_min_stnd
		vel_max		= vel_max_stnd
		velerr_max	= velerr_max_stnd 
	   	go to 99
	endif
	   

c... Averaged or individual maps ...

	print *,' '
   3	print *,' Averaging maps: Options...'
	print *,'	1 : map for each scan '
	print *,'	2 : average maps '
   	print *,' Enter choice [ctrl^D =', idur_stnd,']'
 	read(*,*,iostat=ios) idur
	if(ios.eq.-1) idur = idur_stnd
	if(ios.gt.0.or.idur.gt.2) then
		print *, 'INVALID CHOICE'
		go to 3
	endif
	if(idur.eq.1) tperiod = 120.
	if(idur.eq.2) then
  33	  print *,' Enter averaging time in sec [ctrl^D =', int(tperiod_stnd), ' sec]'
	  read(*,*,iostat=ios) tperiod
	  if(ios.eq.-1) tperiod = tperiod_stnd
	  if(ios.gt.0.or.tperiod.le.0.) then
		print *, 'INVALID CHOICE'
		go to 33
	  endif
	endif

c... Choice of extreme frequencies for merging data from the two radars ...

	print *,' '
  17	print *,' Extreme frequencies are ',f_low_stnd,' and ', f_high_stnd,' MHz...'
	print *,' Do you want to change them ? (y/n) [ctrl^D = n]'
	read (*,*,iostat=ios) ans
	if(ios.eq.-1.or.ans.eq.'n'.or.ans.eq.'N') then
		f_low(1)  = f_low_stnd
		f_low(2)  = f_low_stnd
		f_high(1) = f_high_stnd
		f_high(2) = f_high_stnd
	else if(ios.gt.0) then
		print *, 'INVALID CHOICE'
		go to 17
	else
	  	print *,' Enter radar1 extreme frequencies...'
	  	print *,'	low limit, high_limit...'
	  	read *, f_low(1), f_high(1)
	  	print *,' Enter radar2 extreme frequencies...'
	  	print *,'	low limit, high_limit...'
	  	read *, f_low(2), f_high(2)
	endif

c... Altitude of virtual height and method for setting it ...

	print *,' '
   4	print *,' Setting the virtual height: Options... '
	print *,'	1 : Constant virtual height '
	print *,'	2 : Model for virtual height '
	print *,'	3 : Use elevation angle information '
	print *,' Enter choice [ctrl^D =',ialt_stnd,']'
 	read(*,*,iostat=ios) ialt
	if(ios.eq.-1) ialt = ialt_stnd
	if(ios.gt.0.or.ialt.gt.3) then
		print *, 'INVALID CHOICE'
		go to 4
	endif
	if(ialt.eq.2.or.ialt.eq.3) then
	  	print *,' OPTION NOT READY'
	  	print *,' Constant virtual height will be assumed'
	  	ialt = 1
	endif
  44	if(ialt.eq.1) then
	  print *,' Altitude of virtual height ? [ctrl^D =',int(height_stnd), ' km]' 
	  read(*,*,iostat=ios) height
	  if(ios.eq.-1) height = height_stnd
	  if(ios.gt.0.or.height.le.0.) then
		print *, 'INVALID CHOICE'
		go to 44
	  endif
	endif

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
	print *,' Enter choice [ctrl^D =',igs_stnd,']'
	read (*,*,iostat=ios) igs
	if(ios.eq.-1) igs = igs_stnd
	if(ios.gt.0.or.igs.gt.4) then
		print *, 'INVALID CHOICE'
		go to 5
	endif
	if(igs.ge.3) then
	  	print *,' OPTION NOT READY '
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
	print *,' Enter choice [ctrl^D =',iereg_stnd,']'
	read (*,*,iostat=ios) iereg
	if(ios.eq.-1) iereg = iereg_stnd
	if(ios.gt.0.or.iereg.gt.3) then
		print *, 'INVALID CHOICE'
		go to 6
	endif
	if(iereg.eq.3) then
	  print *,' OPTION NOT READY '
	  go to 6
	endif

	if(iereg.eq.2) then
  66	  print *,' Minimum range for F-region echoes [ctrl^D =', int(frng_min_stnd), ' km]'
	  read(*,*,iostat=ios) frng_min
	  if(ios.eq.-1) frng_min = frng_min_stnd
	  if(ios.gt.0.or.frng_min.le.0.) then
		print *, 'INVALID CHOICE'
		go to 66
	  endif
	endif

c... Processing choices ...

c	print *,' '
c  8	print *,' Radial velocity filtering (y/n)? [ctrl^D = y] '
c	read (*,*,iostat=ios) ans
c	if(ios.eq.-1.or.ans.eq.'y'.or.ans.eq.'Y') then
c		ifltr = ifltr_stnd
c	else if(ios.gt.0) then
c		print *, 'INVALID CHOICE'
c		go to 8
c	else if(ans.eq.'n'.or.ans.eq.'N') then
c		ifltr = 0
c	endif

C Modified to allow selection of all three levels of filtering in
C Mike Ruohoniemis median_filter program; DA 06-OCT-1997

	print *,' '
  8	print *,' Radial velocity filtering (y/n)? [ctrl^D = n] '
	read (*,*,iostat=ios) ans
	if(ios.eq.-1.or.ans.eq.'n'.or.ans.eq.'N') then
		ifltr = 0
	else if(ios.gt.0) then
		print *, 'INVALID CHOICE'
		go to 8
	else if(ans.eq.'y'.or.ans.eq.'Y') then
		print *,'	Enter Level of filtering:'
		print *,'	1, 2, or 3'
		read (*,*,iostat=ios) ifltr
		if (ios .gt. 0 .or. ifltr .lt. 1 .or. ifltr .gt. 3) goto 8
	endif

c	print *,' '
c  9	print *,' Interpolation of radial velocities to '
c	print *,' isolated missing points (y/n)? [ctrl^D = n]'
c	read (*,*,iostat=ios) ans
c	if(ios.eq.-1.or.ans.eq.'n'.or.ans.eq.'N') then
		inter = inter_stnd
c	else if(ios.gt.0) then
c		print *, 'INVALID CHOICE'
c		go to 9
c	else if(ans.eq.'y'.or.ans.eq.'Y') then
c	  	print *, ' OPTION NOT READY '
c		print *, ' Interpolation will NOT be done'
c	endif

	print *,' '
  11	print *,' Divfree analysis for relaxation (y/n) ? [ctrl^D = n] '
	read (*,*,iostat=ios) ans
	if(ios.eq.-1.or.ans.eq.'n'.or.ans.eq.'N') then
		idiv = idiv_stnd
	else if(ios.gt.0) then
		print *, 'INVALID CHOICE'
		go to 11
	else if(ans.eq.'y'.or.ans.eq.'Y') then
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
	  print *, ' '
  12	  print *,' Divfree analysis for extension of the map (y/n)? [ctrl^D = n] '
	  read (*,*,iostat=ios) ans
	  if(ios.eq.-1.or.ans.eq.'n'.or.ans.eq.'N') then
		iext = iext_stnd
	  else if(ios.gt.0) then
		print *, 'INVALID CHOICE'
		go to 12
	  else if(ans.eq.'y'.or.ans.eq.'Y') then
		iext = 1
	  endif
	endif

c	print *, ' '
c  7	print *,' Increase spatial resolution (y/n) ? [ctrl^D = n] '
c	read (*,*,iostat=ios) ans
c	if(ios.eq.-1.or.ans.eq.'n'.or.ans.eq.'N') then
		ires = ires_stnd
c	else if(ios.gt.0) then
c		print *, 'INVALID CHOICE'
c		go to 7
c	else if(ans.eq.'y'.or.ans.eq.'Y') then
c	  	print *, ' OPTION NOT READY '
c		print *, ' Spatial resolution will NOT be increased'
c	endif

c... Limit values for parameters ...

	print *,' '
  13	print *,' Minimum power ? [ctrl^D =', int(pwr_min_stnd), ' db]'
	read(*,*,iostat=ios) pwr_min
	if(ios.eq.-1) pwr_min = pwr_min_stnd
	if(ios.gt.0) then
		print *, 'INVALID CHOICE'
		go to 13
	endif

	print *,' '
  14	print *,' Minimum velocity ? [ctrl^D =', int(vel_min_stnd), ' m/s]'
	read(*,*,iostat=ios) vel_min
	if(ios.eq.-1) vel_min = vel_min_stnd
	if(ios.gt.0) then
		print *, 'INVALID CHOICE'
		go to 14
	endif

	print *,' '
  15	print *,' Maximum velocity ? [ctrl^D =', int(vel_max_stnd), ' m/s]'
	read(*,*,iostat=ios) vel_max
	if(ios.eq.-1) vel_max = vel_max_stnd
	if(ios.gt.0) then
		print *, 'INVALID CHOICE'
		go to 15
	endif

	print *,' '
  16	print *,' Maximum error on velocity ? [ctrl^D =', int(velerr_max_stnd), ' m/s]'
	read(*,*,iostat=ios) velerr_max
	if(ios.eq.-1) velerr_max = velerr_max_stnd
	if(ios.gt.0) then
		print *, 'INVALID CHOICE'
		go to 16
	endif

c... Print Options ...

  99	print *, ' '
	print *, 'OPTIONS:'
	print *, '--------'
	print *, 'idur = ', idur
	print *, 'tperiod = ', tperiod
	print *, 'Frequency limits for radar1 = ', f_low(1), f_high(1)
	print *, 'Frequency limits for radar2 = ', f_low(2), f_high(2)
	print *, 'ialt = ', ialt
	print *, 'height = ', height
	print *, 'igs = ', igs
	print *, 'iereg = ', iereg
	if(iereg.eq.2) print *, 'frng_min = ', frng_min
	print *, 'ifltr = ', ifltr
	print *, 'inter = ', inter
	print *, 'idiv = ', idiv
	print *, 'iext = ', iext
	print *, 'ires = ', ires
	print *, 'pwr_min = ', pwr_min
	print *, 'vel_min = ', vel_min
	print *, 'vel_max = ', vel_max
	print *, 'velerr_max = ', velerr_max


					!real stnd height
	height_r = 100. + (height-100.) * 0.75

c... Prints the names of the radars ...
	print *,' '
	r = RADAR_NAME(st1)
	print '(a)', 'Radar1: ', r
	r = RADAR_NAME(st2)
	print '(a)', 'Radar2: ', r
	print *,' '

c... Opens the output files ...
c... Merge and Line of sight velocities and TAB ; DA

	outf = out(:lnblnk_(out)) // '.mrg'
	outfile = MRG_NAME(outf)
	print *, ' '
	print *, 'The name of the merge output file is: ', outfile
	print *, ' '
c... changed last -> lnblnk(outfile)
	open( unit_dat,file=outfile(1:lnblnk_(outfile)),status='unknown',form='unformatted')

	outf = out(:lnblnk_(out)) // '.los'
	outfile = MRG_NAME(outf)
	print *, ' '
	print *, 'The name of the vlos output file is: ', outfile
	print *, ' '
	open( unit_los,file=outfile(1:lnblnk_(outfile)),status='unknown',form='unformatted')

	outf = out(:lnblnk_(out)) // '.tab'
	outfile = MRG_NAME(outf)
	print *, ' '
	print *, 'The name of the vlos output file is: ', outfile
	print *, ' '
	open( unit_table,file=outfile(1:lnblnk_(outfile)),status='unknown',form='unformatted')


	outf = out(:lnblnk_(out)) // '.prt'
	outfile = MRG_NAME(outf)
	print *, ' '
	print *, 'The name of the prot output file is: ', outfile
	print *, ' '
	open( unit_prt,file=outfile(1:lnblnk_(outfile)),status='unknown',form='formatted')

	return
	end

