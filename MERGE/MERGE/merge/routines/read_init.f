	subroutine READ_INIT(t,ierror)
c	******************************

c********************************************************************
c This subroutine reads the data files (.fit and .inx) to initialize
c the radar parameters of the common 'param', and the date in common
c 'date'.
c
c Output:
c	ierror = 0 	if time_found > or = t (from FIND_FIT_REC)
c	ierror = 1	if time_found < t
c			In that case, nothing is done
c
c Last written: Oct. 93 (CS)
c
c	$Log: read_init.f,v $
c	Revision 1.5  1996/09/19 13:31:26  senior
c	Changed to allow scan by scan merging. Also recognizes if station
c	 has changed.
c
c Revision 1.5  96/03/12  10:14:36  10:14:36  senior (Catherine SENIOR)
c Now recovers the values of lagfr and smsep from
c parms386.lagfr and parms386.smsep
c 
c Revision 1.4  95/09/14  16:39:52  16:39:52  senior (Catherine SENIOR)
c Modified to take into account the new CP parameters
c defined by Kile Baker (29 august 1995)
c 
c Revision 1.3  95/08/03  16:13:38  16:13:38  senior (Catherine SENIOR)
c Checks that the data being processed are Common Programs
c This is done only after August 1st, 1995.
c 
c Revision 1.2  95/01/11  10:03:16  10:03:16  senior (Catherine SENIOR)
c Starts properly, even if the start time given is smaller
c than the start time of the fit files.
c 
c Revision 1.1  94/09/05  17:34:54  17:34:54  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.5 $
c********************************************************************

	include 'info.inc'
	include 'radops.par'
	include 'raddat386_1_3.inc'
	equivalence (parms, parm_list)
	integer*4 FIND_FIT_REC, READ_DATREC
	integer*4 t,tend,time_found,stat_find,stat_read
	integer*2 sdate(2,6),edate(2,6)
	integer*2 mhz(2),khz(2),lag(2),smp(2),integ(2),nrg(2),station(2)
	integer*2 old_lag(2),old_sep(2),old_st(2)
	integer*2 cp_code(2)
	common/param/mhz,khz,lag,smp,integ,nrg
	common/old_param/old_lag,old_sep,old_st
	common/date/sdate,edate
c_______________________________


	ierror = 0
	do k=1,2				!radar loop

	  rec_time = 0
	  stat_find = FIND_FIT_REC(%val(ifileptr(k)),%val(t),time_found)

	  tend = t + ifix(tperiod/2)
	  if((stat_find.ne.0).and.(time_found.gt.tend)) then
c	    print *,' READ_INIT: radar, t, time_found = ', k,t,time_found
	    ierror = 1
	    return
	  endif

	  stat_read = 1
	  do while ((stat_read.eq.1).and.(rec_time.lt.t))
	    stat_read = READ_DATREC(ifileptr(k))
	  enddo
c	  if (stat_read.ne.1) then
c	    print *,' READ_INIT: radar, t, stat_read = ',k,t,stat_read
c	  endif

	  sdate(k,1) = parms386.year
	  sdate(k,2) = parms386.month
	  sdate(k,3) = parms386.day
	  sdate(k,4) = parms386.hour
	  sdate(k,5) = parms386.minut
	  sdate(k,6) = parms386.sec

	  cp_code(k) = parms386.cp

	  lag(k) = parms386.lagfr
	  smp(k) = parms386.smsep
	  nrg(k) = parms386.nrang
	  mhz(k) = parms386.tfreq/1000
	  khz(k) = parms386.tfreq - mhz(k)*1000
	  integ(k) = parms386.intt
	  station(k) = parms386.st_id

	  old_lag(k) = lag(k)
	  old_sep(k) = smp(k)
	  old_st(k)  = station(k)

 	enddo					!end of radar loop

c... Checks that this is a Common Program for data after 950801 ...

	if(sdate(1,1).gt.1900) then
	   iy = sdate(1,1) - 1900
	else
	   iy = sdate(1,1)
	endif
	idate = iy*10000 + sdate(1,2)*100 + sdate(1,3)

	if(idate.ge.950801) call CP_RUN(cp_code(1),cp_code(2))

c... Checks if one by one scan merging is possible

	if(idur.eq.1) then
	   if((abs(cp_code(1)).lt.100).or.(abs(cp_code(1)).ge.200).or.
     &	      (abs(cp_code(2)).lt.100).or.(abs(cp_code(2)).ge.200)) then
		print *, ' '
		print *, 'THE ONE BY ONE SCAN MERGING OPTION IS NOT'
		print *, 'AVAILABLE WHEN ONE OR BOTH OF THE TWO RADARS'
		print *, 'DO NOT OPERATE WITH NORMAL SCANS.'
		print *, ' '
		print *, 'THE INTEGRATION TIME HAS BEEN SET TO 120 SEC.'
		idur = 2
	   endif
	endif

	return
	end
