	subroutine READ_SCAN(t,flag_data,flag_change,time_change)
c	*********************************************************
c
c**********************************************************************
c This routine calls one of the two reading routines: 
c AVRG_SCAN for averaging data over time 'tperiod' (in 'info.inc')
c ONE_SCAN  for processing data from single scans
c It fills in the arrays contained in the common 'data'
c The data may be filtered if required
c
c	Input:	t = start time of scan or of averaging period
c		    (from cnv$mdhms_sec)
c	Outputs: 
c		flag_data:	.true.  if there is data for both
c					radars
c		flag_change:	.true.  if lagfr or smsep or st_id for one
c					of the radars have changed
c		time_change:	time at which lagfr or smsep or st_id have
c				changed (the largest of the 2 radars)
c
c Last written: Oct. 93 (CS)
c
c	$Log: read_scan.f,v $
c	Revision 1.3  1996/09/19 13:33:04  senior
c	Recognizes if station ID has changed
c
c Revision 1.2  95/01/11  14:31:18  14:31:18  senior (Catherine SENIOR)
c Modified to use the new MEDIAN_FILTER routine from JMR
c 
c Revision 1.1  94/09/01  16:14:01  16:14:01  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.3 $
c**********************************************************************

	include   'info.inc'
	parameter (ng = 75)
	integer*4 t,tchange(2),time_change
	integer*2 sdate(2,6),edate(2,6)
	integer*2 mhz(2),khz(2),lagfr(2),smsep(2),intt(2),nrang(2)
	integer*2 old_lag(2),old_sep(2),old_st(2),nbeams
	integer*2 gsc(2,0:15,ng),n_avg(2,0:15,ng)
	integer*2 unit_prt
	real*4    p(2,0:15,ng),v(2,0:15,ng)
	real*4	  verr_v(2,0:15,ng),verr_w(2,0:15,ng)
	real*4    width(2,0:15,ng),werr(2,0:15,ng)
	real*4    el(2,0:15,ng),el_low(2,0:15,ng),el_high(2,0:15,ng)
	real*4    vr(16,ng)
	logical*1 bad(2,0:15,ng),fdata(2),fchange(2)
	logical*1 flag_data,flag_change
	logical*1 gbad(16,ng),gscat(16,ng)
	common/date/sdate,edate
	common/param/mhz,khz,lagfr,smsep,intt,nrang
	common/old_param/old_lag,old_sep,old_st
	common/data/p,v,verr_v,verr_w,width,werr,el,el_low,el_high,
     &			bad,gsc,n_avg
	data unit_prt/4/
	data nbeams/16/
c_______________________________


c... Initialisations ...

	flag_data = .true.
	flag_change = .false.
	time_change = 0

	do k=1,2
	  do ib=0,15
	    do ir=1,nrang(k)
	      p(k,ib,ir) = 0.
	      v(k,ib,ir) = 0.
	      verr_v(k,ib,ir) = 0.
	      verr_w(k,ib,ir) = 0.
	      width(k,ib,ir) = 0.
	      werr(k,ib,ir) = 0.
	      el(k,ib,ir) = 0.
	      el_low(k,ib,ir) = 0.
	      el_high(k,ib,ir) = 0.
	      bad(k,ib,ir) = .true.
	      gsc(k,ib,ir) = 1
	      n_avg(k,ib,ir) = 0
	    enddo
	  enddo
	enddo

	if(idur.eq.1) call  ONE_SCAN(t,fdata,fchange,tchange)
	if(idur.eq.2) call AVRG_SCAN(t,fdata,fchange,tchange)

	do k=1,2
	  if(.not.fdata(k)) flag_data = .false.
	  if(fchange(k)) flag_change = .true.
	enddo

	if(flag_change) time_change = max(tchange(1),tchange(2))
	if((.not.flag_data).and.(.not.flag_change)) then
	  print *,'   No Common Data '
	  write(unit_prt,100)
 100	  format('    No Common Data ')
	  return
	endif

c... Filters bad velocities ...

	if(ifltr.eq.1) then
	  do k=1,2
	    do ib=1,16
	      do ir=1,nrang(k)
		vr(ib,ir) = 0.
		gbad(ib,ir) = .true.
		gscat(ib,ir) = 1
	      enddo
	    enddo
	    do ib=1,16
	      do ir=1,nrang(k)
		vr(ib,ir) = v(k,ib-1,ir)
		gbad(ib,ir)  = bad(k,ib-1,ir)
		gscat(ib,ir) = gsc(k,ib-1,ir)
	      enddo
	    enddo

c	    call SMOOTH(gbad,vr,nbeams,nrang(k))
c	Mod to use Rev. 1.5 of median_filter
	    call MEDIAN_FILTER(gbad,vr,gscat, ifltr, nbeams,nrang(k))

	    do ib=0,15
	      do ir=1,nrang(k)
		v(k,ib,ir) = vr(ib+1,ir)
		bad(k,ib,ir) = gbad(ib+1,ir)
		gsc(k,ib,ir) = gscat(ib+1,ir)
	      enddo
	    enddo
	  enddo
	endif

	return
	end
