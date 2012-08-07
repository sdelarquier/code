	subroutine AVRG_SCAN(t,fdata,fchange,tchange)
c	*********************************************

c********************************************************************
c This subroutine reads the data files (.fit and .inx) and fills the
c radar/beam/range arrays
c It averages the data between times 't' and 't+tperiod', where 
c 'tperiod' is found in the inclusion 'info.inc'
c The following arrays must be declared in common data
c	p:		power array
c	v: 		velocity array
c       verr_v:		velocity error (from vel_err)
c       verr_w:		velocity error (from width)
c	width:		spectral width
c	werr:		spectral width error
c	el:		elevation angle
c	el_low:		elevation angle, low limit
c	el_high:	elevation angle, high limit
c	bad:		bad points array
c	gsc:		ground-scatter array
c	n_avg:		number of valid data points averaged during
c			time 'tperiod' at a given beam and range
c
c Input:	t:	start time of scan
c
c Outputs:	fdata(2):	.true. if scan contains available
c				       data
c		fchange(2):	.true. if lagfr or smsep changed
c				       during the scan
c		tchange(2):	time when lagfr or smsep changed
c
c Last written: Oct. 93 (CS)
c
c	$Log: avrg_scan.f,v $
c	Revision 1.4  1996/09/19 13:23:51  senior
c	Now checks if the station ID does not change.
c
c Revision 1.3  1996/09/11 08:52:14  senior
c Now recovers the values of lagfr and smsep from
c parms386.lagfr and parms386.smsep
c
c Revision 1.2  94/11/23  16:26:10  16:26:10  senior (Catherine SENIOR)
c Stops properly at the end of .fit files
c 
c Revision 1.1  94/09/05  18:19:04  18:19:04  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.4 $
c********************************************************************
	
	include 'info.inc'
	include 'radops.par'
	include 'raddat386_1_3.inc'
	parameter (ng = 75)
	equivalence (parms, parm_list)
	integer*4 FIND_FIT_REC,READ_DATREC
	integer*4 time_found,stat_find,stat_read
	integer*4 t,ct,etscan,tchange(2),CNV_MDHMS_SEC
	integer*2 mhz(2),khz(2),lag(2),smp(2),integ(2),nrg(2),station(2)
	integer*2 st_bm,f_mhz
	integer*2 lag16, sms16, st16
	integer*2 old_lag(2),old_sep(2),old_st(2)
	integer*2 bm,range_gate
	integer*2 sdate(2,6),edate(2,6)
        integer*2 num(0:15,ng),gsc(2,0:15,ng),n_avg(2,0:15,ng)
	real*4	  pos(2,5), st_freq
	real*4    v(2,0:15,ng),p(2,0:15,ng)
	real*4	  verr_v(2,0:15,ng),verr_w(2,0:15,ng)
	real*4	  width(2,0:15,ng),werr(2,0:15,ng)
	real*4    el(2,0:15,ng),el_low(2,0:15,ng),el_high(2,0:15,ng)
        real*4    pit(0:15,ng),vit(0:15,ng)
	real*4	  vev(0:15,ng),vew(0:15,ng)
	real*4	  wid(0:15,ng),wed(0:15,ng)
	real*4    l(0:15,ng),llow(0:15,ng),lhigh(0:15,ng)
	logical*1 bad(2,0:15,ng),bflag(2,0:15)
	logical*1 fdata(2),fchange(2)
	common/radars/pos
	common/date/sdate,edate
	common/param/mhz,khz,lag,smp,integ,nrg
	common/old_param/old_lag,old_sep,old_st
	common/data/p,v,verr_v,verr_w,width,werr,el,el_low,el_high,
     &			bad,gsc,n_avg
c_______________________________


	do k=1,2				!radar loop

	icount = 0
	n = 0
	rxris = pos(k,5)
	fdata(k) = .false.
	fchange(k) = .false.
	tchange(k) = 0

c... Initializes temporary arrays ...

        do ib=0,15
	   bflag(k,ib) = .false.
           do ir=1,ng
              num(ib,ir) = 0
              pit(ib,ir) = 0.
              vit(ib,ir) = 0.
              vev(ib,ir) = 0.
              vew(ib,ir) = 0.
	      wid(ib,ir) = 0.
	      wed(ib,ir) = 0.
	      l(ib,ir) = 0.
	      llow(ib,ir) = 0.
	      lhigh(ib,ir) = 0.
           end do
        end do

c... Reads data ...

	etscan = t + int(tperiod) - 1

	stat_find = FIND_FIT_REC(%val(ifileptr(k)),%val(t),time_found)
c	if(stat_find.ne.0) then
c	  print *,' AVRG_SCAN: radar, t, stat_find = ',k,t,stat_find
c	endif

 	stat_read = 1
	rec_time = 0
	do while ((stat_read.eq.1).and.(rec_time.lt.t))
 	  stat_read = READ_DATREC(ifileptr(k))
	enddo
	do while ((stat_read.eq.1).and.(rec_time.le.etscan))
c	  if(stat_read.ne.1) then
c	    print *,' AVRG_SCAN: radar, t, stat_read = ',k,t,stat_read
c	  endif
	  f_mhz = parms386.tfreq/1000
	  if((f_mhz.lt.f_low(k)).or.(f_mhz.gt.f_high(k))) goto 10

  	  icount = icount + 1

c... Start time of scan, and radops parameters ...

	  if(icount.eq.1) then
	    sdate(k,1) = parms386.year		!start time of first
	    sdate(k,2) = parms386.month		!included beam of
	    sdate(k,3) = parms386.day		!the scan
	    sdate(k,4) = parms386.hour
	    sdate(k,5) = parms386.minut
	    sdate(k,6) = parms386.sec

	    lag(k) = parms386.lagfr
	    smp(k) = parms386.smsep
	    nrg(k) = parms386.nrang
	    mhz(k) = parms386.tfreq/1000
	    khz(k) = parms386.tfreq - mhz(k)*1000
	    integ(k) = parms386.intt
	    station(k) = parms386.st_id

	    st_freq = parms386.tfreq/1000.
	    st_bm = parms386.bmnum

	    if(k.eq.1) print *,' '
	    print 55, k,(sdate(k,ii),ii=1,6),st_freq,st_bm
 55	    format(' Radar',i3,':',i6,5i4,5x,f8.3,' Mhz, beam ',i2)
	  endif

c... Adds data from time 't' to time 't+tperiod' ...

	  sms16 = parms386.smsep
	  lag16 = parms386.lagfr
	  if((lag16.ne.old_lag(k)).or.(sms16.ne.old_sep(k))) then
	    fchange(k) = .true.
	    tchange(k) = rec_time
	    print *,' '
	    print *,' Radar',k,': lagfr or smsep have changed at ',
     &			year,month,day,hour,minut,sec
	    print *,' The Lookup Table will be recalculated'
	    print *,' '
	    go to 80
	  endif

	  st16  = parms386.st_id
	  if(st16.ne.old_st(k)) then
	    print *,' '
	    print *,' Radar',k,': st_id has changed at ',
     &			year,month,day,hour,minut,sec
	    print *,' If you have concateneted files, you may'
	    print *,' have inserted data from another radar.'
	    print *,' THE PROGRAM WILL STOP'
	    print *,' '
	    stop
	  endif

          edate(k,1) = parms386.year		!start time of last included
          edate(k,2) = parms386.month		!beam of the scan
          edate(k,3) = parms386.day
          edate(k,4) = parms386.hour
          edate(k,5) = parms386.minut
          edate(k,6) = parms386.sec

	  bm = parms386.bmnum
	  bflag(k,bm) = .true.

	  do ir=1,parms386.nrang

	    if(qflag(ir).eq.1) then
	      if(iereg.ne.1) then			!E_region elim.
	        range_gate = ir
C uses now frang and rsep
	        d = SLANT_RANGE(lag16* 0.15,sms16* 0.15,rxris,0.,range_gate)
	        fmin = frng_min
	        if(d.lt.fmin) go to 110
	      endif
	      if((igs.eq.2).and.(gscat(ir).ne.0)) then	!grnd scat. elim.
	        go to 110
	      endif
	      if((pwr_l(ir).ge.pwr_min).and.		!data elim.
     *	         (abs(vel(ir)).lt.vel_max).and.
     *		 (abs(vel(ir)).gt.vel_min).and.
     *		 (vel_err(ir).lt.velerr_max)) then
                num(bm,ir) = num(bm,ir) + 1
                pit(bm,ir) = pit(bm,ir) + pwr_l(ir)
                vev(bm,ir) = vev(bm,ir) + vel_err(ir)**2
                vew(bm,ir) = vew(bm,ir) + width_l(ir)**2
                vit(bm,ir) = vit(bm,ir) + vel(ir)
	        wid(bm,ir) = wid(bm,ir) + width_l(ir)
	        wed(bm,ir) = wed(bm,ir) + width_l_err(ir)**2
	        l(bm,ir) = l(bm,ir) + elev(ir)
	        llow(bm,ir) = llow(bm,ir) + elev_low(ir)
	        lhigh(bm,ir) = lhigh(bm,ir) + elev_high(ir)
	        n = n+1
              endif
	    endif
 110	    continue
	  enddo
 10	  continue
	  stat_read = READ_DATREC(ifileptr(k))
	  if(stat_read .ne. 1) stop
	enddo


c... Computes average parameters ...

  80	if(n.ne.0) fdata(k) = .true.

	  write(4,344) k,(sdate(k,n),n=1,6),
     &			 (edate(k,n),n=1,6),
     &			  st_bm,st_freq
 344	  format('radar ',i1,':',2x,i4,5i3,'  to: ',i4,5i3,
     &			';   st_beam ',i2,',',f7.3,' Mhz')

        do ib=0,15
          do ir=1,ng
            if(num(ib,ir).ne.0) then
	      n_avg(k,ib,ir) = num(ib,ir)
              p(k,ib,ir) = pit(ib,ir)/num(ib,ir)
              v(k,ib,ir) = vit(ib,ir)/num(ib,ir)
              verr_v(k,ib,ir) = sqrt(vev(ib,ir))/num(ib,ir)
              verr_w(k,ib,ir) = sqrt(vew(ib,ir))/num(ib,ir)
	      width(k,ib,ir) = wid(ib,ir)/num(ib,ir)
	      werr(k,ib,ir) = sqrt(wed(ib,ir))/num(ib,ir)
	      el(k,ib,ir) = l(ib,ir)/num(ib,ir)
	      el_low(k,ib,ir) = llow(ib,ir)/num(ib,ir)
	      el_high(k,ib,ir) = lhigh(ib,ir)/num(ib,ir)
              gsc(k,ib,ir) = 0
	      bad(k,ib,ir) = .false.
            endif
          enddo
        enddo

c... Fills the empty beams of the Schefferville scan ...
c... (Schefferville  has 8 beams instead of 16) ...

	if((k.eq.1).and.(st1.eq.2)) then
	do jb=1,14
	if(.not.bflag(k,jb).and.(bflag(k,jb-1).and.bflag(k,jb+1))) then
	  bflag(k,jb) = .true.
	  do jr=1,ng
	   if(.not.bad(k,jb-1,jr).and.(.not.bad(k,jb+1,jr))) then
             bad(k,jb,jr) = .false.
	     n_avg(k,jb,jr) = max(n_avg(k,jb-1,jr),n_avg(k,jb+1,jr))
	     p(k,jb,jr) = (p(k,jb-1,jr)+p(k,jb+1,jr))/2.
	     v(k,jb,jr) = (v(k,jb-1,jr)+v(k,jb+1,jr))/2.
	     width(k,jb,jr) = (width(k,jb-1,jr)+width(k,jb+1,jr))/2.
             verr_v(k,jb,jr) = sqrt(verr_v(k,jb-1,jr)**2 +
     &				    verr_v(k,jb+1,jr)**2)/2.
             verr_w(k,jb,jr) = sqrt(verr_w(k,jb-1,jr)**2 +
     &				    verr_w(k,jb+1,jr)**2)/2.
	     werr(k,jb,jr)=sqrt(werr(k,jb-1,jr)**2+werr(k,jb+1,jr)**2)/2.
	     el(k,jb,jr) = (el(k,jb-1,jr)+el(k,jb+1,jr))/2.
	     el_low(k,jb,jr) = (el_low(k,jb-1,jr)+el_low(k,jb+1,jr))/2.
	     el_high(k,jb,jr)=(el_high(k,jb-1,jr)+el_high(k,jb+1,jr))/2.
	     gsc(k,jb,jr) = max(gsc(k,jb-1,jr),gsc(k,jb+1,jr))
	     if((igs.eq.2).and.(gsc(k,jb,jr).ne.0)) bad(k,jb,jr) = .true.
	   endif
	  enddo
	endif
	enddo
	endif

 	enddo					!end of radar loop

	return
	end

