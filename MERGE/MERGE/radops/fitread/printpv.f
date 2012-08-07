	program printpv(outfile)
	implicit none
	include '/home/darn/include/radops.par'
	include '/home/darn/include/raddat386_1_3.inc'
	equivalence(parms,parm_list)
	character*80 outfile
	integer*4 nlen
	integer*4 stime,etime,cnv_mdhms_sec
	integer*4 time_found
	integer*4 iostat,read_datrec
	real*4 ts1
	parameter pi=3.1415927
	real*4 Re/6357./	!polar radius of earth
	real*4 pos(2,2,2),lat,long
	character*1 gf
	character*30 fname
	character*80 filename
	byte afname(30), afilename(80)
	equivalence (afname, fname), (afilename,filename)
	real*4 alt(75)
	integer*2 syear,smo,sday,shr,smin,ssec,emo,eday,ehr,emin,esec
        integer*4 fitropen, fit_close
	integer*4 stat
	integer*4 ifileptr, find_fit_rec
	integer*2 i,k,j,mgflag,ierr
	real*4 range,frange,freq
	real*4 plim,height
c
c
	type *,'enter file name with no extension'
	accept 1,fname
1	format(a)

	ifileptr = fitropen(afname)
	if (ifileptr .le. 0) then
	  type *,'Could not open the file'
	  type *,'file pointer = ',ifileptr
	  stop
	endif

	type *,'enter year'
	accept *,syear
	type *,'enter start time (mo,day,hr,min,sec)'
	accept *,smo,sday,shr,smin,ssec
	type *,'enter end time'
	accept *,emo,eday,ehr,emin,esec
	stime = cnv_mdhms_sec(syear,smo,sday,shr,smin,ssec)
	etime = cnv_mdhms_sec(syear,emo,eday,ehr,emin,esec)
	if (stime .lt. 0 .or. etime .lt. 0) then
	   type *,'Invalid start or end time'
	   stop
	endif

	type *,'enter power limit to print (dB)'
	accept *,plim
	type *,'enter height'
	accept *,height
	type *,'enter 0 for geographic position or 1 for magnetic'
	accept *,mgflag
c
	if (outfile(1:2) .ne. '  ') then
	   nlen = len(outfile)
	   do while ((outfile(i:i) .eq. ' ') .and. (i .gt. 1)) 
	      nlen = nlen - 1
	   enddo
	   open(unit=6, file=outfile(1:nlen))
	end if

        stat = find_fit_rec(%val(ifileptr), %val(stime), time_found)

	if (stat .eq. 0 .or. (abs(mod(stat,2)) .eq. 1)) then
	   type *,'stime,time_found,etime',stime,time_found,etime
	else
	   type *,'find_fit_rec returned error condition',stat,abs(mod(stat,2))
	   stop
	endif

	if (mgflag.eq.0) then
	  write(6,105)
105	  format('          geograph')
	else if (mgflag.eq.1) then
	  write(6,106)
106	  format('          magnetic')
	else
	  type *,'invalid geographic/magnetic flag.'
	  stop 'error exit'
	end if
	write(6,101)
101	format(' bm rng   lat  long    pwr0    pwr_l    pwr_s  ',
     $	' vel.   v_err    width_l  lags  x_pwr_l  x_vel  x_wid_l'
     %  ' el_low elev el_high   alt')
	iostat = 1
	do while ((iostat.eq.1) .and.(rec_time.lt.stime))
          iostat = read_datrec(ifileptr)
	end do
	if (iostat .ne. 1) type *,'iostat=',iostat
	do while ((iostat.eq.1) .and. (rec_time.le.etime))
	  frange = 0.15*lagfr
	  freq = fmhz + fkhz*.001
	  write(6,103)year,month,day,hour,minut,sec,frange,freq,NOISE,noise_lev
103	  format('0time = ',i4,'/',i2,'/',i2,4x,2i2.2,':',i2.2,4x,
     %    'first range = ',f5.0,4x,
     %		'freq=',f4.1,4x,'NOISE=',i8,4x,'nlev=',g12.5)
	  do i=1,NRANG
c	     print*,bmnum,i,qflag(i),pwr_l(i),vel(i),gscat(i)
	    if (((qflag(i)) .and.(pwr_l(i).ge.plim)).or.(plim.le.-50.0))then
	        range = 0.15*(lagfr + smsep*(i-1))
		if (gscat(i).and.(plim.gt.-50))range = range/2
		if ((xcf.ne.0) .and. x_qflag(i))then
		  alt(i) = sqrt(Re**2 + range**2 +2*range*Re*
     #                     sind(elev(i))) - Re
		else
		  alt(i) = height
 		end if
c
c	temporary change to fix alt = height and print phi0
c
                if (st_id .gt. 13) print *,"invalid station ID = ",st_id
		alt(i) = height
		call rbpos(i,bmnum,lagfr,smsep,alt(i),pos,parms386.st_id,
	1	     mgflag,ierr)
		lat=0
		long=0
		do k=1,2
		  do j=1,2
		    lat = lat + pos(1,k,j)
		    long = long + pos(2,k,j)
		  end do
		end do
		lat = lat/4
		long = long/4
		if (gscat(i))then
		  gf = 'G'
		else
		  gf = ' '
		end if
		if ((xcf.eq.0) .OR. (.not. x_qflag(i)))then
		  write(6,100)bmnum,i,lat,long,pwr_lag0(i),
     #                  pwr_l(i),pwr_s(i),
     $			vel(i),vel_err(i),width_l(i),gf,num_lags(i)
100	 	  format(i3,i3,2x,2(f6.1),2x,3f7.1,2x,2f8.0,3x,f7.0,1x,a1,i3)
		else
		  write(6,102)bmnum,i,lat,long,pwr_lag0(i),
     #                  pwr_l(i),pwr_s(i),
     $			vel(i),vel_err(i),width_l(i),gf,num_lags(i),x_pwr_l(i),
     #                  x_vel(i),x_width_l(i),
     %			elev_low(i),elev(i),elev_high(i),phi0(i)
102	 	  format(i3,i3,2x,2(f6.1),2x,3f7.1,2x,2f8.0,3x,f7.0,1x,a1,i3,
     $			1x,2f7.1,1x,f7.0,3x,3f6.1,1x,f7.2)
		end if
	    end if
	  end do
          iostat = read_datrec(ifileptr)
	end do
999	iostat = fit_close(%val(ifileptr))
        stop 'normal exit'
	end
