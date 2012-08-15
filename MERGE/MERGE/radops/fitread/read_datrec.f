	integer*4 function read_datrec(fitptr)
c
c	This function is used to read a logical record from a .FIT file
c	and fill in the data arrays.  It also reads the .INX file record
c	corresponding to the .FIT record.  You must use the function
c	FIND_REC before using this routine, in order to initialize the
c	record pointers.
c
c	The data are read into the common block /PROCDAT/
c	the input to read_datrec is fitptr
c	
c	The returned values of the function are:
c	value	meaning
c	---------------------------------------
c	1	successful read
c	-2	invalid record.  expecting parameter record
c	-4	time in data record inconsistent with time in index record
c	-8	range in data record inconsistent with slist 
c	-10	invalid record.  expecting ACF data record
c	-12	time in ACF data record inconsistent with index record
c	-34	invalid record.  expecting XCF data record
c	-36	time in XCF data record inconsistent with index record
c
c
c $Log:	read_datrec.f,v $
c Revision 1.3  94/09/06  10:41:28  10:41:28  baker (Kile Baker S1G)
c removed paths in include statements.
c 
c Revision 1.2  93/11/24  19:34:23  19:34:23  baker (Kile Baker S1G)
c added the padding to make the internal data structure consistent with
c the structure defined by the C routines.
c 
c Revision 1.1  1993/08/23  14:07:14  baker
c Initial revision
c
c
c $Revision: 1.3 $
c
	implicit none
	include 'radops.par'
	include 'raddat.inc'
	equivalence (parms, parm_list)
	integer*4 i, fitptr, read_fit, iostat

	structure /FIT_DATA/
	  integer*4 rec_time
	  record /radops_parms/ p
	  integer*2 pulse_pattern(16)
	  integer*2 lag_table(48, 2)
	  logical*1 combf(80)
	  integer*4 PAD1
	  real*8 noise_lev
	  real*8 noise_lag0
	  real*8 noise_vel
	  real*8 pwr_lag0(75)
	  integer*2 slist(75)
	  integer*2 nsel
	  integer*4 qflag(75)
	  integer*4 PAD2
	  real*8 pwr_l(75)
	  real*8 pwr_l_err(75)
	  real*8 pwr_s(75)
	  real*8 pwr_s_err(75)
	  real*8 vel(75)
	  real*8 vel_err(75)
	  real*8 width_l(75)
	  real*8 width_l_err(75)
	  real*8 width_s(75)
	  real*8 width_s_err(75)
	  real*8 stnd_dev_l(75)
	  real*8 stnd_dev_s(75)
	  real*8 stnd_dev_phi(75)
	  integer*2 gscat(75)
	  integer*2 PAD3
	  integer*4 x_qflag(75)
	  integer*4 PAD4
	  real*8 x_pwr_l(75)
	  real*8 x_pwr_l_err(75)
	  real*8 x_pwr_s(75)
	  real*8 x_pwr_s_err(75)
	  real*8 x_vel(75)
	  real*8 x_vel_err(75)
	  real*8 x_width_l(75)
	  real*8 x_width_l_err(75)
	  real*8 x_width_s(75)
	  real*8 x_width_s_err(75)
	  real*8 phi0(75)
	  real*8 phi0_err(75)
	  real*8 elev(75)
	  real*8 elev_low(75)
	  real*8 elev_high(75)
	  real*8 x_stnd_dev_l(75)
	  real*8 x_stnd_dev_s(75)
	  real*8 x_stnd_dev_phi(75)
	  integer*2 num_lags(75)

	end structure

	record /FIT_DATA/ fd
	
c
c	All this function actually has to do is call read_fit, and then
c	parcel out the data returned in the structure to the common
c	block the fortran programs are expecting
c
	iostat = read_fit(%val(fitptr), fd)
	
	rec_time = fd.rec_time
	parms386.nparm = fd.p.nparm
	parms386.st_id = fd.p.st_id
	parms386.year = fd.p.year
	parms386.month = fd.p.month
	parms386.day = fd.p.day
	parms386.hour = fd.p.hour
	parms386.minut = fd.p.minut
	parms386.sec = fd.p.sec
	parms386.nave = fd.p.nave
	parms386.atten = fd.p.atten
	parms386.lagfr = fd.p.lagfr
c !!! Temporary fix to avoid range squashing due to Kap phase coding DA 13-JUN-1997
c		was:   = fd.p.smsep
	parms386.smsep = fd.p.rsep* (20.0/ 3.0)
	parms386.ercod = fd.p.ercod
	parms386.noise = fd.p.noise
	parms386.intt = fd.p.intt
	parms386.txpl = fd.p.txpl
	parms386.mpinc = fd.p.mpinc
	parms386.mppul = fd.p.mppul
	parms386.nrang = fd.p.nrang
	parms386.bmnum = fd.p.bmnum
	parms386.xcf = fd.p.xcf
	parms386.tfreq = fd.p.tfreq
	parms386.mxpwr = fd.p.mxpwr
	parms386.lvmax = fd.p.lvmax
	parms386.rsep = fd.p.rsep
	parms386.frang = fd.p.frang
	parms386.cp = fd.p.cp
	parms386.scan = fd.p.scan

c
	noise_lev = fd.noise_lev
	noise_lag0 = fd.noise_lag0
	noise_vel = fd.noise_vel

	do i=1,75
		pwr_lag0(i) = fd.pwr_lag0(i)
		slist(i) = fd.slist(i)
	enddo

	do i=1,75
		num_lags(i) = fd.num_lags(i)
	enddo

	do i=1,75
		qflag(i) = fd.qflag(i)
		x_qflag(i) = fd.x_qflag(i)
	enddo

	do i=1,75
		pwr_l(i) = fd.pwr_l(i)
		pwr_l_err(i) = fd.pwr_l_err(i)
		pwr_s(i) = fd.pwr_s(i)
		pwr_s_err(i) = fd.pwr_s_err(i)
		vel(i) = fd.vel(i)
		vel_err(i) = fd.vel_err(i)
		width_l(i) = fd.width_l(i)
		width_l_err(i) = fd.width_l_err(i)
		width_s(i) = fd.width_s(i)
		width_s_err(i) = fd.width_s_err(i)
		stnd_dev_l(i) = fd.stnd_dev_l(i)
		stnd_dev_s(i) = fd.stnd_dev_s(i)
		stnd_dev_phi(i) = fd.stnd_dev_phi(i)
		
		x_pwr_l(i) = fd.x_pwr_l(i)
		x_pwr_l_err(i) = fd.x_pwr_l_err(i)
	        x_pwr_s(i) = fd.x_pwr_s(i)
		x_pwr_s_err(i) = fd.x_pwr_s_err(i)
		x_vel(i) = fd.x_vel(i)
		x_vel_err(i) = fd.x_vel_err(i)
		x_width_l(i) = fd.x_width_l(i)
		x_width_l_err(i) = fd.x_width_l_err(i)
		x_width_s(i) = fd.x_width_s(i)
		x_width_s_err(i) = fd.x_width_s_err(i)
		x_stnd_dev_l(i) = fd.x_stnd_dev_l(i)
		x_stnd_dev_s(i) = fd.x_stnd_dev_s(i)
		x_stnd_dev_phi(i) = fd.x_stnd_dev_phi(i)
		phi0(i) = fd.phi0(i)
		phi0_err(i) = fd.phi0_err(i)
		elev(i) = fd.elev(i)
		elev_low(i) = fd.elev_low(i)
		elev_high(i) = fd.elev_high(i)

		gscat(i) = fd.gscat(i)
	enddo

c
c	finally, move the parmlist structure to the old parm array
c
	nparm = parms386.nparm
	st_id = parms386.st_id
	year = parms386.year
	month = parms386.month
	day = parms386.day
	hour = parms386.hour
	minut = parms386.minut
	sec = parms386.sec
	nave = parms386.nave
	atten = parms386.atten
	lagfr = parms386.lagfr
	smsep = parms386.smsep
	ercod = parms386.ercod
	noise = parms386.noise
	intt = parms386.intt
	txpl = parms386.txpl
	mpinc = parms386.mpinc
	mppul = parms386.mppul
	nrang = parms386.nrang
	bmnum = parms386.bmnum
	xcf = parms386.xcf
	fmhz = parms386.tfreq/1000
C	fkhz = mod( parms386.tfreq, 1000)	! f90; 20000909; DA
	fkhz = mod( INT( parms386.tfreq), 1000)
	if (parms386.mxpwr .gt. 32767) 	then
		mxpwr = 32767
	else
		mxpwr = parms386.mxpwr
	endif
	if (parms386.lvmax .gt. 32767) then
		lvmax = 32767
	else
		lvmax = parms386.lvmax
	endif

	dparm = 0
	do i=1,80
	  combyts(i) = fd.combf(i)
	enddo
	if (iostat .eq. 0) then 
		read_datrec = 1		!fortran returns a 1 instead of 0
	else
		read_datrec = iostat
	endif
	return
	end

