c
c	the following arrays are for the values returned from fitting
c	the ACFs
c
	integer*2 gscat(75)
	integer*4 qflag(75)
	real*4 pwr_l(75),pwr_l_err(75),pwr_s(75),pwr_s_err(75),
     $		omega(75),omega_err(75),lamda(75),lamda_err(75),
     %		sigma(75),sigma_err(75),
     &		stnd_dev_l(75),stnd_dev_s(75),stnd_dev_phi(75)
	real*4 vel(75),vel_err(75),width_l(75),width_l_err(75),
     %		width_s(75),width_s_err(75),elev(75),elev_low(75),elev_high(75)
c
c	the values prefixed with x_ are the values returned or calculated
c	from the fit to the XCFs
c
	integer*4 x_qflag(75)
	real*4 x_pwr_l(75),x_pwr_l_err(75),x_pwr_s(75),x_pwr_s_err(75),
     $		x_omega(75),x_omega_err(75),x_lamda(75),x_lamda_err(75),
     %		x_sigma(75),x_sigma_err(75),phi0(75),phi0_err(75),
     &		x_stnd_dev_l(75),x_stnd_dev_s(75),x_stnd_dev_phi(75)
	real*4 x_vel(75),x_vel_err(75),x_width_l(75),x_width_l_err(75),
     %		x_width_s(75),x_width_s_err(75)
	real*4 pwr_lag0(75),noise_lev,noise_lag0,noise_vel
	integer*2 slist(75),num_lags(75),parm_list(70)
	integer*4 rec_time
	record /radops_parms/parms386
c
c	the values prefixed by r_ are equated to the data record which will
c	be outputted to the disk file
c
	integer*4 r_noise,r_noise_lag0,r_noise_vel
	integer*2 r_pwr_0(50),r_slist(50)
	integer*2 r_parm(70)
	byte r_range_list(25),r_qflag(25)
	integer*2 r_pwr_l(25),r_pwr_l_err(25),r_pwr_s(25),r_pwr_s_err(25),
     %		r_vel(25),r_vel_err(25),r_w_l(25),r_w_l_err(25),
     $		r_w_s(25),r_w_s_err(25),r_phi0(25),r_phi0_err(25),
     &		r_elev(25),r_el_low(25),r_el_high(25),r_sdev_l(25),
     #		r_sdev_s(25),r_sdev_phi(25),r_gscat(25)
	integer*4 rrn,r_time,r_xflag
c
c	the actual data record is RECORD
c
	byte record(1024)
c
	equivalence (r_noise,record(149)),(r_noise_lag0,record(153)),
     %		(r_noise_vel,record(157)),(r_pwr_0,record(161)),
     &		(r_slist,record(261))
	equivalence (r_parm,record(9))
	equivalence  (rrn,record(1)),(r_time,record(5)),(r_xflag,record(9)),
     $	  (r_range_list,record(13)),(r_qflag,record(38)),
     &    (r_pwr_l,record(63)), (r_pwr_l_err,record(113)),
     *    (r_pwr_s,record(163)),(r_pwr_s_err,record(213)),
     #	  (r_vel, record(263)), (r_vel_err,record(313)),
     @	  (r_w_l, record(363)), (r_w_l_err,record(413)),
     ^	  (r_w_s, record(463)), (r_w_s_err,record(513)),
     +	  (r_phi0,record(563)), (r_phi0_err,record(613)),
     $	  (r_elev,record(663)), (r_el_low,record(713)),(r_el_high,record(763)),
     #	  (r_sdev_l,record(813)),(r_sdev_s,record(863)),(r_sdev_phi,record(913)),
     @	  (r_gscat,record(963))
c
c	The values for the ACF and the XCF data as well as lag0 power, etc
c	are all placed into the common block /procdat/
c
c	name		type		comment
c_____________________________________________________________________
c	rec_time	I4	time in seconds (from cnv$mdhms_sec)
c	parm_list(70)	I2	RADOPS parameter list
c	noise_lev	R4	in A-to-D converter units
c	noise_lag0	R4	the noise subtracted from lag0 pwr
c	noise_vel	R4	velocity found from the noise ACF
c	pwr_lag0(75)	R4 	lag-0 power in dB
c	slist(75)	I2	range selection list
c	qflag(75)	I4	ACF quality flag (see below)
c	pwr_l(75)	R4	fitted power from lamda fit in dB
c	pwr_l_err(75)	R4	error in fitted power from lamda fit
c	pwr_s(75)	R4	fitted power from sigma fit in dB
c	pwr_s_err(75)	R4	error in fitted power from sigma fit
c	vel(75)		R4	velocity in m/s
c	vel_err(75)	R4	error in velocity
c	width_l(75)	R4	spectral width from lamda fit (m/s)
c	width_l_err(75)	R4	error in spectral width (lamda)
c	width_s(75)	R4	spectral width from sigma fit (m/s)
c	width_s_err(75)	R4	error in spectral width (sigma)
c	stnd_dev_l(75)	R4	Standard deviation of points from the lamda fit
c	stnd_dev_s(75)	R4	Standard deviation of points from the sigma fit
c	stnd_dev_phi(75)R4	Standard deviation of points from the phase fit
c	gscat(75)	I2	logical*2: true if ACF is ground scatter
c	x_qflag(75)	I4	quality flag for XCF fit
c	x_pwr_l(75)	R4	fitted power of XCF (lamda fit) in dB
c	x_pwr_l_err(75)	R4	error in x_pwr_l
c	x_pwr_s(75)	R4	fitted power of XCF (sigma fit) in dB
c	x_pwr_s_err(75)	R4	error in x_pwr_s
c	x_vel(75)	R4	velocity from XCF fit (m/s)
c	x_vel_err(75)	R4	error in x_vel
c	x_width_l(75)	R4	width of XCF spectrum (lamda fit) (m/s)
c	x_width_l_err(75) R4	error in x_width_l
c	x_width_s(75)	R4	width of XCF spectrum (sigma fit) (m/s)
c	x_width_s_err(75) R4	error in x_width_s
c	phi0(75)	R4	XCF phase at lag0 (-180 to +180 degrees)
c	phi0_err(75)	R4	error in phi0
c	elev(75)	R4	elevation angle corresponding to phi0 (degrees)
c	elev_low(75)	R4	elevation angle low limit (from phi0_err)
c	elev_high(75)	R4	elevation angle high limit
c
c	The quality flags (qflag and x_qflag) have the following definitions:
c	qflag value	meaning
c	-------------------------
c	0		no ACF or XCF done for this range
c	1		good ACF (or XCF)
c	2		bad ACF, lag0 power < noise level 
c	4		bac ACF, insufficient number of good lags for a fit
c	8		bad ACF, bad data results in division by 0
c	16		bad ACF, oscillating value of omega
c	32		bad ACF, too many iterations to get omega
c
c	NOTE:  in a logical IF test in FORTRAN on a VAX, only the "good ACF"
c	will test TRUE.  all the other values of the quality flag will test
c	FALSE.
c
	common /procdat/rec_time,parm_list,noise_lev,noise_lag0,noise_vel,
     @	  pwr_lag0,slist,qflag,pwr_l,pwr_l_err,pwr_s,pwr_s_err,vel,vel_err,
     #	  width_l,width_l_err,width_s,width_s_err,stnd_dev_l,stnd_dev_s,
     $	  stnd_dev_phi,gscat,x_qflag,x_pwr_l,x_pwr_l_err,x_pwr_s,x_pwr_s_err,
     %	  x_vel,x_vel_err,x_width_l,x_width_l_err,x_width_s,x_width_s_err,
     ^	  phi0,phi0_err,elev,elev_low,elev_high,x_stnd_dev_l,x_stnd_dev_s,
     @	  x_stnd_dev_phi,parms386,num_lags
c
c
c	The following arrays are used to initialize the pulse pattern and lag
c	tables.  This is done to insure compatibility between the Data General
c	FIT files and the RADOPS386 FIT files.
c
	integer*2 dg_pulse_pat(16)/0,2,3,7,13,15,16,9*0/
	integer*2 dg_lags(48,2)/7,2,0,0,3,2,7,0,7,7,3,2,3,0,2,0,0,31*0,
     &		7,3,2,3,7,7,13,7,15,16,13,13,15,13,16,15,16,31*0/
c
c
