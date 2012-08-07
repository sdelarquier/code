c	RADDAT386.INC
c
c	This include file defines the structure of the FIT data files
c	and the resulting values that are created by READ_DATREC.
c	This version is used with data files created by RADOPS386
c	The version for use with the old Data General FIT files is
c	RADDAT.INC
c
c	Note that this file requires the structure /radops_parms/ which
c	is defined in the include file, "RADOPS.PAR".  Therefore, any
c	program which uses this include file must also have the line
c	include 'radops.par' BEFORE this file is included.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	
c	The following parameters define the size of the pulse table and the
c	lag table.  In version 1.01 of FITACF these tables were set to 7
c	and 17.  In version 1.03 they are set to 16 and 48.
c
	INTEGER*2 PULSE_PAT_LEN, LAG_TAB_LEN
	parameter (PULSE_PAT_LEN = 7, LAG_TAB_LEN = 17)
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
	integer*2 slist(75),parm_list(70)
	integer*4 rec_time
	record /radops_parms/parms386
c
c	This section defines the data structure for a FIT file data record
c	There are two types of records.  Relative Record 0 contains the
c	radar parameters, the lag0 powers, and the range selection list.
c	All other records contain the fitted values for up to 25 ranges.
c
	structure /fitrec/
	  union
	    map
		byte frcrd0(1024)	!the record can be treated as a byte stream
	    end map
	    map				!relative record 0
		integer*4 rrn0, r_time0
		record /radops_parms/ r_parm
		integer*2 r_ppat(PULSE_PAT_LEN), r_lagtable(LAG_TAB_LEN,2)
		character*80 r_combf
		integer*4 r_noise, r_noise_lag0, r_noise_vel
		integer*2 r_pwr_0(75),r_slist(75)
	    end map
	    map				!regular data records
		integer*4 rrn, r_time, r_xflag
		byte r_range_list(25),r_qflag(25)
		integer*2 r_pwr_l(25),r_pwr_l_err(25),r_pwr_s(25),r_pwr_s_err(25),
     %		r_vel(25),r_vel_err(25),r_w_l(25),r_w_l_err(25),
     $		r_w_s(25),r_w_s_err(25),r_phi0(25),r_phi0_err(25),
     &		r_elev(25),r_el_low(25),r_el_high(25),r_sdev_l(25),
     #		r_sdev_s(25),r_sdev_phi(25),r_gscat(25)

	    end map
	  end union
	end structure
	
	record /fitrec/ fitdata

c
c	The values for the ACF and the XCF data as well as lag0 power, etc
c	are all placed into the common block /procdat/
c
c	name		type		comment
c_____________________________________________________________________
c	rec_time	I4	time in seconds (from cnv$mdhms_sec)
c	parms386	struct	parameter list
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
c	NOTE:  for RADOPS386, only "good" values of qflag will occur since
c	failures are not saved on the optical disk
c
	common /procdat/rec_time,parm_list,noise_lev,noise_lag0,noise_vel,
     @	  pwr_lag0,slist,qflag,pwr_l,pwr_l_err,pwr_s,pwr_s_err,vel,vel_err,
     #	  width_l,width_l_err,width_s,width_s_err,stnd_dev_l,stnd_dev_s,
     $	  stnd_dev_phi,gscat,x_qflag,x_pwr_l,x_pwr_l_err,x_pwr_s,x_pwr_s_err,
     %	  x_vel,x_vel_err,x_width_l,x_width_l_err,x_width_s,x_width_s_err,
     ^	  phi0,phi0_err,elev,elev_low,elev_high,x_stnd_dev_l,x_stnd_dev_s,
     @	  x_stnd_dev_phi,parms386
c
c	Note that the radops386 structure /radops_parms/ is used by the
c	variable, 'parms386', which is also saved in the common block.
c

