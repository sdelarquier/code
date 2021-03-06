;+
; NAME:
; RT_RUN
;
; PURPOSE:
; This procedure runs raytracing code or read existing raytracing results into
; the common block RT_DATA_BLK. The time range for which data is read is controled by
; the DATE and TIME keywords. Adapted for MPI code.
;
; CATEGORY:
; Input/Output
;
; CALLING SEQUENCE:
; RT_RUN
;
; INPUTS:
; DATE: the date for which you want to run the raytracing (for IRI model)
; Format is YYYYMMDD
;
; RADAR: the radar code for which you want to run the raytracing. Use 'custom' to provide your own radar position and azimuth (using GEOPOS and AZIM)
;
; KEYWORD PARAMETERS:
; BEAM: the beam for which you want to run the raytracing (default is all)
;
; TIME: time interval for which you want to run the raytracing, default is [0000,2400]
; If TIME[1] is greater or equal to TIME[0] then TIME[1] is taken to be on the next day.
;
; FREQ: operating frequency in MHz. default is 11MHz
;
; ELEVSTP: elevation angle step size in degrees
;
; DHOUR: specify time reoslution in hour (default is 0.5 hour)
;
; NHOP: specify the number of hop considered in the ray-tracing. Default is 1.
;
; GEOPOS: [latitude, longitude]. Overrides the radar command.
;
; AZIM: To be specified when using the geopos keyword
;
; BIN: set this keyword to the desired parameter for range gate binning (phase, group of ground range).
; Default is group, same as for radars.
;
; LTIME: set this keyword to run in LT instead of UT
;
; OUTDIR: directory where ray-tracing files will be outputed: *.dat contain radar binned results, *.rays contain raw ray paths
;
; NMF2: Electron density at the peak of the f2 layer in log scale [log10(m-3)]. The default value is computed by IRI.
;
; HMF2: Height of the f2 layer peak [km]. The default value is computed by IRI.
;
; BACK
;
; SILENT
;
; NPROCS: Number of processors to be used in computation (default is 4)
;
; COMMON BLOCKS:
; RT_DATA_BLK: The common block holding the currently loaded raytracing data and
; information about that data.
;
; EXAMPLE:
;	; Run the raytracing for august 2 2010 for Blackstone
;	; from 17 to 24 LT
;	rt_run, 20100802, 'bks', time=[1700,2400]
;	; Plot results on range-time plot for the first 60 gates
;	rt_plot_rti, param=['power','elevation']
;
; COPYRIGHT:
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.
;
; MODIFICATION HISTORY:
; Based on Lasse Clausen, RAD_FIT_READ
; Based on Nitya Ravindran, RT
; Written by Sebastien de Larquier, Sept. 2010
; Last modified 05-2012
;-
pro 	RT_RUN, date, radar, dhour=dhour, ltime=ltime, $
	time=time, beam=beam, nhop=nhop, azim=azim, nmf2=nmf2, hmf2=hmf2, $
	freq=freq, elev=elev, geopos=geopos, no_weights=no_weights, $
	back=back, bin=bin, nprocs=nprocs, outdir=outdir, $
	silent=silent,  force=force, no_ionos=no_ionos, no_rays=no_rays

common	rt_data_blk
common	radarinfo


; Save time at begining of run
script_time = systime(/julian, /utc)

; Choose day for IRI model
IF n_elements(date) gt 1 THEN BEGIN
	prinfo, 'date does not accept more than one element (for now)'
	return
ENDIF

if ~keyword_set(freq) then $
	freq = 11.

if ~keyword_set(bin) then $
	bin = 'group'

if ~keyword_set(nprocs) then $
	nprocs = 4

if ~keyword_set(elev) then $
	elev = [5., 55., .1]

; Format time parameters
if ~keyword_set(dhour) then $
	dhour = .5
if ~keyword_set(time) then $
	time = [0000,2400]
parse_date, date[0], year, month, day
parse_time, time, shour ,sminutes, fhour, fminutes
hour = [shour + sminutes/60., fhour + fminutes/60., dhour]
if n_elements(time) eq 1 then $
	hour = [shour + sminutes/60., shour + 1., dhour]
sjul = julday(month, day, year, shour, sminutes)
fjul = julday(month, day, year, fhour, fminutes)
if hour[0] ge hour[1] then begin
	fjul = fjul + 1.d
	caldat, fjul, month, day, year
	date = [date, calc_date(date, 1)]
endif
if n_elements(time) eq 1 then $
	hour[1] = hour[0] + .5
if ~keyword_set(ltime) then begin
	hour[0] = hour[0] + 25.
	hour[1] = hour[1] + 25.
	tz = 'UT'
endif else $
	tz = 'LT'

; Read radar location and boresight
if radar eq 'custom' then begin
	if ~keyword_set(azim) then begin
		prinfo, 'No azimuth specified. Setting it to 0.'
		azim = 0.
	endif
	rad_set_customradar, geopos[0], geopos[1], azim
	radarsite = network[0].site[0]
	
        if n_elements(beam) lt 1 then $
		beam = indgen(radarsite.maxbeam)
	if max(beam) ge radarsite.maxbeam then $
		return

	; Calculate azimuth limits and step
	azim = [rt_get_azim(radar, beam[0], date[0]), $
	        	rt_get_azim(radar, beam[n_elements(beam)-1], date[0]), $
						radarsite.bmsep]
endif else begin
	radID = where(network.code[0,*] eq radar)
	tval = TimeYMDHMSToEpoch(year, month, day, shour, sminutes, 0)
	for s=0,31 do begin
		if (network[radID].site[s].tval eq -1) then break
		if (network[radID].site[s].tval ge tval) then break
	endfor
	radarsite = network[radID].site[s]

	if n_elements(beam) lt 1 then $
		beam = indgen(radarsite.maxbeam)
	if max(beam) ge radarsite.maxbeam then $
		return

	; Calculate azimuth limits and step
	if ~keyword_set(azim) then begin
		azim = [rt_get_azim(radar, beam[0], date[0]), $
				rt_get_azim(radar, beam[n_elements(beam)-1], date[0]), $
				radarsite.bmsep]
	endif
endelse

; Re-format azimuth input for input file
if n_elements(azim) ne 3 then begin
	azim = [azim, azim, 1.]
endif

if ~keyword_set(nhop) then $
	nhop = 1L

; Get path to davit
;davit_lib = getenv("DAVIT_LIB")
davit_lib = '~/Documents/code/rtmpi'

; Start MPD
spawn, 'mpdtrace', mpdout
if n_elements(mpdout) gt 1 then begin
	spawn, 'mpd --daemon', mpdout
	print, mpdout
endif

; Some useful parameters
ngates 		= 70
range_gate 	= 180L + 45L*lindgen(ngates+1)
Re 			= 6370.
P 			= 180. + findgen(ngates)*45.
minpower 	= 4.

;******************************************************************************************************
; Some file management and testing
;******************************************************************************************************
; Check if files exist
if keyword_set(outdir) then begin
	if strmid(outdir,strlen(outdir)-1,1) ne '/' then $
		outdir = outdir+'/'
endif

rtFileTest, date, time, radar, beam=beam, freq=freq, dhour=dhour, nhop=nhop, back=back, ground=no_ionos, code=code, outdir=outdir
if ~keyword_set(no_rays) then $
	rtFileTest, date, time, radar, beam=beam, freq=freq, dhour=dhour, nhop=nhop, back=back, /rays, code=rcode, outdir=outdir $
else $
	rcode = 1

; Get directory where to store output files
if ~keyword_set(outdir) then begin
	dirname = '/sd-data/rt/'
	if ~file_test(dirname+'/'+strtrim(year,2), /dir) then begin
		FILE_MKDIR, dirname+'/'+strtrim(year,2)
	endif
	if ~file_test(dirname+'/'+strtrim(year,2)+'/'+radar, /dir) then begin
		FILE_MKDIR, dirname+'/'+strtrim(year,2)+'/'+radar
	endif
endif else $
	dirname = outdir

if keyword_set(nmf2) or keyword_set(hmf2) then begin
	outdir = '/tmp/'
	dirname = outdir
endif

;**************************************************************************************
; Get to the ray-tracing part of this all thing
;**************************************************************************************
; Run raytracing if no data already exist
IF ~code OR ~rcode OR keyword_set(force) THEN BEGIN

	; Remove fortran output
	spawn, 'rm -f /tmp/edens.dat'
	spawn, 'rm -f /tmp/ranges.dat'
	spawn, 'rm -f /tmp/ionos.dat'
	spawn, 'rm -f /tmp/rays.dat'

	; Generate input file for fortran code
	rt_write_input, radarsite.geolat, radarsite.geolon, azim, date[0], hour, $
			freq=freq, elev=elev, nhop=nhop, nmf2=nmf2, hmf2=hmf2, $
			outdir=inputdir, filename=filename, silent=silent

	spawn, 'echo "'+inputdir+filename+'" > /tmp/rt_inp_file'
	spawn, 'mpiexec -n '+strtrim(nprocs, 2)+' raydarn < /tmp/rt_inp_file'

	; Read data from files to be saved in common block
; 	time_mark = systime(/julian, /utc)
	rt_read_header, txlat, txlon, azim_beg, azim_end, azim_stp, $
			elev_beg, elev_end, elev_stp, freq, $
			year, mmdd, hour_beg, hour_end, hour_stp, $
			nhour, nazim, nelev, dir='/tmp'
; 	print, 'Time reading header files (s): ',(systime(/julian, /utc) - time_mark)*86400.d

	; Ground scatter
; 	time_mark = systime(/julian, /utc)
	rt_read_ranges, rantheta, grpran, ranhour, ranazim, ranelv, ranalt, ran, grndran, $
		lat=ranlat, lon=ranlon, dir='/tmp', silent=silent
; 	print, 'Time reading ranges files (s): ',(systime(/julian, /utc) - time_mark)*86400.d

	; Ionospheric scatter
; 	time_mark = systime(/julian, /utc)
	if ~keyword_set(no_ionos) then begin
		rt_read_ionos, normtheta, normgrpran, normhour, normazim, normelv, normalt, normran, normgnd, normweights, normnrefract, $
			lat=ionolat, lon=ionolon, aspect=aspect, dir='/tmp', silent=silent
	endif
; 	print, 'Time reading ionos files (s): ',(systime(/julian, /utc) - time_mark)*86400.d

	; Electron densities
; 	time_mark = systime(/julian, /utc)
	rt_read_edens, edens, dip=dip, dir='/tmp'
; 	print, 'Time reading edens files (s): ',(systime(/julian, /utc) - time_mark)*86400.d

	if ~keyword_set(silent) then $
		print, 'Time reading output files (s): ',(systime(/julian, /utc) - script_time)*86400.d

	; Initialize structure
	rtStructInit, nhour, nazim, ngates+1, rt_data, rt_info

	; Populate data structure
	rt_data.edens = edens[*,*,0:*:10,0:*:2]
	rt_data.dip = dip[*,*,*,0:*:2]

	;***************************************************************************
	; Cycle through time
	parse_date, date[0], year, month, day
	FOR hr=0,nhour-1 do begin

		; times in julian dates
		thour = hour_beg + hr*hour_stp
		if ~keyword_set(ltime) then $
			thour = thour - 25.
		if thour ge 24. then begin
			tdate = calc_date(date[0], 1)
			parse_date, tdate, year, month, day
			thour = thour - 24.
		endif
		rt_data.juls[hr] = JULDAY(month,day,year,floor(thour),(thour-floor(thour))*60.)

		; operating frequency
		rt_data.tfreq[hr] = freq

		;***********************************************************
		; Cycle through beams
		FOR b=0,nazim-1 do begin
			; Define how to bin the data
			case bin of
				'phase':	range_sort = ran[hr,b,*]
				'group':	range_sort = grpran[hr,b,*]
				'ground':	range_sort = grndran[hr,b,*]
			endcase
			if ~keyword_set(no_ionos) then begin
				case bin of
					'phase':	ionosrange_sort = normran[hr,b,*]
					'group':	ionosrange_sort = normgrpran[hr,b,*]
					'ground':	ionosrange_sort = normgnd[hr,b,*]
				endcase
			endif


			;*******************************************
			; populate data structures

			; beam and corresponding azimuth
			rt_data.beam[hr,b] = beam[b]
			rt_data.azim[hr,b] = azim_beg + b*azim_stp

			; linear power(param%
			rt_data.lagpower[hr,b,0:ngates-1] = histc(range_sort*1e-3,range_gate, weights=1./grpran[hr,b,*]^3)

			; Ground scatter flag
			gscatterinds = where(rt_data.lagpower[hr,b,*] gt 0., cc)
			if cc gt 0 then $
				rt_data.gscatter[hr,b,gscatterinds] = 1b

			; ionospheric linear and log power and scatter flag
			if ~keyword_set(no_ionos) then begin
				if keyword_set(no_weights) then $
					ionoslagpower = histc(ionosrange_sort*1e-3,range_gate, weights=1./normran[hr,b,*]^3) $
				else $
					ionoslagpower = histc(ionosrange_sort*1e-3,range_gate, weights=normweights[hr,b,*])
				ionoslagpower2 = histc(ionosrange_sort*1e-3,range_gate, weights=normweights[hr,b,*]/max(normweights[hr,*,*]))
				ionosinds = where(ionoslagpower2 gt 1e-1,cc)
				if cc gt 0 then begin
					rt_data.lagpower[hr,b,ionosinds] = ionoslagpower[ionosinds]
					rt_data.gscatter[hr,b,ionosinds] = 2b
				endif
			endif

			for ng=0, ngates-1 do begin
				gateinds = where(range_sort*1e-3 LT range_gate[ng+1] AND range_sort*1e-3 GE range_gate[ng])
				; elevation angle for GS
				rt_data.elevation[hr,b,ng] = ( gateinds[0] ge 0 and rt_data.gscatter[hr,b,ng] eq 1b ? MEAN(ranelv[hr,b,gateinds]) : 10000.)
				; reflection altitude for GS
				rt_data.altitude[hr,b,ng] = ( gateinds[0] ge 0 and rt_data.gscatter[hr,b,ng] eq 1b  ? MEAN(ranalt[hr,b,gateinds]) : 10000.)
				; virtual height for GS
				tempvalt = sqrt(P[ng]^2./4. + Re^2. + 2.*Re*P[ng]/2.*sin(rt_data.elevation[hr,b,ng]*!dtor)) - Re
				rt_data.valtitude[hr,b,ng] = (rt_data.gscatter[hr,b,ng] eq 1b ? tempvalt : 10000.)
				; ground range for GS
				rt_data.grange[hr,b,ng] = ( gateinds[0] ge 0 and rt_data.gscatter[hr,b,ng] eq 1b  ? MEAN(grndran[hr,b,gateinds]) : 10000.)
				; latitude and longitude
				rt_data.latitude[hr,b,ng] = ( gateinds[0] ge 0 and rt_data.gscatter[hr,b,ng] eq 1b  ? MEAN(ranlat[hr,b,gateinds]) : 10000.)
				rt_data.longitude[hr,b,ng] = ( gateinds[0] ge 0 and rt_data.gscatter[hr,b,ng] eq 1b  ? MEAN(ranlon[hr,b,gateinds]) : 10000.)


				if ~keyword_set(no_ionos) then begin
					gateindsionos = where(ionosrange_sort*1e-3 LT range_gate[ng+1] AND ionosrange_sort*1e-3 GE range_gate[ng])
					; elevation angle for IS
					rt_data.elevation[hr,b,ng] = ( gateindsionos[0] ge 0 and rt_data.gscatter[hr,b,ng] eq 2b ? TOTAL(normelv[hr,b,gateindsionos]*normweights[hr,b,gateindsionos])/TOTAL(normweights[hr,b,gateindsionos]) : rt_data.elevation[hr,b,ng])
					; reflection altitude for IS
					rt_data.altitude[hr,b,ng] = ( gateindsionos[0] ge 0 and rt_data.gscatter[hr,b,ng] eq 2b ? TOTAL(normalt[hr,b,gateindsionos]*normweights[hr,b,gateindsionos])/TOTAL(normweights[hr,b,gateindsionos]) : rt_data.altitude[hr,b,ng])
					; virtual height for IS
					tempvalt = sqrt(P[ng]^2. + Re^2. + 2.*Re*P[ng]*sin(rt_data.elevation[hr,b,ng]*!dtor)) - Re
					rt_data.valtitude[hr,b,ng] = (rt_data.gscatter[hr,b,ng] eq 2b ? tempvalt : rt_data.valtitude[hr,b,ng])
					; refraction index for ionos scatter (at reflection point)
					rt_data.nr[hr,b,ng] = ( gateindsionos[0] ge 0 and rt_data.gscatter[hr,b,ng] eq 2b ? TOTAL(normnrefract[hr,b,gateindsionos]*normweights[hr,b,gateindsionos])/TOTAL(normweights[hr,b,gateindsionos]) : 10000.)
					; aspect angle (at reflection point)
					rt_data.aspect[hr,b,ng] = ( gateindsionos[0] ge 0 and rt_data.gscatter[hr,b,ng] eq 2b ? abs(90.-(MEDIAN(aspect[hr,b,gateindsionos])+MEANABSDEV(aspect[hr,b,gateindsionos]))) : 10000.)
					; ground range for IS
					rt_data.grange[hr,b,ng] = ( gateindsionos[0] ge 0 and rt_data.gscatter[hr,b,ng] eq 2b  ? TOTAL(normgnd[hr,b,gateindsionos]*normweights[hr,b,gateindsionos])/TOTAL(normweights[hr,b,gateindsionos]) : rt_data.grange[hr,b,ng])
					; latitude and longitude for IS
					rt_data.latitude[hr,b,ng] = ( gateindsionos[0] ge 0 and rt_data.gscatter[hr,b,ng] eq 2b  ? TOTAL(ionolat[hr,b,gateindsionos]*normweights[hr,b,gateindsionos])/TOTAL(normweights[hr,b,gateindsionos]) : rt_data.latitude[hr,b,ng])
					rt_data.longitude[hr,b,ng] = ( gateindsionos[0] ge 0 and rt_data.gscatter[hr,b,ng] eq 2b  ? TOTAL(ionolon[hr,b,gateindsionos]*normweights[hr,b,gateindsionos])/TOTAL(normweights[hr,b,gateindsionos]) : rt_data.longitude[hr,b,ng])
				endif
			endfor

			; End populate data structures
			;*******************************************

		ENDFOR
		; End beam loop
		;***********************************************************

	ENDFOR
	; End time loop
	;***************************************************************************

	; Populate info structure
	; start and end times
	rt_info.sjul = sjul
	rt_info.fjul = fjul

	; Radar code name
	rt_info.name = radar

	; radar ID
	if ~keyword_set(geopos) then $
		rt_info.id = network[where(network.code[0] eq radar)].ID $
	else $
		rt_info.id = 0

	; Time format (UT or LT)
	rt_info.timez = tz

	; Number of range gates
	rt_info.ngates = ngates

	; Radar position
	rt_info.glat = radarsite.geolat
	rt_info.glon = radarsite.geolon

	; Elevation range
	rt_info.elev_beg = elev_beg
	rt_info.elev_end = elev_end
	rt_info.elev_stp = elev_stp

	; Binning parameter
	rt_info.bin = bin

	; Binning parameter
	rt_info.nhop = nhop

	; Output directory
	rt_info.outdir = dirname

	; Normalize power distributions
	if ~keyword_set(no_ionos) then begin
		ionosinds = where(rt_data.gscatter eq 2b, ccionos)
		if ccionos gt 0 then begin
			rt_data.power[ionosinds] = 10.*alog10( rt_data.lagpower[ionosinds]/max(rt_data.lagpower[ionosinds]) )
			rt_data.power[ionosinds] = rt_data.power[ionosinds]*30./45.;abs(min(rt_data.power[ionosinds]))
		endif
	endif
	ginds = where(rt_data.gscatter eq 1b, ccgnd)
	if ccgnd gt 0 then begin
		rt_data.power[ginds] = 10.*alog10( rt_data.lagpower[ginds]/max(rt_data.lagpower[ginds]) )
	endif
	noinds = where(rt_data.gscatter eq 0, ccno)
	if ccno gt 0 then $
		rt_data.power[noinds] = 10000.

	if ~keyword_set(silent) then $
		print, 'Time filling structures (s): ',(systime(/julian, /utc) - script_time)*86400.d

	; Clear memory a bit
	rantheta = 0
	grpran = 0
	ranhour = 0
	ranazim = 0
	ranelv = 0
	ranalt = 0
	ran = 0
	grndran = 0
	ranlat = 0
	ranlon = 0
	normtheta = 0
	normgrpran = 0
	normhour = 0
	normazim = 0
	normelv = 0
	normalt = 0
	normran = 0
	normgnd = 0
	normweights = 0
	normnrefract = 0
	ionolat = 0
	ionolon = 0
	aspect = 0
	edens = 0
	dip = 0

	; Save to file
	if ~keyword_set(no_rays) then $
		rt_read_rays2files, radar, beam, dir='/tmp', nhop=nhop, back=back, outdir=outdir
	if ~keyword_set(silent) then $
		print, 'Time creating ray files (s): ',(systime(/julian, /utc) - script_time)*86400.d
	rtWriteStruct, rt_data, rt_info, code=code, ground=no_ionos, outdir=outdir
	if ~code and ~keyword_set(silent) then $
		prinfo, 'Error writting structure to file'

	; Remove fortran output
	spawn, 'rm -f /tmp/edens.dat'
	spawn, 'rm -f /tmp/ranges.dat'
	spawn, 'rm -f /tmp/ionos.dat'
	spawn, 'rm -f /tmp/rays.dat'

ENDIF ELSE begin
;**************************************************************************************
;**************************************************************************************
; Else read data from files
	if ~keyword_set(silent) then $
		print, 'Raytracing data already present. Reading from files.'

	; Read strcuture from file
	rtReadStruct, date[0], time, radar, beam, freq, rt_data, rt_info, dhour=dhour, nhop=nhop, back=back, ground=no_ionos, code=code, outdir=outdir
	if ~code and ~keyword_set(silent) then $
		prinfo, 'Error reading structure from file'
endelse
;**************************************************************************************
;**************************************************************************************

; Computation time
if ~keyword_set(silent) then $
	print, 'Time elapsed (s): ',(systime(/julian, /utc) - script_time)*86400.d


END

