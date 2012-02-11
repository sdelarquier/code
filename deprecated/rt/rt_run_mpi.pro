;+ 
; NAME: 
; RT_RUN
;
; PURPOSE: 
; This procedure runs raytracing code or read existing raytracing results into
; the common block RT_DATA_BLK. The time range for which data is read is controled by
; the DATE and TIME keywords.
; 
; CATEGORY: 
; Input/Output
; 
; CALLING SEQUENCE:
; RT_RUN, Date, Radar[, time=time, beam=beam, mmin=mmin, freq=freq, el_stp=el_stp, /DHOUR, /rplot]
;
; INPUTS:
; DATE: the date for which you want to run the raytracing (for IRI model)
; Format is YYYYMMDD
;
; RADAR: the radar code for which you want to run the raytracing
;
; KEYWORD PARAMETERS:
; BEAM: the beam for which you want to run the raytracing (default is boresight)
;
; TIME: time interval for which you want to run the raytracing, default is [0000,2400]
; If TIME[1] is greater or equal to TIME[0] then TIME[1] is taken to be on the next day.
;
; FREQ: operating frequency in MHz. default is 11MHz
;
; EL_STP: elevation angle step size in degrees
;
; DHOUR: set this keyword to run with hour precision (default 1/2 hr)
;
; MMIN: set this keyword to the requiered precision in minutes
;
; NHOP: specify the number of hop considered in the ray-tracing. Default is 1.
;
; IONOS: set this keyword to compute ionospheric scatter too (takes longer)
; 
; GEOPOS: [latitude, longitude]. Overrides the radar command.
;
; AZIM: To be specified when using the geopos keyword
;
; BIN: set this keyword to the desired parameter for range gate binning (phase, group of ground range).
; Default is group, same as for radars.
;
; COMMON BLOCKS:
; RT_DATA_BLK: The common block holding the currently loaded raytracing data and 
; information about that data.
;
; EXAMPLE:
;	; Run the raytracing for august 2 2010 for Blackstone
;	; from 17 to 24 LT 
;	rt_run, 20100802, 'bks', time=[1700,2400], sti='LT'
;	; Plot results on range-time plot for the first 60 gates
;	rt_plot_rti, yrange=[0,60]
;
; COPYRIGHT:
; Non-Commercial Purpose License
; Copyright © November 14, 2006 by Virginia Polytechnic Institute and State University
; All rights reserved.
; Virginia Polytechnic Institute and State University (Virginia Tech) owns the DaViT
; software and its associated documentation (“Software”). You should carefully read the
; following terms and conditions before using this software. Your use of this Software
; indicates your acceptance of this license agreement and all terms and conditions.
; You are hereby licensed to use the Software for Non-Commercial Purpose only. Non-
; Commercial Purpose means the use of the Software solely for research. Non-
; Commercial Purpose excludes, without limitation, any use of the Software, as part of, or
; in any way in connection with a product or service which is sold, offered for sale,
; licensed, leased, loaned, or rented. Permission to use, copy, modify, and distribute this
; compilation for Non-Commercial Purpose is hereby granted without fee, subject to the
; following terms of this license.
; Copies and Modifications
; You must include the above copyright notice and this sti=sti, license on any copy or modification
; of this compilation. Each time you redistribute this Software, the recipient automatically
; receives a license to copy, distribute or modify the Software subject to these terms and
; conditions. You may not impose any further restrictions on this Software or any
; derivative works beyond those restrictions herein.
; You agree to use your best efforts to provide Virginia Polytechnic Institute and State
; University (Virginia Tech) with any modifications containing improvements or
; extensions and hereby grant Virginia Tech a perpetual, royalty-free license to use and
; distribute such modifications under the terms of this license. You agree to notify
; Virginia Tech of any inquiries you have for commercial use of the Software and/or its
; modifications and further agree to negotiate in good faith with Virginia Tech to license
; your modifications for commercial purposes. Notices, modifications, and questions may
; be directed by e-mail to Stephen Cammer at cammer@vbi.vt.edu.
; Commercial Use
; If you desire to use the software for profit-making or commercial purposes, you agree to
; negotiate in good faith a license with Virginia Tech prior to such profit-making or
; commercial use. Virginia Tech shall have no obligation to grant such license to you, and
; may grant exclusive or non-exclusive licenses to others. You may contact Stephen
; Cammer at email address cammer@vbi.vt.edu to discuss commercial use.
; Governing Law
; This agreement shall be governed by the laws of the Commonwealth of Virginia.
; Disclaimer of Warranty
; Because this software is licensed free of charge, there is no warranty for the program.
; Virginia Tech makes no warranty or representation that the operation of the software in
; this compilation will be error-free, and Virginia Tech is under no obligation to provide
; any services, by way of maintenance, update, or otherwise.
; THIS SOFTWARE AND THE ACCOMPANYING FILES ARE LICENSED “AS IS”
; AND WITHOUT WARRANTIES AS TO PERFORMANCE OR
; MERCHANTABILITY OR ANY OTHER WARRANTIES WHETHER EXPRESSED
; OR IMPLIED. NO WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE IS
; OFFERED. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF
; THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE,
; YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
; CORRECTION.
; Limitation of Liability
; IN NO EVENT WILL VIRGINIA TECH, OR ANY OTHER PARTY WHO MAY
; MODIFY AND/OR REDISTRIBUTE THE PRORAM AS PERMITTED ABOVE, BE
; LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL,
; INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR
; INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS
; OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED
; BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE
; WITH ANY OTHER PROGRAMS), EVEN IF VIRGINIA TECH OR OTHER PARTY
; HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
; Use of Name
; Users will not use the name of the Virginia Polytechnic Institute and State University nor
; any adaptation thereof in any publicity or advertising, without the prior written consent
; from Virginia Tech in each case.
; Export License
; Export of this software from the United States may require a specific license from the
; United States Government. It is the responsibility of any person or organization
; contemplating export to obtain such a license before exporting.
;
; MODIFICATION HISTORY:
; Based on Lasse Clausen, RAD_FIT_READ
; Based on Nitya Ravindran, RT
; Written by Sebastien de Larquier, Sept. 2010
;	Last modified 08-2011
;-
pro 	RT_RUN, date, radar, hour=hour, mmin=mmin, $
	time=time, beam=beam, nhop=nhop, force=force, $
	freq=freq, el_stp=el_stp, ionos=ionos, geopos=geopos, $
	back=back, silent=silent, bin=bin, azim=azim, mpi=mpi
	
common	rt_data_blk
common	radarinfo

if ~keyword_set(silent) then $
	print, 	'---------------running RT_RUN---------------'
; procedure to execute multiple hours of raytracing

if ~keyword_set(beam) then $
	beam = 7

if ~keyword_set(freq) then $
	freq = 11.

if ~keyword_set(bin) then $
	bin = 'group'

; Start MPD
if keyword_set(mpi) then $
	spawn, 'mpd &'

; Read radar location and boresight
if keyword_set(geopos) then begin
	radar = 'custom'
	if ~keyword_set(azim) then begin
		prinfo, 'No azimuth specified.'
		return
	endif
	beam = STRTRIM(string(geopos[0],format='(F06.1)'),2)+STRTRIM(string(geopos[1],format='(F06.1)'),2) + $
		STRTRIM(string(azim,format='(F06.1)'),2)
endif else begin
	radID = where(network.code[0,*] eq radar)
	radarsite = network[radID].site[where(network[radID].site.tval eq -1)]
	geopos = fltarr(2)
	geopos[0] = radarsite.geolat
	geopos[1] = radarsite.geolon
endelse

; Save time at begining of run
script_time = systime(/julian, /utc)

; Choose day for IRI model
IF n_elements(date) gt 1 THEN BEGIN
	prinfo, 'date does not accept more than one element (for now)'
	return
ENDIF

; Choose hours for IRI model (use findgen to generate many succesive hours)
parse_date, date, year, month, day
IF ~keyword_set(time) THEN $
	time = [0000,2400]
parse_time, time, shour ,sminutes, fhour, fminutes
hour = [shour, fhour]
minutes = [sminutes, fminutes]
IF keyword_set(dhour) THEN $
	hfact = 1. $
ELSE IF keyword_set(mmin) THEN $
	hfact = 60./mmin $
ELSE $
	hfact = 2.
IF n_elements(time) eq 1 THEN $
	arr_hr = hour[0] + minutes[0]*1./60. $
ELSE IF time[1] le time[0] THEN BEGIN
	arr_hr = [hour[0] + minutes[0]*1./60. + findgen((24-hour[0])*hfact)*1/hfact, $
		minutes[0]*1./60. + findgen((hour[1]+1)*hfact)*1/hfact]
ENDIF ELSE BEGIN
	arr_hr = [hour[0] + minutes[0]*1./60. + findgen((hour[1]-hour[0])*hfact)*1/hfact]
ENDELSE

; Choose time zone (UT, LT)
IF ~keyword_set(sti) THEN $
	sti = 'UT'
tz=sti

; Get path to davit
davit_lib = getenv("DAVIT_LIB")

; Some useful parameters
ngates 		= 70
range_gate= [180L + 45L*FINDGEN(ngates+1)]
Re = 6370.
P = 180. + findgen(ngates)*45.
minpower = 4.

; Number of time steps and beams
nbeams 		= n_elements(beam)
tsteps 		= n_elements(arr_hr)

;******************************************************************************************************
; cycle trhough the hours to generate raytrace
;******************************************************************************************************
nrecs = 0
adate = date
FOR hr = 0, (tsteps-1) DO BEGIN

	IF hr gt 1 THEN $
		IF (arr_hr[hr] - arr_hr[hr-1]) lt 0. then begin
			juld = temp_rt_data.juls(hr-1)
			juld++
			CALDAT, juld, month, day, year
			adate = year*10000L + month*100L + day
		endif
	; adjust for UT or LT
	IF (arr_hr[hr] gt 24.) then BREAK
	CASE tz OF
		'UT': hrU=arr_hr[hr]+25.
		'LT': hrU=arr_hr[hr]
	ENDCASE

	;****************************************************************************************************
	; Cycle through beams
	FOR b=0,nbeams-1 do begin
		if ~keyword_set(silent) then $
			PRINT, 'Process hour:', hrU, '; Beam:', beam[b]
			

		; Calculate azimuth
		azim = rt_get_azim(radar, beam[b])
		if keyword_set(back) then $
			azim = (azim + 180.) mod 360.
			

		; Generate input file for fortran code
		rt_write_input, radar, adate, hrU, beam=beam[b], outdir=outdir, $
			filename=filename, freq=freq, el_stp=el_stp, nhop=nhop, $
			geopos=geopos, azim=azim, back=back, silent=silent


		; Create directory for this time
		if ~file_test('~/tmp/rt', /dir) then begin
			FILE_MKDIR, '~/tmp'
			FILE_MKDIR, '~/tmp/rt'
		endif
		dirname='~/tmp/rt/ray_'+radar+STRTRIM(beam[b],2)+'_'+STRTRIM(adate,2)+'_'+ $
				STRTRIM(STRING(floor(arr_hr[hr])*100L + ROUND((arr_hr[hr]-floor(arr_hr[hr]))*60.),format='(I04)'),2)+tz
		if keyword_set(back) then $
			dirname = dirname+'b'
		if ~keyword_set(silent) then $
			print, dirname
		dirTest = FILE_TEST(dirname+'/rays.dat', /READ)
		

		;*******************************************
		; Run raytracing if no data already exist
		IF ~(dirTest EQ 1) OR keyword_set(force) THEN BEGIN
			FILE_MKDIR, dirname

			spawn, 'echo "'+outdir+filename+'" > rt_inp_file'
			if ~keyword_set(mpi) then $
				spawn, davit_lib+'/vt/fort/rt/raydarn < rt_inp_file' $
			else $
				spawn, 'mpiexec -n '+strtrim(mpi, 2)+' '+davit_lib+'/vt/fort/rtmpi/raydarn < rt_inp_file'

			; Copy raytracing output files into hourly folders
			FILE_MOVE, ['edens.dat', outdir+filename, 'ranges.dat', $
				'rays.dat','day_edens.dat','dip.dat'], dirname, /overwrite

		ENDIF ELSE begin
			if ~keyword_set(silent) then $
				print, 'Raytracing data already present. Reading from files.'
		endelse
		;*******************************************
		

		;*******************************************
		; Read data from files to be saved in common block
		rt_read_header, txlat, txlon, azim, $
			elev_beg, elev_end, elev_stp, freq, $
			year, mmdd, hour, nrays=nrays, mpi=mpi, dir=dirname

		; Ground scatter
		rt_read_ranges, numran, rantheta, grpran, ranelv, ranalt, grpeqran, $
			mpi=mpi, dir=dirname, grndran=grndran

		; Ionospheric scatter
		if keyword_set(ionos) then begin
			rt_get_normrays, normpath, normelev, normalt, normweights, normnr, dirname=dirname, $
				normeqpath=normeqpath, normgrnd=normgrnd
		endif
		;*******************************************
		

		; display number of ground scatter
		if ~keyword_set(silent) then $
			print, 'Gnd scatter = '+STRTRIM(numran,2)
			
	
		; initialize temp structure at first loop run
		IF hr EQ 0 and b EQ 0 THEN BEGIN
				rt_data = { $
					juls: dblarr(tsteps,nbeams), $
					beam: intarr(tsteps,nbeams), $
					azim: fltarr(tsteps,nbeams), $
					numran: intarr(tsteps,nbeams), $
					lagpower: fltarr(tsteps,nbeams,ngates), $
					power: fltarr(tsteps,nbeams,ngates), $
					gscatter: bytarr(tsteps,nbeams,ngates), $
					ionoscatter: bytarr(tsteps,nbeams,ngates), $
					altitude: fltarr(tsteps,nbeams,ngates), $
					valtitude: fltarr(tsteps,nbeams,ngates), $
					elevation: fltarr(tsteps,nbeams,ngates), $
					grange: fltarr(tsteps,nbeams,ngates), $
					nr: fltarr(tsteps,nbeams,ngates), $
					tfreq: fltarr(tsteps,nbeams), $
					edens: fltarr(tsteps,nbeams,500) $
				}

				rt_info = { $
					name: '', $
					id: 0L, $
					nrecs: 0L, $
					sjul: 0.d, $
					fjul: 0.d, $
					timez: '', $
					ngates: 0L, $
					glat: 0., $
					glon: 0., $
					bin: '' $
				}
		ENDIF
		; Data stored
		nrecs = nrecs + numran


		; Define how to bin the data
		case bin of
			'phase':	range_sort = grpran
			'group':	range_sort = grpeqran
			'ground':	range_sort = grndran
		endcase
		if keyword_set(ionos) then begin
			case bin of
				'phase':	ionosrange_sort = normpath
				'group':	ionosrange_sort = normeqpath
				'ground':	ionosrange_sort = normgrnd
			endcase
		endif


		;*******************************************
		; populate structures
		if numran eq 0 then $
			numran = 1
			
		; times in julian dates
		temp_rt_data.juls[hr,b] = JULDAY(month,day,year,floor(arr_hr[hr]),(arr_hr[hr]-floor(arr_hr[hr]))*60.)
		
		; beam and corresponding azimuth
		temp_rt_data.beam[hr,b] = beam[b]
		temp_rt_data.azim[hr,b] = azim
		
		; number of ray reaching the ground
		temp_rt_data.numran[hr,b] = numran
		
		; operating frequency
		temp_rt_data.tfreq[hr,b] = freq
		
		; electron density in the center of the domain
		rt_read_edens, edens, mpi=mpi, dir=dirname
		temp_rt_data.edens[hr,b,*] = edens[*,249]
		
		; linear power
		temp_rt_data.lagpower[hr,b,*] = histc(range_sort*1e-3,range_gate)
		
		; log power
		temp_rt_data.power[hr,b,*] = 10D*ALOG10(temp_rt_data.lagpower[hr,*])
		
		; Ground scatter flag
		gscatterinds = where(temp_rt_data.power[hr,b,*] ge minpower, cc)
		if cc gt 0 then $
			temp_rt_data.gscatter[hr,b,gscatterinds] = 1b
			
		; ionospheric linear and log power and scatter flag
		if keyword_set(ionos) then begin
			ionoslagpower = histc(ionosrange_sort,range_gate, weights=normweights)
			ionosinds = where(ionoslagpower gt 0.,cc)
			ionospower = 10D*ALOG10( ionoslagpower )
			if cc gt 0 then begin
				temp_rt_data.lagpower[hr,b,ionosinds] = ionoslagpower[ionosinds]
				temp_rt_data.power[hr,b,ionosinds] = ionospower[ionosinds]
				temp_rt_data.ionoscatter[hr,b,ionosinds] = 1b
			endif
		endif
		
		for ng=0, ngates-1 do begin
			gateinds = where(range_sort*1e-3 LT range_gate[ng+1] AND range_sort*1e-3 GE range_gate[ng])
			
			; elevation angle for GS
			temp_rt_data.elevation[hr,b,ng] = ( gateinds[0] ge 0 and temp_rt_data.gscatter[hr,b,ng] eq 1b ? MEAN(ranelv[gateinds]) : -1000.)
			; reflection altitude for GS
			temp_rt_data.altitude[hr,b,ng] = ( gateinds[0] ge 0 and temp_rt_data.gscatter[hr,b,ng] eq 1b  ? MEAN(ranalt[gateinds]) : -1000.)
			; virtual height for GS
			temp_rt_data.valtitude[hr,b,ng] = sqrt(P[ng]^2./4. + Re^2. + 2.*Re*P[ng]/2.*sin(temp_rt_data.elevation[hr,b,ng]*!dtor)) - Re
			temp_rt_data.valtitude[hr,b,ng] = (temp_rt_data.gscatter[hr,b,ng] eq 1b ? temp_rt_data.valtitude[hr,b,ng] : -1000.)
			; ground range for GS
			temp_rt_data.grange[hr,b,ng] = ( gateinds[0] ge 0 and temp_rt_data.gscatter[hr,b,ng] eq 1b  ? MEAN(grndran[gateinds]) : -1000.)
			
			if keyword_set(ionos) then begin
				gateindsionos = where(ionosrange_sort LT range_gate[ng+1] AND ionosrange_sort GE range_gate[ng])
				
				; elevation angle for IS
				temp_rt_data.elevation[hr,b,ng] = ( gateindsionos[0] ge 0 and temp_rt_data.ionoscatter[hr,b,ng] eq 1b ? MEAN(normelev[gateindsionos]) : -1000.)
				; reflection altitude for IS
				temp_rt_data.altitude[hr,b,ng] = ( gateindsionos[0] ge 0 and temp_rt_data.ionoscatter[hr,b,ng] eq 1b ? MEAN(normalt[gateindsionos]) : -1000.)
				; virtual height for IS
				temp_rt_data.valtitude[hr,b,ng] = sqrt(P[ng]^2. + Re^2. + 2.*Re*P[ng]*sin(temp_rt_data.elevation[hr,b,ng]*!dtor)) - Re
				temp_rt_data.valtitude[hr,b,ng] = (temp_rt_data.ionoscatter[hr,b,ng] eq 1b ? temp_rt_data.valtitude[hr,b,ng] : 0.)
				; refraction index for ionos scatter (at reflection point)
				temp_rt_data.nr[hr,b,ng] = ( gateindsionos[0] ge 0 and temp_rt_data.ionoscatter[hr,b,ng] eq 1b ? MEAN(normnr[gateindsionos]) : -1000.)
				; ground range for IS
				temp_rt_data.grange[hr,b,ng] = ( gateindsionos[0] ge 0 and temp_rt_data.ionoscatter[hr,b,ng] eq 1b  ? MEAN(normgrnd[gateindsionos]) : -1000.)
			endif
		endfor
		; start and end times
		sfjul, adate, time, sjul, fjul
		temp_rt_info.sjul = sjul
		temp_rt_info.fjul = sjul

		; Radar code name
		temp_rt_info.name = radar

		; radar ID
		temp_rt_info.id = network[where(network.code[0] eq radar)].ID

		; Time format (UT or LT)
		temp_rt_info.timez = tz

		; Number of range gates
		temp_rt_info.ngates = ngates

		; Radar position
		temp_rt_info.glat = geopos[0]
		temp_rt_info.glon = geopos[1]

		; Binning parameter
		temp_rt_info.bin = bin
		
		; End populate structures
		;*******************************************

	ENDFOR
	; End beam loop
	;****************************************************************************************************
	
ENDFOR
; end time loop
;******************************************************************************************************
;******************************************************************************************************

temp_rt_info.nrecs = nrecs

; Copy temp structures into common block
rt_data = temp_rt_data
rt_info = temp_rt_info

; Computation time
if ~keyword_set(silent) then $
	print, 'Time elapsed (s): ',(systime(/julian, /utc) - script_time)*86400.d


END

