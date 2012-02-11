function	rad_fit_trend, date=date, time=time, silent=silent

common rad_data_blk

; get index for current data
data_index = rad_fit_get_data_index()
if data_index eq -1 then $
	stop

if (*rad_fit_info[data_index]).nrecs eq 0L then begin
	if ~keyword_set(silent) then begin
		prinfo, 'No data in index '+string(data_index)
		rad_fit_info
	endif
	stop
endif

param= 'power'
beam = rad_get_beam()
coords = get_coordinates()
freq_band = get_default_range('tfreq')

if ~keyword_set(date) then begin
	if ~keyword_set(silent) then $
		prinfo, 'No DATE given, trying for scan date.'
	caldat, (*rad_fit_data[data_index]).juls[0], month, day, year
	date = year*10000L + month*100L + day
endif

if ~keyword_set(time) then $
	time = [0000,2400]

scan_id = -1
sfjul, date, time, sjul, fjul, long=long

if n_elements(channel) eq 0 and scan_id eq -1 then begin
	channel = (*rad_fit_info[data_index]).channels[0]
endif

; Determine maximum width to plot scan - to decide how big a 'data gap' has to
; be before it really is a data gap.  Default to 5 minutes
if ~keyword_set(max_gap) then $
	max_gap = 5.

; get data
xtag = 'juls'
ytag = param
if ~tag_exists((*rad_fit_data[data_index]), xtag) then begin
	if ~keyword_set(silent) then $
		prinfo, 'Parameter '+xtag+' does not exist in RAD_FIT_DATA.'
	stop
endif
if ~tag_exists((*rad_fit_data[data_index]), ytag) then begin
	if ~keyword_set(silent) then $
		prinfo, 'Parameter '+ytag+' does not exist in RAD_FIT_DATA.'
	stop
endif
s = execute('xdata = (*rad_fit_data[data_index]).'+xtag)
s = execute('ydata = (*rad_fit_data[data_index]).'+ytag)

; select data to trend
; must fit beam, channel, scan_id, time (roughly) and frequency
;
; check first whether data is available for the user selection
; then find data to actually trend
beam_inds = where((*rad_fit_data[data_index]).beam eq beam, cc)
if cc eq 0 then begin
	if ~keyword_set(silent) then $
		prinfo, 'No data found for beam '+string(beam)
	stop
endif
txdata = xdata[beam_inds]
tchann = (*rad_fit_data[data_index]).channel[beam_inds]
ttfreq = (*rad_fit_data[data_index]).tfreq[beam_inds]
tscani = (*rad_fit_data[data_index]).scan_id[beam_inds]
if n_elements(channel) ne 0 then begin
	scch_inds = where(tchann eq channel, cc)
	if cc eq 0 then begin
		if ~keyword_set(silent) then $
			prinfo, 'No data found for for beam '+string(beam)+$
				' and channel '+string(channel)
		stop
	endif
endif else if scan_id ne -1 then begin
	if scan_id eq -1 then $
		stop
	scch_inds = where(tscani eq scan_id, cc)
	if cc eq 0 then begin
		if ~keyword_set(silent) then $
			prinfo, 'No data found for for beam '+string(beam)+$
				' and scan_id '+string(scan_id)
		stop
	endif
endif
tchann = 0
tscani = 0
txdata = txdata[scch_inds]
ttfreq = ttfreq[scch_inds]
juls_inds = where(txdata ge sjul-10.d/1440.d and txdata le fjul+10.d/1440.d, cc)
if cc eq 0 then begin
	if ~keyword_set(silent) then $
		prinfo, 'No data found for beam '+string(beam)+$
			' and time '+format_time(time)
	stop
endif
txdata = 0
ttfreq = ttfreq[juls_inds]
tfre_inds = where(ttfreq*0.001 ge freq_band[0] and ttfreq*0.001 le freq_band[1], cc)
if cc eq 0 then begin
	if ~keyword_set(silent) then $
		prinfo, 'No data found for beam '+string(beam)+$
			' and channel '+string(channel)+' and time '+format_time(time) + $
			' and freq. band '+strjoin(string(freq_band,format='(F4.1)'),'-')
	stop
endif
ttfreq = 0

; get indeces of data to trend
if n_elements(channel) ne 0 then begin
	beam_inds = where((*rad_fit_data[data_index]).beam eq beam and $
		(*rad_fit_data[data_index]).channel eq channel and $
		(*rad_fit_data[data_index]).juls ge sjul-10.d/1440.d and $
		(*rad_fit_data[data_index]).juls le fjul+10.d/1440.d and $
		(*rad_fit_data[data_index]).tfreq*0.001 ge freq_band[0] and $
		(*rad_fit_data[data_index]).tfreq*0.001 le freq_band[1], $
		nbeam_inds)
endif else if scan_id ne -1 then begin
	beam_inds = where((*rad_fit_data[data_index]).beam eq beam and $
		(*rad_fit_data[data_index]).scan_id eq scan_id and $
		(*rad_fit_data[data_index]).juls ge sjul-10.d/1440.d and $
		(*rad_fit_data[data_index]).juls le fjul+10.d/1440.d and $
		(*rad_fit_data[data_index]).tfreq*0.001 ge freq_band[0] and $
		(*rad_fit_data[data_index]).tfreq*0.001 le freq_band[1], $
		nbeam_inds)
endif

old_lagfr = (*rad_fit_data[data_index]).lagfr[beam_inds[0]]
old_smsep = (*rad_fit_data[data_index]).smsep[beam_inds[0]]

ajul = (sjul+fjul)/2.d
caldat, ajul, mm, dd, year
yrsec = (ajul-julday(1,1,year,0,0,0))*86400.d
rad_define_beams, (*rad_fit_info[data_index]).id, (*rad_fit_info[data_index]).nbeams, $
	(*rad_fit_info[data_index]).ngates, (*rad_fit_info[data_index]).bmsep, year, yrsec, coords=coords, $
	lagfr0=(*rad_fit_data[data_index]).lagfr[beam_inds[0]], smsep0=(*rad_fit_data[data_index]).smsep[beam_inds[0]], $
	fov_loc_full=fov_loc_full, fov_loc_center=fov_loc_center


trendARR = dindgen(nbeam_inds-1L,2)
; Cycle through beams to trend
FOR b=0L, nbeam_inds-2L DO BEGIN

	; If lag to first range or gate length has changed then update 
	; field-of-view info
	IF (*rad_fit_data[data_index]).lagfr[beam_inds[b]] NE old_lagfr OR $
		(*rad_fit_data[data_index]).smsep[beam_inds[b]] NE old_smsep THEN BEGIN
		rad_define_beams, (*rad_fit_info[data_index]).id, (*rad_fit_info[data_index]).nbeams, $
			(*rad_fit_info[data_index]).ngates, (*rad_fit_info[data_index]).bmsep, year, yrsec, coords=coords, $
			lagfr0=(*rad_fit_data[data_index]).lagfr[beam_inds[b]], smsep0=(*rad_fit_data[data_index]).smsep[beam_inds[b]], $
			fov_loc_full=fov_loc_full, fov_loc_center=fov_loc_center
		old_lagfr = (*rad_fit_data[data_index]).lagfr[beam_inds[b]]
		old_smsep = (*rad_fit_data[data_index]).smsep[beam_inds[b]]
	ENDIF

	start_time = xdata[beam_inds[b]]
	; check for data gaps
	if xdata[beam_inds[b+1]] gt start_time+max_gap/1440.d then $
		; make this point 1 minute long, just for default resons
		end_time = start_time+1./1440.d $
	else $
		end_time = xdata[beam_inds[b+1]]

	; create trend over one beam index
	; average power trend over all range gate
	; use power as weighing function
	tr = findgen((*rad_fit_info[data_index]).ngates)
	trinds = where(ydata[beam_inds[b],*] ne 10000. AND tr[*] gt 10.)
	IF trinds[0] GE 0. THEN BEGIN
		trdata = ROUND(TOTAL((10^ydata[beam_inds[b],trinds])*tr[trinds])/TOTAL((10^ydata[beam_inds[b],trinds]))) 
; 		trdata = ROUND(PRODUCT(tr[trinds]^(ydata[beam_inds[b],trinds]))^(1/(ydata[beam_inds[b],trinds])))
	ENDIF ELSE $
		trdata = 0L

	; update trend array
	trendARR[b,0] = start_time
	trendARR[b,1] = fov_loc_center[0,beam,trdata]
ENDFOR

trendPTR = PTR_NEW(/ALLOCATE_HEAP) 
*trendPTR = trendARR

return, trendPTR

end
