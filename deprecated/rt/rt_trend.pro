function rt_trend, date=date, time=time, silent=silent
	
common	rt_data_blk

if ~keyword_set(date) then begin
	if ~keyword_set(silent) then $
		prinfo, 'No DATE given, trying for scan date.'
	caldat, rt_data.juls[0], month, day, year
	date = year*10000L + month*100L + day
endif

if ~keyword_set(time) then $
	time = [0000,2400]

sfjul, date, time, sjul, fjul
xrange = [sjul, fjul]

; Determine maximum width to plot scan - to decide how big a 'data gap' has to
; be before it really is a data gap.  Default to 5 minutes
if ~keyword_set(max_gap) then $
	max_gap = 120.

; get data
xtag = 'juls'
ytag = 'power'
s = execute('xdata = rt_data.'+xtag)
s = execute('ydata = rt_data.'+ytag)

; select data to plot
; must fit beam, channel, scan_id, time (roughly) and frequency
;
; check first whether data is available for the user selection
; then find data to actually plot
txdata = xdata
juls_inds = where(txdata ge sjul-10.d/1440.d and txdata le fjul+10.d/1440.d, cc)
if cc eq 0 then begin
	if ~keyword_set(silent) then $
		prinfo, 'No data found for time '+format_time(time)
	stop
endif
txdata = 0

; Add final elements for full plots
tsteps = n_elements(xdata)
xdata = [xdata, xdata[tsteps-1]+1]

trendARR = dindgen(tsteps,2)

; overplot data
; Cycle through beams to plot
FOR b=0, tsteps-1 DO BEGIN
	start_time = xdata[b]
	end_time = MIN( [xdata[b+1], start_time+max_gap/1440.d] )
	; create trend over one beam index
	; average power trend over all range gate
	; use power as weighing function
	tr = findgen(rt_info.ngates)
	trinds = where(ydata[b,*] gt 0. AND tr[*] gt 25.)
	IF trinds[0] GE 0. THEN $
		trdata = ROUND(TOTAL(10^ydata[b,trinds]*tr[trinds])/TOTAL(10^ydata[b,trinds])) $
	ELSE $
		trdata = 0L

	; update trend array
	trendARR[b,0] = start_time
	trendARR[b,1] = rt_info.fov_loc_center[trdata]

ENDFOR

trendPTR = PTR_NEW(/ALLOCATE_HEAP) 
*trendPTR = trendARR

return, trendPTR

end

