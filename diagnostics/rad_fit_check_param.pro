;+
; NAME:
; rad_fit_check_param
;
; PURPOSE:
; This procedure reads radar fitacf data for a whole day and plots a given parameter
; as a function of time.
;
; CATEGORY:
; Diagnostics
;
; CALLING SEQUENCE:
; rad_fit_check_param, param, radar, date=date
;
; INPUTS:
; Radar: Set this to a 3-letter radar code to indicate the radar for which to read
; data.
;
; KEYWORD PARAMETERS:
; Date: The date of which to read data. Can be a scalar in YYYYMMDD format or
; a 2-element vector in YYYYMMDD format. Defaults to yesterday.
;
; PROCEDURE:
;
; COMMON BLOCKS:
; RAD_DATA_BLK: The common block holding the currently loaded radar data and
; information about that data.
;
; RADARINFO: The common block holding data about all radar sites (from RST).
;
; EXAMPLE:
;
; COPYRIGHT:
;
; MODIFICATION HISTORY:
; Written by Sebastien de Larquier, Jun, 30, 2012
;-
pro rad_fit_check_param, param, radar, date=date, time=time, $
	oldfit=oldfit, fitacf=fitacf, fitex=fitex, $
	ps=ps, clock=clock, scale=scale, beam=beam, $
	position=position, charsize=charsize, scanbeams=scanbeams, $
	noxlabels=noxlabels, notitle=notitle, schedule=schedule, average=average, ratio=ratio

common radarinfo
common rad_data_blk

; If no date is given, select previous day
if ~keyword_set(date) then begin
	juld = systime(/utc, /julian)
	caldat, juld-1.d, month, day, year
	date = year*10000L + month*100L + day
endif
; Calculate julian day
tdate = date
parse_date, tdate, year, month, day
juld = julday(month, day, year, 0, 0)
; Calculate plot x-limits
xrange = [juld, juld+1.d]

; If you need a standalone postscript...
if keyword_set(ratio) then fileparam = 'nave-intt' else fileparam = param
if keyword_set(ps) then begin
	if size(ps,/type) eq 7 then $
		filename = ps $
	else $
		filename = '~/Desktop/check_'+fileparam+'_'+strtrim(tdate,2)+'_'+radar
	ps_open, filename+'.ps'
endif

; Adjust position for right axis labels
if ~keyword_set(position) then $
	position = define_panel(1,1, /no_title)
pos = position
pos[2] = position[2] - .05
; Adjust position for frequency/cpid panel
; pos[3] = position[3] - (position[3]-position[1])/5.


; If no x-axis labels
if keyword_set(noxlabels) then begin
	xtickname = replicate(' ', 60)
	xtitle = ''
	xtickformat = ''
endif else begin
	xtickname = ''
	xtitle = 'Time [UT]'
	xtickformat = 'label_date'
endelse

; Set ticks length
yticklen = -.02
xticklen = -.02

; ****************************
;- Schedule
; ****************************
if keyword_set(schedule) then begin
	loadct, 0
	switchud = 0
	rad_fit_check_schedule, date, schedule=rad_schedule
	plot, xrange, [0,1], /nodata, $
		xstyle=1, ystyle=1, position=[pos[0],pos[1]-.07,pos[2], pos[1]-.04], charsize=charsize, $
		ytitle='', yticks=1, ytickname=replicate(' ',60), $
		xtitle='', xtickformat='', xticks=4, xtickv=xrange[0]+dindgen(5)*6./24.d, xminor=6, xtickname=replicate(' ',60), xticklen=.2
	for isc=0,rad_schedule.nsch-1 do begin
		oplot, juld + rad_schedule.time[isc]*1.d/24.d*[1.d,1.d], [0,1], col=120, linestyle=1
		xyouts, juld + rad_schedule.time[isc]*1.d/24.d, .05 + switchud*.6, rad_schedule.name[isc], /data, charsize=charsize
		switchud = 1 - switchud
	endfor
	xyouts, .5, pos[1]-.08, 'Schedule', align=.5, /normal, charsize=charsize
endif

; Read file in required format and populate structures
rad_fit_read, tdate, radar, time=time, oldfit=oldfit, fitacf=fitacf, fitex=fitex

; Get data index
data_index = rad_fit_get_data_index()
if data_index ne -1 then begin
    if (*rad_fit_info[data_index]).code ne radar then data_index = -1
endif
if data_index eq -1 then begin
	; If no data, draw empty plotplot, xrange, yrange, /nodata, $
	plot, xrange, [0,1], /nodata, $
		xstyle=1, ystyle=1, position=pos, charsize=charsize, $
		ytitle=strupcase(radar), yticklen=yticklen, xticklen=xticklen, $
		xtitle=xtitle, xtickformat=xtickformat, xticks=4, xtickv=xrange[0]+dindgen(5)*6.d/24.d, xminor=6, xtickname=xtickname
	; Add an unambiguous statement about data availability for this radar on that day...
	xyouts, xrange[0]+(xrange[1]-xrange[0])/2., .5, 'NO DATA', charsize=charsize, align=.5
	return
endif

; Set parameter
s = execute('xdata = (*rad_fit_data[data_index]).'+param)
if param eq 'tfreq' then xdata = xdata*1e-3

; If using the ratio keyword, then do nave/intt (just for Jeff)
if keyword_set(ratio) then xdata = float((*rad_fit_data[data_index]).nave)/(*rad_fit_data[data_index]).intt

; Get number of records
nrecs = (*rad_fit_info[data_index]).nrecs

; Get number of beams
nbeams = (*rad_fit_info[data_index]).nbeams

; Create a time array, because I'm tired of typing the whole structure...
atime = (*rad_fit_data[data_index]).juls

; Beam and time array (for compatibility with data array)
if size(xdata,/n_dim) eq 2 then begin
	databeams = (1 + intarr((*rad_fit_info[data_index]).ngates)) ## (*rad_fit_data[data_index]).beam
	datatime =  (1 + intarr((*rad_fit_info[data_index]).ngates)) ## atime
endif else begin
	databeams = (*rad_fit_data[data_index]).beam
	datatime = atime
endelse

; Set beam
if keyword_set(beam) then begin
	setbeam = 1
	beaminds = where(databeams eq beam, cc)
	if cc le 0 then begin
		prinfo, 'Ohoh! There is a problem with '+radar+', beam '+strtrim(beam,2)
		return
	endif
	strbeam = strtrim(beam,2)
endif else begin
	setbeam = 0
	beaminds = indgen(n_elements(databeams))
	strbeam = 'all'
endelse

; If the main parameter is noise, then we have to do something special
if strmid(param,0,5) eq 'noise' then begin
	ylog = 1
	; If no scale is provided, just do your best to guess what it should be...
	if ~keyword_set(scale) then $
		scale = [1e0, 1e5]
endif else begin
	ylog = 0
	; If no scale is provided, just do your best to guess what it should be...
	if ~keyword_set(scale) then $
		scale = [floor(min(xdata[where(xdata[beaminds] ne 10000)])), ceil(max(xdata[where(xdata[beaminds] ne 10000)]))]
endelse

; Get character size
if ~keyword_set(charsize) then $
	charsize = get_charsize(1,1)

; Calculate plot y-limits
yrange = scale

; Set up plotting area
if keyword_set(ratio) then $
	ytitle = strupcase(radar)+' ('+strbeam+') - nave/intt' $
else $
	ytitle = strupcase(radar)+' ('+strbeam+') - '+param
plot, xrange, yrange, /nodata, ylog=ylog, $
	xstyle=1, ystyle=9, position=pos, charsize=charsize, $
	ytitle=ytitle, yticklen=yticklen, xticklen=xticklen, $
	xtitle=xtitle, xtickformat=xtickformat, xticks=4, xtickv=xrange[0]+dindgen(5)*6.d/24.d, xminor=6, xtickname=xtickname


; ****************************
;- Parameter
; ****************************
load_usersym, /circle
loadct, 0
for ib=0,nbeams-1 do begin
	if setbeam then if ib ne beam then continue
	ibinds = where(databeams eq ib, cc)
	if cc le 0 then begin
		prinfo, 'Ohoh! There is a problem with '+radar+', beam '+strtrim(ib,2)
		continue
	endif
	datainds = where( xdata[ibinds] ne 10000., ccdat )
	if ccdat gt 0 then begin
		oplot, datatime[ibinds[datainds]], xdata[ibinds[datainds]], psym=8, symsize=.1, col=50
		satinds = where( xdata[ibinds[datainds]] gt scale[1] , ccsat )
		if ccsat gt 0 then begin
			rad_load_colortable, /aj
			oplot, datatime[ibinds[datainds[satinds]]], scale[1]*(1.+fltarr(ccsat)), psym=8, symsize=.1, col=250
			loadct, 0
		endif
	endif
endfor


; ****************************
;- Integration/Scan time
; ****************************
if keyword_set(clock) then begin
	; Calculate scan times
	scaninds = where( (*rad_fit_data[data_index]).scan_mark eq 1 , ccscan)
	; If no scan flags, then force it
	if ccscan le 1 then begin
		rad_fit_force_scan_flag, data_index, scan_mark=scan_mark
		scaninds = where( scan_mark eq 1 , ccscan)
		force_flag = '(forced)'
	endif else force_flag = ''
	if ccscan gt 0 then begin
		scantime = fltarr(ccscan-1)
		scanjuls = dblarr(ccscan-1)
		for iscan=0,ccscan-2 do begin
			scantime[iscan] = ( (*rad_fit_data[data_index]).juls[scaninds[iscan+1]-1] + (*rad_fit_data[data_index]).intt[scaninds[iscan+1]-1]/86400.d - (*rad_fit_data[data_index]).juls[scaninds[iscan]] )*1440.d
			scanjuls[iscan] = atime[scaninds[iscan]]
		endfor
	endif else begin
		scantime = 0
		scanjuls = 0.d
	endelse
	; Add an integration/scan time time axis
	yrange = [0., 18.]
	plot, xrange, yrange, /nodata, position=pos, xstyle=5, ystyle=5
	axis, /yaxis, /data, ystyle=1, yrange=intrange, ytitle='', charsize=charsize, $
		ytickv=indgen(4)*3., yticks=4, yminor=3, yticklen=yticklen
	; Add axis titles
	loadct, 10
	xyouts, pos[2]+.07, pos[1]+(pos[3]-pos[1])/2., 'Integration time [s]', col=120, align=.5, orientation=90., /normal, charsize=charsize
	xyouts, pos[2]+.1, pos[1]+(pos[3]-pos[1])/2., 'Scan time [min]'+force_flag, col=50, align=.5, orientation=90., /normal, charsize=charsize
	; Plot scan time (in green)
	plots, scanjuls, scantime, col=50, psym=8, symsize=.1;, linestyle=2
	; Plot integration time (in blue)
	oplot, atime, (*rad_fit_data[data_index]).intt, col=120, psym=8, symsize=.1
endif else if ~keyword_set(scanbeams) then begin
	; Add y plain axis
	axis, /yaxis, /data, ystyle=1, yrange=yrange, ytickname=replicate(' ', 60), yticklen=yticklen
endif


; ****************************
;- Beam number
; ****************************
if keyword_set(scanbeams) then begin
	; Get beam number at begining of scan and rotate through them
	; (scan 0 -> beam 0, scan 1 -> beam 1, ..., scan N -> beam0 + N mod nbeams)
	scaninds = where( (*rad_fit_data[data_index]).scan_mark eq 1 , ccscan)
	; If no scan flags, then force it
	if ccscan le 1 then begin
		rad_fit_force_scan_flag, data_index, scan_mark=scan_mark
		scaninds = where( scan_mark eq 1 , ccscan)
		force_flag = '(forced)'
	endif else force_flag = ''
	if ccscan gt 0 then begin
		scanbeam = fltarr(ccscan-1)
		scanjuls = dblarr(ccscan-1)
		offset = 0
		for iscan=0,ccscan-2 do begin
			scanbeam[iscan] = (*rad_fit_data[data_index]).beam[scaninds[iscan]+1+offset]
			scanjuls[iscan] = atime[scaninds[iscan]]
			if scaninds[iscan]+1+offset ge scaninds[iscan+1] then offset = 0 else offset += 1
		endfor
	endif else begin
		scanbeam = 0
		scanjuls = 0.d
	endelse
	; Add an Beam/Freq axis
	yrange = [0., 36.]
	plot, xrange, yrange, /nodata, position=pos, xstyle=5, ystyle=5
	axis, /yaxis, /data, ystyle=1, yrange=beamrange, ytitle='', charsize=charsize, $
		ytickv=indgen(7)*4., yticks=7, yminor=4, yticklen=yticklen
	; Add axis titles
	loadct, 10
	xyouts, pos[2]+.07, pos[1]+(pos[3]-pos[1])/2., 'Beam #'+force_flag, col=120, align=.5, orientation=90., /normal, charsize=charsize
	; Plot beams
	plots, scanjuls, scanbeam, col=120, psym=8, symsize=.1
	; Add a note to explain this crazy beam plot...
	if ~keyword_set(noxlabels) then begin
		xyouts, .5, pos[1]-.1, 'Note on Beam #: a dot is plotted showing the beam # of the k!Eth!N record of the k!Eth!N scan.', $
			align=.5, orientation=0., /normal, charsize=charsize*.8, col=120
	endif
endif else if ~keyword_set(clock) then begin
	; Add y plain axis
	axis, /yaxis, /data, ystyle=1, yrange=yrange, ytickname=replicate(' ', 60), yticklen=yticklen
endif


; ****************************
;- CPID and averages
; ****************************
; Mark initial cpid
loadct, 0
xyouts, xrange[0]+(xrange[1]-xrange[0])*10.d/1440.d, yrange[1] - (yrange[1]-yrange[0])/15., $
	rad_cpid_translate((*rad_fit_data[data_index]).scan_id[0])+' ('+strtrim((*rad_fit_data[data_index]).scan_id[0], 2)+')', $
	align=0., /data, col=120, charsize=charsize*.7
; Mark cpid changes
iddiff = (*rad_fit_data[data_index]).scan_id[1:nrecs-1] - (*rad_fit_data[data_index]).scan_id[0:nrecs-2]
gapinds = where(iddiff ne 0, ccgap)
; If cpid changes, then mark and annotate it (in gray)
cpidpos = 1		; used to switch the position of the cpid name to avoid overlap
if ccgap gt 0 then begin
	step = 1
	if ccgap gt 24 then step = round(ccgap/10)
	igap = 0L
	while igap lt ccgap do begin
		; Plot cpid change marker
		oplot, atime[gapinds[igap]]*[1,1], yrange, col=120, linestyle=2
		; Plot cpid name and number
		xyouts, atime[gapinds[igap]]+(xrange[1]-xrange[0])*10.d/1440.d, yrange[1] - (yrange[1]-yrange[0])/15. - cpidpos * (yrange[1]-yrange[0])/15., $
			rad_cpid_translate((*rad_fit_data[data_index]).scan_id[gapinds[igap]+1])+' ('+strtrim((*rad_fit_data[data_index]).scan_id[gapinds[igap]+1], 2)+')', $
			align=0., /data, col=120, charsize=charsize*.7

		igap += step
		
		; Plot average and deviation of main parameter over the time the previous mode was run (only if the mode ran for more than 2 hours)
		if keyword_set(average) then begin
			if igap-step eq 0 then previd = 0 else previd = gapinds[igap-step-1]
			if (atime[gapinds[igap-step]]-atime[previd]) lt 2.d/24.d then continue
			datachunck = xdata[previd:gapinds[igap-step]]
			xmean = median( datachunck[sort(datachunck)] ) 
			strxmean = strtrim(string(xmean, format='(I4)'),2)
			xyouts, atime[gapinds[igap-step]]-(xrange[1]-xrange[0])*10.d/1440.d, yrange[1] - (yrange[1]-yrange[0])/15. - (1-cpidpos) * (yrange[1]-yrange[0])/15., strxmean, align=1., /data, charsize=charsize*.9
		endif
		; Add min/max/camping beam if reuqested
		if keyword_set(scanbeams) then begin
			if igap-step eq 0 then previd = 0 else previd = gapinds[igap-step-1]
			if (atime[gapinds[igap-step]]-atime[previd]) lt 2.d/24.d then continue
			loadct, 10
			datachunck = (*rad_fit_data[data_index]).beam[previd:gapinds[igap-step]]
			maxbeam = max( datachunck ) 
			minbeam = min( datachunck ) 
			strmaxbeam = strtrim(string(maxbeam, format='(I2)'),2)
			strminbeam = strtrim(string(minbeam, format='(I2)'),2)
			xyouts, atime[gapinds[igap-step]]-(xrange[1]-xrange[0])*10.d/1440.d, maxbeam, strmaxbeam, align=1., /data, charsize=charsize*.9, col=120
			xyouts, atime[gapinds[igap-step]]-(xrange[1]-xrange[0])*10.d/1440.d, minbeam, strminbeam, align=1., /data, charsize=charsize*.9, col=120
			loadct, 0
		endif

		cpidpos = 1 - cpidpos
	endwhile

	; Plot average and deviation of main parameter over the time the last mode was run
	if keyword_set(average) then begin
		previd = gapinds[ccgap-1]
		datachunck = xdata[previd:*]
		xmean = median( datachunck[sort(datachunck)] )
		strxmean = strtrim(string(xmean, format='(I4)'),2)
		xyouts, atime[n_elements(atime)-1]-(xrange[1]-xrange[0])*10.d/1440.d, yrange[1] - (yrange[1]-yrange[0])/15. - (1-cpidpos) * (yrange[1]-yrange[0])/15., strxmean, align=1., /data, charsize=charsize*.9
	endif
	; Add min/max/camping beam if reuqested
	if keyword_set(scanbeams) then begin
		loadct, 10
		previd = gapinds[ccgap-1]
		datachunck = (*rad_fit_data[data_index]).beam[previd:*]
		maxbeam = max( datachunck ) 
		minbeam = min( datachunck ) 
		strmaxbeam = strtrim(string(maxbeam, format='(I2)'),2)
		strminbeam = strtrim(string(minbeam, format='(I2)'),2)
		xyouts, atime[n_elements(atime)-1]-(xrange[1]-xrange[0])*10.d/1440.d, maxbeam, strmaxbeam, align=1., /data, charsize=charsize*.9, col=120
		xyouts, atime[n_elements(atime)-1]-(xrange[1]-xrange[0])*10.d/1440.d, minbeam, strminbeam, align=1., /data, charsize=charsize*.9, col=120
		loadct, 0
	endif
endif else begin
	if keyword_set(average) then begin
		; Plot average and deviation of main parameter over the whole time range
		previd = 0
		datachunck = xdata[previd:*]
		xmean = median( datachunck[sort(datachunck)] )
		strxmean = strtrim(string(xmean, format='(I4)'),2)
		xyouts, atime[n_elements(atime)-1]-(xrange[1]-xrange[0])*10.d/1440.d, yrange[1]-(yrange[1]-yrange[0])/15.-(1-cpidpos)*(yrange[1]-yrange[0])/15., strxmean, align=1., /data, charsize=charsize*.9
	endif
	; Add min/max/camping beam if reuqested
	if keyword_set(scanbeams) then begin
		loadct, 10
		previd = 0
		datachunck = (*rad_fit_data[data_index]).beam[previd:*]
		maxbeam = max( datachunck ) 
		minbeam = min( datachunck ) 
		strmaxbeam = strtrim(string(maxbeam, format='(I2)'),2)
		strminbeam = strtrim(string(minbeam, format='(I2)'),2)
		xyouts, atime[n_elements(atime)-1]-(xrange[1]-xrange[0])*10.d/1440.d, maxbeam, strmaxbeam, align=1., /data, charsize=charsize*.9, col=120
		xyouts, atime[n_elements(atime)-1]-(xrange[1]-xrange[0])*10.d/1440.d, minbeam, strminbeam, align=1., /data, charsize=charsize*.9, col=120
		loadct, 0
	endif
endelse
rad_load_colortable, /aj


; ****************************
;- Title
; ****************************
; Add radar name and date
if ~keyword_set(notitle) then begin
	if keyword_set(fitex) then $
		datafmt = 'fitex' $
	else if keyword_set(fitacf) then $
		datafmt = 'fitacf' $
	else if keyword_set(oldfit) then $
		datafmt = 'oldfit' $
	else $
		datafmt = 'no data'
	xyouts, position[0], position[3], $
		network[where(network.code[0] eq radar)].name+' ('+datafmt+') -- '+STRMID(format_juldate(juld),0,11), $
		charsize=charsize, /normal
endif

; If you needed a standalone postscript... then close it
if keyword_set(ps) then $
	ps_close, /no_f


end
