;+ 
; NAME: 
; rad_fit_check_datetime
;
; PURPOSE: 
; This procedure reads radar fitacf data for a whole day and check for 
; date/time/integration-time consistency as well as data gaps greater than threshold
; 
; CATEGORY: 
; Diagnostics
; 
; CALLING SEQUENCE:
; rad_fit_check_datetime, radar, date=date, threshold=threshold
;
; INPUTS:
; Radar: Set this to a 3-letter radar code to indicate the radar for which to read
; data.
;
; KEYWORD PARAMETERS:
; Date: The date of which to read data. Can be a scalar in YYYYMMDD format or
; a 2-element vector in YYYYMMDD format. Defaults to yesterday.
; 
; Threshold: time in minutes defining the minimum length of a flagged data gap
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
; Written by Sebastien de Larquier, Jun, 21, 2012
;-
pro rad_fit_check_datetime, radar, date=date, time=time, $
	oldfit=oldfit, fitacf=fitacf, fitex=fitex, $
	threshold=threshold, ps=ps, $
	position=position, charsize=charsize, $
	noxtitle=noxtitle, notitle=notitle

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

; If you need a standalone postscript...
if keyword_set(ps) then begin
	if size(ps,/type) eq 7 then $
		filename = ps $
	else $
		filename = '~/Desktop/check_clock_'+strtrim(tdate,2)+'_'+radar
	ps_open, filename+'.ps'
endif

; Adjust position
if ~keyword_set(position) then $
	position = define_panel(1,1, /no_title)
position[2] -= .05 

; Set ticks length
yticklen = -.02
xticklen = -.02

; Read file in required format and populate structures
rad_fit_read, time=time, tdate, radar, oldfit=oldfit, fitacf=fitacf, fitex=fitex

; Get data index
data_index = rad_fit_get_data_index()
if data_index ne -1 then begin
    if (*rad_fit_info[data_index]).code ne radar then data_index = -1
endif
if data_index eq -1 then begin
	; If no data, draw empty plotplot, xrange, yrange, /nodata, $
	plot, [0,1], [juld, juld+1.d], /nodata, $
		xstyle=1, ystyle=9, position=position, charsize=charsize, $
		xtitle='', xtickname=replicate(' ', 60), xticks=1, $
		yticklen=yticklen, ytitle=strupcase(radar)+' - Time [UT]', ytickformat='label_date', yticks=4, ytickv=juld+dindgen(5)*6.d/24.d, yminor=6
	; Add y plain axis
	axis, /yaxis, /data, ystyle=1, yrange=[0,1], yticks=1, ytickname=replicate(' ', 60)
	; Add x axis title (the default one is too far)
	if ~keyword_set(noxtitle) then $
		xyouts, .5, juld- .1d, 'Record #', align=.5, charsize=charsize
	; Add an unambiguous statement about data availability for this radar on that day...
	xyouts, .5, juld+.5d, 'NO DATA', charsize=charsize, align=.5
	return
endif

; Get number of records
nrecs = (*rad_fit_info[data_index]).nrecs

; Get character size
if ~keyword_set(charsize) then $
	charsize = get_charsize(1,1)

; Calculate plot limits
xrange = [0, nrecs]
yrange = [juld, juld+1.d]

; Set up plotting area
plot, xrange, yrange, /nodata, $
	yticklen=yticklen, xstyle=1, ystyle=9, position=position, charsize=charsize, $
	xtitle='', xtickname=replicate(' ', 60), xticks=1, $
	ytitle=strupcase(radar)+' - Time [UT]', ytickformat='label_date', yticks=4, ytickv=juld+dindgen(5)*6.d/24.d, yminor=6
; Add x axis title (the default one is too far)
if ~keyword_set(noxtitle) then $
	xyouts, (xrange[1]-xrange[0])/2., yrange[0]- (yrange[1]-yrange[0])*.1, 'Record #', align=.5, charsize=charsize, /data
; Add number of records
xyouts, xrange[1], yrange[0]- (yrange[1]-yrange[0])*.05, strtrim(nrecs,2), align=.5, charsize=charsize*.8, /data


; Scatter time values on plot
oplot, (*rad_fit_data[data_index]).juls, psym=3


; If no time threshold is passed, default to 10 times the max integration time
if ~keyword_set(threshold) then $
	julthresh = 10.*max((*rad_fit_data[data_index]).intt)/60.d/1440.d $
else $
	julthresh = threshold/1440.d


; ****************************
;- Gaps
; ****************************
; Find time jumps
timediff = (*rad_fit_data[data_index]).juls[1:nrecs-1] - (*rad_fit_data[data_index]).juls[0:nrecs-2]
gapinds = where(timediff gt julthresh, ccgap)
; If gaps are found, then mark and annotate them (in red)
if ccgap gt 0 then begin
	for igap=0,ccgap-1 do begin
		oplot, gapinds[igap]*[1,1], yrange, col=250, linestyle=3
		caldat, (*rad_fit_data[data_index]).juls[gapinds[igap]], month, day, year, hour, minute, sec
		xyouts, gapinds[igap], yrange[1], $
			strtrim(string(timediff[gapinds[igap]]*1440.d, format='(f10.1)'))+'min @ '+ $
				strtrim(string(hour,format='(I02)'),2)+':'+strtrim(string(minute,format='(I02)'),2)+':'+strtrim(string(sec,format='(I02)'),2)+' UT', $
			orientation=90., align=1., /data, col=250, charsize=charsize*.8
	endfor
endif
; Write threshold on plot
xyouts, xrange[0], yrange[0]- (yrange[1]-yrange[0])*.05, 'gaps > '+strtrim(string(julthresh*1440.d,format='(f10.1)'),2)+' min', align=.5, charsize=charsize*.8, /data, col=250
; Add a bottom page note to explain
xyouts, .5, .05, 'Note on gaps: a gap is marked when two consecutive records are more than 10 integration times apart.', align=.5, col=250, /normal, charsize=charsize*.8


; ****************************
;- Integration/Scan time
; ****************************
; Calculate scan times
scaninds = where( (*rad_fit_data[data_index]).scan_mark eq 1 , ccscan)
; If no scan flags, then force it
if ccscan le 1 then begin
	rad_fit_force_scan_flag, data_index, scan_mark=scan_mark
	scaninds = where( scan_mark eq 1 , ccscan)
	force_flag = '(forced)'
endif else force_flag = ''
if ccscan gt 1 then begin
	scantime = fltarr(ccscan-1)
	scanrec = fltarr(ccscan-1)
	for iscan=0,ccscan-2 do begin
		scantime[iscan] = ( (*rad_fit_data[data_index]).juls[scaninds[iscan+1]-1] + (*rad_fit_data[data_index]).intt[scaninds[iscan+1]-1]/86400.d - (*rad_fit_data[data_index]).juls[scaninds[iscan]] )*1440.d
		scanrec[iscan] = scaninds[iscan]
	endfor
endif else begin
	scantime = 0
	scanrec = 0
endelse
; Add an integration/scan time time axis
intrange = [0., 18.]
axis, /yaxis, /data, ystyle=1, yticklen=yticklen, yrange=intrange, ytitle='', charsize=charsize, $
	ytickv=indgen(4)*3., yticks=4, yminor=3
; Add axis titles
loadct, 10
xyouts, position[2]+.07, position[1]+(position[3]-position[1])/2., 'Integration time [s]', col=120, align=.5, orientation=90., /normal, charsize=charsize
xyouts, position[2]+.1, position[1]+(position[3]-position[1])/2., 'Scan time [min]'+force_flag, col=50, align=.5, orientation=90., /normal, charsize=charsize
; Plot scan time (in green)
load_usersym, /circle
plots, scanrec, yrange[0] + (yrange[1]-yrange[0])/intrange[1]*scantime, col=50, psym=8, symsize=.1;, linestyle=2
; Plot integration time (in blue)
oplot, yrange[0] + (yrange[1]-yrange[0])/intrange[1]*(*rad_fit_data[data_index]).intt, col=120


; ****************************
;- CPID
; ****************************
; Mark initial cpid
loadct, 0
xyouts, xrange[0]+(xrange[1]-xrange[0])/100., yrange[1]-2.d/24.d, $
	rad_cpid_translate((*rad_fit_data[data_index]).scan_id[0])+' ('+strtrim((*rad_fit_data[data_index]).scan_id[0], 2)+')', $
	align=0., /data, col=100, charsize=charsize*.7
; Mark cpid changes
iddiff = (*rad_fit_data[data_index]).scan_id[1:nrecs-1] - (*rad_fit_data[data_index]).scan_id[0:nrecs-2]
gapinds = where(iddiff ne 0, ccgap)
; If cpid changes, then mark and annotate it (in gray)
offset = 1
if ccgap gt 0 then begin
	step = 1
	if ccgap gt 24 then step = round(ccgap/10)
	igap = 0L
	while igap lt ccgap do begin
		oplot, gapinds[igap]*[1,1], yrange, col=100, linestyle=2
		xyouts, gapinds[igap]+(xrange[1]-xrange[0])/100., yrange[1] - (yrange[1]-yrange[0])*2.d/24.d - offset*(yrange[1]-yrange[0])*2.d/24.d, $
			rad_cpid_translate((*rad_fit_data[data_index]).scan_id[gapinds[igap]+1])+' ('+strtrim((*rad_fit_data[data_index]).scan_id[gapinds[igap]+1], 2)+')', $
			align=0., /data, col=100, charsize=charsize*.7
		offset = 1 - offset
		igap += step
	endwhile
endif
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
	xyouts, xrange[0], yrange[1], $
		network[where(network.code[0] eq radar)].name+' ('+datafmt+') -- '+STRMID(format_juldate(juld),0,11), $
		charsize=charsize
endif

; If you needed a standalone postscript... then close it
if keyword_set(ps) then $
	ps_close, /no_f

end
