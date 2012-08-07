;+ 
; NAME: 
; rad_fit_check_datetime_all
;
; PURPOSE: 
; This procedure reads 10 radars fitacf data for a whole day and check for 
; date/time/integration-time consistency as well as data gaps greater than threshold
; 
; CATEGORY: 
; Diagnostics
; 
; CALLING SEQUENCE:
; rad_fit_check_datetime_all, date=date, threshold=threshold
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
pro rad_fit_check_datetime_all, date=date, $
	threshold=threshold, outfile=outfile

; Declare mid-latitude radars
midlat = ['cvw', 'cve', 'fhw', 'fhe', 'bks', 'wal']
; Declare high-latitude radars
highlat = ['sas', 'kap', 'gbr', 'sto']

; For now, default data format is fitacf
datafmt='fitacf'

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

; Open postscript for output
set_format, /portrait, /tokyo, /sardines
if ~keyword_set(outfile) then $
	outfile = '~/Desktop/check_datetime_'+strtrim(date,2)
filename = outfile
ps_open, filename+'.ps'

; Plot high latitude radar diagnostics
clear_page
noxtitle = 1
; Add date at top of page
xyouts, .5, .95, $
	'High latitude radars ('+datafmt+') - '+STRMID(format_juldate(juld),0,11), $
	charsize=get_charsize(1,2), align=.5, /normal
xyouts, .5, .98, $
	'Clock diagnostics', $
	charsize=get_charsize(1,2), align=.5, /normal
for irad=0,n_elements(highlat)-1 do begin
	if irad eq n_elements(highlat)-1 then noxtitle = 0
	rad_fit_check_datetime, highlat[irad], date=tdate, $
		/fitacf, noxtitle=noxtitle, /notitle, $
		threshold=threshold, position=define_panel(1,n_elements(highlat), 0, irad, /no_title), $
		charsize=get_charsize(1,n_elements(highlat))

endfor

; Plot mid latitude radar diagnostics
clear_page
noxtitle = 1
; Add date at top of page
xyouts, .5, .95, $
	'Mid latitude radars ('+datafmt+') - '+STRMID(format_juldate(juld),0,11), $
	charsize=get_charsize(1,2), align=.5, /normal
xyouts, .5, .98, $
	'Clock diagnostics', $
	charsize=get_charsize(1,2), align=.5, /normal
for irad=0,n_elements(midlat)-1 do begin
	if irad eq n_elements(midlat)-1 then noxtitle = 0
	rad_fit_check_datetime, midlat[irad], date=tdate, $
		/fitacf, noxtitle=noxtitle, /notitle, $
		threshold=threshold, position=define_panel(1,n_elements(midlat), 0, irad, /no_title), $
		charsize=get_charsize(1,n_elements(highlat))

endfor

ps_close, /no_f
spawn, 'ps2pdf '+filename+'.ps '+filename+'.pdf'
spawn, 'rm -f '+filename+'.ps'

end