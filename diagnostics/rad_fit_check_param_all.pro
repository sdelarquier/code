;+
; NAME:
; rad_fit_check_param_all
;
; PURPOSE:
; This procedure reads 10 radars fitacf data for a whole day and plots a given parameter
; as a function of time
;
; CATEGORY:
; Diagnostics
;
; CALLING SEQUENCE:
; rad_fit_check_param_all, date=date
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
pro rad_fit_check_param_all, param, date=date, time=time, $
	outfile=outfile, clock=clock, scale=scale, beam=beam, $
	average=average, scanbeams=scanbeams, pagetitle=pagetitle, ratio=ratio

; Declare mid-latitude radars
midlat = ['cvw', 'cve', 'fhw', 'fhe', 'bks', 'wal']
midlatbeams = [12,12,10,10,13,07]
; Declare high-latitude radars
highlat = ['kod','pgr','sas', 'kap', 'gbr', 'sto']
highlatbeams = [07,07,07,07,07,07]

; For now, default data format is fitacf
datafmt='fitacf'

if ~keyword_set(pagetitle) then begin
	case param of
		'nave': pagetitle = 'Timing diagnostics (vs UT)'
		'tfreq': pagetitle = 'Frequency/Beam diagnostics (vs UT)'
		'velocity': pagetitle = 'Velocity scatter plot'
		else: pagetitle = 'Parameter diagnostics'
	endcase
endif

if ~keyword_set(time) then $
	time = [0,2400]

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
if keyword_set(ratio) then fileparam = 'nave-intt' else fileparam = param
set_format, /portrait, /tokyo, /sardines
if ~keyword_set(outfile) then $
	outfile = '~/Desktop/check_'+fileparam+'_'+strtrim(date,2)
filename = outfile
ps_open, filename+'.ps'

; Plot high latitude radar diagnostics
clear_page
noxlabels = 1
; Add date at top of page
xyouts, .5, .95, $
	'High latitude radars ('+datafmt+') - '+STRMID(format_juldate(juld),0,11), $
	charsize=get_charsize(1,2), align=.5, /normal
; Page title (type of plots)
xyouts, .5, .98, $
	pagetitle, $
	charsize=get_charsize(1,2), align=.5, /normal
; Plot stuff
for irad=0,n_elements(highlat)-1 do begin
	if irad eq n_elements(highlat)-1 then noxlabels = 0
	if keyword_set(beam) then abeam = highlatbeams else abeam = 0*highlatbeams
	; Calls rad_fit_check_param for each radar, and only plots an x-axis and schedule for the bottom one
	rad_fit_check_param, param, highlat[irad], date=tdate, time=time, scanbeams=scanbeams, $
		/fitacf, noxlabels=noxlabels, /notitle, beam=abeam[irad], schedule=(1-noxlabels), $
		position=define_panel(1,n_elements(highlat), 0, irad, /no_title), ratio=ratio, $
		charsize=get_charsize(1,n_elements(highlat)), clock=clock, scale=scale, average=average
endfor


; Plot mid latitude radar diagnostics
clear_page
noxlabels = 1
; Add date at top of page
xyouts, .5, .95, $
	'Mid latitude radars ('+datafmt+') - '+STRMID(format_juldate(juld),0,11), $
	charsize=get_charsize(1,2), align=.5, /normal
; Page title (type of plots)
xyouts, .5, .98, $
	pagetitle, $
	charsize=get_charsize(1,2), align=.5, /normal
; Plot stuff
for irad=0,n_elements(midlat)-1 do begin
	if irad eq n_elements(midlat)-1 then noxlabels = 0
	if keyword_set(beam) then abeam = midlatbeams else abeam = 0*midlatbeams
	; Calls rad_fit_check_param for each radar, and only plots an x-axis and schedule for the bottom one
	rad_fit_check_param, param, midlat[irad], date=tdate, time=time, scanbeams=scanbeams, $
		/fitacf, noxlabels=noxlabels, /notitle, beam=abeam[irad], schedule=(1-noxlabels), $
		position=define_panel(1,n_elements(midlat), 0, irad, /no_title), ratio=ratio, $
		charsize=get_charsize(1,n_elements(highlat)), clock=clock, scale=scale, average=average
endfor


ps_close, /no_f
spawn, 'ps2png.sh '+filename+'.ps'
spawn, 'convert -units PixelsPerInch -extent 2300x3200 -density 96 '+filename+'*.png '+filename+'.pdf'
spawn, 'rm -f '+filename+'.ps'
spawn, 'rm -f '+filename+'*.png'

end
