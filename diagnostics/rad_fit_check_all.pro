;+ 
; NAME: 
; rad_fit_check_all
;
; PURPOSE: 
; This procedure runs all the radar diagnostics tools in one conveninet swoop!
; 
; CATEGORY: 
; Diagnostics
; 
; CALLING SEQUENCE:
; rad_fit_check_all, dates
;
; INPUTS:
;
; KEYWORD PARAMETERS:
; DATES: YYYYMMDD for the day you want (can be an array)
;
; PROCEDURE:
;
; COMMON BLOCKS:
;
; EXAMPLE:
;
; COPYRIGHT:
;
; MODIFICATION HISTORY:
; Written by Sebastien de Larquier, Jul, 3, 2012
;-
pro rad_fit_check_all, dates, outdir=outdir, force=force

; Defaults to active directory
if ~keyword_set(outdir) then $
  outdir = "./"

; Announce start year and month
parse_date, dates[n_elements(dates)-1], year, month, day
print, 'Year: ', year
print, '  Month: ', month

; Loops through provided dates, starting from the end
for id=n_elements(dates)-1,0,-1 do begin
  ; Announce changes in year and month
  if id lt n_elements(dates)-1 then begin
    parse_date, dates[id], year, month, day
    parse_date, dates[id+1], pyear, pmonth, pday
    if year ne pyear then print, 'Year: ', year
    if month ne pmonth then print, '  Month: ', month
  endif

  ; Set filename
  filename = outdir+"RadCheck_"+strtrim(dates[id],2)
  
  ; Skip if output file already exists, unless force keyword is set
  if keyword_set(force) or ~file_test(filename+'_0_clock.pdf') then $
    rad_fit_check_datetime_all, date=dates[id], out=filename+'_0_clock'

  ; Skip if output file already exists, unless force keyword is set
  if keyword_set(force) or ~file_test(filename+'_1_nave.pdf') then $
    rad_fit_check_param_all, 'nave', date=dates[id], /clock, /ave, out=filename+'_1_nave', scale=[0.,50.]

  ; Skip if output file already exists, unless force keyword is set
  if keyword_set(force) or ~file_test(filename+'_2_nave-intt.pdf') then $
    rad_fit_check_param_all, 'nave', date=dates[id], /clock, /ave, out=filename+'_2_nave-intt', scale=[0.,10.], /ratio

  ; Skip if output file already exists, unless force keyword is set
  if keyword_set(force) or ~file_test(filename+'_3_tfreq.pdf') then $
    rad_fit_check_param_all, 'tfreq', date=dates[id], /scanbeams, out=filename+'_3_tfreq', scale=[8.,18.]

  ; Skip if output file already exists, unless force keyword is set
  if keyword_set(force) or ~file_test(filename+'_4_vel.pdf') then $
    rad_fit_check_param_all, 'velocity', date=dates[id], out=filename+'_4_vel', scale=[-2000,2000.], /beam

  ; Skip if output file already exists, unless force keyword is set
  if keyword_set(force) or ~file_test(filename+'_5_echoes.pdf') then $
    rad_fit_check_echoes_all, date=dates[id], /beam,  out=filename+'_5_echoes'

  ; Skip if output file already exists, unless force keyword is set
  if keyword_set(force) or ~file_test(filename+'.png') or ~file_test(filename+'.pdf') then begin
    if file_test(filename+'.pdf') then spawn, 'rm -f '+filename+'.pdf'
    spawn, 'pdftk '+filename+'_*.pdf cat output '+filename+'.pdf'
    if file_test(filename+'.png') then spawn, 'rm -f '+filename+'.png'
    spawn, 'convert -trim -resize 15000x1200 +append +repage '+filename+'_*.pdf '+filename+'.png'
  endif

endfor

end
