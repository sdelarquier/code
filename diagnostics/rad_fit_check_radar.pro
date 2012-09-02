;+ 
; NAME: 
; rad_fit_check_radar
;
; PURPOSE: 
; This procedure runs all the radar diagnostics tools in one conveninet swoop!
; 
; CATEGORY: 
; Diagnostics
; 
; CALLING SEQUENCE:
; rad_fit_check_radar, dates, radar
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
pro rad_fit_check_radar, dates, radar, outdir=outdir, force=force, beam=beam, threshold=threshold

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
  filename = outdir+"RadCheck_"+radar+"_"+strtrim(dates[id],2)
  
  ; Skip if output file already exists, unless force keyword is set
  if keyword_set(force) or ~file_test(filename+'_0_clock.pdf') then begin
    ending = '_0_clock'
    rad_fit_check_datetime, radar, date=dates[id], /fitacf, ps=filename+ending, threshold=threshold
    spawn, 'ps2pdf '+filename+ending+'.ps '+filename+ending+'.pdf'
    spawn, 'rm '+filename+ending+'.ps'
  endif

  ; Skip if output file already exists, unless force keyword is set
  if keyword_set(force) or ~file_test(filename+'_1_nave.pdf') then begin
    ending = '_1_nave'
    rad_fit_check_param, 'nave', radar, date=dates[id], /clock, /ave, ps=filename+ending, scale=[0.,50.]
    spawn, 'ps2pdf '+filename+ending+'.ps '+filename+ending+'.pdf'
    spawn, 'rm '+filename+ending+'.ps'
  endif

  ; Skip if output file already exists, unless force keyword is set
  if keyword_set(force) or ~file_test(filename+'_2_nave-intt.pdf') then begin
    ending = '_2_nave-intt'
    rad_fit_check_param, 'nave', radar, date=dates[id], /clock, /ave, ps=filename+ending, scale=[0.,10.], /ratio
    spawn, 'ps2pdf '+filename+ending+'.ps '+filename+ending+'.pdf'
    spawn, 'rm '+filename+ending+'.ps'
  endif

  ; Skip if output file already exists, unless force keyword is set
  if keyword_set(force) or ~file_test(filename+'_3_tfreq.pdf') then begin
    ending = '_3_tfreq'
    rad_fit_check_param, 'tfreq', radar, date=dates[id], /scanbeams, ps=filename+ending, scale=[8.,18.]
    spawn, 'ps2pdf '+filename+ending+'.ps '+filename+ending+'.pdf'
    spawn, 'rm '+filename+ending+'.ps'
  endif

  ; Skip if output file already exists, unless force keyword is set
  if keyword_set(force) or ~file_test(filename+'_4_vel.pdf') then begin
    ending = '_4_vel'
    rad_fit_check_param, 'velocity', radar, date=dates[id], ps=filename+ending, scale=[-2000,2000.], beam=beam
    spawn, 'ps2pdf '+filename+ending+'.ps '+filename+ending+'.pdf'
    spawn, 'rm '+filename+ending+'.ps'
  endif

  ; Skip if output file already exists, unless force keyword is set
  if keyword_set(force) or ~file_test(filename+'_5_echoes.pdf') then begin
    ending = '_5_echoes'
    rad_fit_check_echoes, radar, date=dates[id], beam=beam,  ps=filename+ending
    spawn, 'ps2pdf '+filename+ending+'.ps '+filename+ending+'.pdf'
    spawn, 'rm '+filename+ending+'.ps'
  endif

  ; Skip if output file already exists, unless force keyword is set
  if keyword_set(force) or ~file_test(filename+'.png') or ~file_test(filename+'.pdf') then begin
    if file_test(filename+'.pdf') then spawn, 'rm -f '+filename+'.pdf'
    spawn, 'pdftk '+filename+'_*.pdf cat output '+filename+'.pdf'
    spawn, 'rm '+filename+'_*.pdf'
  endif

endfor

end
