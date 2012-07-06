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
; rad_fit_check_all, date=date
;
; INPUTS:
;
; KEYWORD PARAMETERS:
; DATE: YYYYMMDD for the day you want
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
pro rad_fit_check_all, filename, date=date

script_time = systime(/julian, /utc)

rad_fit_check_datetime_all, date=date, out=filename+'_clock'
rad_fit_check_param_all, 'nave', date=date, /clock, /ave, out=filename+'_nave', scale=[0.,50.]
rad_fit_check_param_all, 'tfreq', date=date, /scanbeams, out=filename+'_tfreq', scale=[8.,18.]
rad_fit_check_param_all, 'noise.search', date=date, out=filename+'_noise.search', scale=[1e0,1e5]
rad_fit_check_param_all, 'noise.sky', date=date, out=filename+'_noise.sky', scale=[1e0,1e5]
rad_fit_check_param_all, 'velocity', date=date, out=filename+'_vel', scale=[-2000,2000.], /beam

print, 'Execution time (s): ',(systime(/julian, /utc) - script_time)*86400.d

end