;+
; NAME:
; rad_fit_check_calc_echoes
;
; PURPOSE:
; This procedure reads radar fitacf data for a whole day and calculates the number
; of ionospheric (black) and groundscatter (gray) echoes as a function of time.
;
; CATEGORY:
; Diagnostics
;
; CALLING SEQUENCE:
; rad_fit_check_echoes, radar, date=date
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
; Modified by Evan Thomas, Jul, 05, 2012
;-
pro rad_fit_check_calc_echoes, beam=beam, $
	data_index=data_index, setbeam=setbeam, $
	array=array, juls=juls, cols=cols

common radarinfo
common rad_data_blk

; Set parameters
nscans = (*rad_fit_info[data_index]).nscans
ngates = (*rad_fit_info[data_index]).ngates
juls = (*rad_fit_data[data_index]).juls
velocity = (*rad_fit_data[data_index]).velocity
gflag = (*rad_fit_data[data_index]).gscatter
scan_mark = (*rad_fit_data[data_index]).scan_mark
intt = (*rad_fit_data[data_index]).intt

; ****************************
;- Parameter
; ****************************
sjuls = dblarr(nscans)
fjuls = dblarr(nscans)
gs = fltarr(nscans)
is = fltarr(nscans)

; Loop through each scan
for ii=0,nscans-1 do begin
	gs_scan_sum = 0
	is_scan_sum = 0

	; Find the scan ids pertaining to the particular scan number
	if setbeam eq 1 then $
		scan_inds = where((*rad_fit_data[data_index]).beam_scan eq ii and $
			(*rad_fit_data[data_index]).beam eq beam, nscan_inds) $
	else $
		scan_inds = where((*rad_fit_data[data_index]).beam_scan eq ii, nscan_inds)

	; Check for data within the scan number
	if nscan_inds eq 0 then begin
		sjuls[ii] = !VALUES.F_NAN
		fjuls[ii] = !VALUES.F_NAN
		is[ii] = !VALUES.F_NAN
		gs[ii] = !VALUES.F_NAN
		continue
	endif

	; Find start and stop time of scan
	sjuls[ii] = juls[scan_inds[0]]
	fjuls[ii] = juls[scan_inds[nscan_inds-1]] + intt[scan_inds[nscan_inds-1]]/86400.

	; Loop through the individual scan
	for nn=0,nscan_inds-1 do begin
		; Loop through each range gate
		for gg=0,ngates-1 do begin
			; Check whether scatter is real and if from ionosphere or ground
			if gflag[scan_inds[nn],gg] eq 1 and velocity[scan_inds[nn],gg] lt 10000. then $
				gs_scan_sum = gs_scan_sum+1 $
			else if gflag[scan_inds[nn],gg] eq 0 and velocity[scan_inds[nn],gg] lt 10000. then $
				is_scan_sum = is_scan_sum+1
		endfor
	endfor

	gs[ii] = gs[ii]+gs_scan_sum/nscan_inds
	is[ii] = is[ii]+is_scan_sum/nscan_inds
endfor

array = fltarr(2,nscans)
juls = dblarr(2,nscans)
cols = intarr(2)

array[0,*] = is
array[1,*] = gs

juls[0,*] = sjuls
juls[1,*] = fjuls

cols[0] = get_black()
cols[1] = get_gray()

end
