;+
; NAME:
; rad_fit_force_scan_flag
;
; PURPOSE:
; This procedure forces a scan flag based on beam number in the rad_fit_data structure
;
; CATEGORY:
; Diagnostics
;
; CALLING SEQUENCE:
; rad_fit_force_scan_flag, dataindex
;
; INPUTS:
; DATAINDEX: set this to the rad_fit_data structure index in which the data to modify is located
;
; KEYWORD PARAMETERS:
;
; PROCEDURE:
;
; COMMON BLOCKS:
; RAD_DATA_BLK: The common block holding the currently loaded radar data and
; information about that data.
;
; EXAMPLE:
;
; COPYRIGHT:
;
; MODIFICATION HISTORY:
; Written by Sebastien de Larquier, Jun, 21, 2012
;-
pro	rad_fit_force_scan_flag, dataindex, scan_mark=scan_mark

common rad_data_blk

; Make sure the data index is between 0 and the maximum number of indices
if dataindex gt rad_max_radars or dataindex lt 0 then begin
	prinfo, 'Invalid data index', dataindex
	return
endif

; number of records
nrecs = (*rad_fit_info[dataindex]).nrecs

; Temporary arrays
beam = (*rad_fit_data[dataindex]).beam
scan_mark = (*rad_fit_data[dataindex]).scan_mark

; Find where beam switches from max to min
switches = beam[1:nrecs-1] - beam[0:nrecs-2]
inds = where( abs(switches) gt 2, cc )
if cc gt 0 then $
	scan_mark[inds+1] = 1

end