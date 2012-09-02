;+ 
; NAME: 
; rad_fit_check_schedule
;
; PURPOSE: 
; This procedure reads radar fitacf data for a whole day and check for 
; date/time/integration-time consistency as well as data gaps greater than threshold
; 
; CATEGORY: 
; Diagnostics
; 
; CALLING SEQUENCE:
; rad_fit_check_schedule, date
;
; INPUTS:
; DATE: YYYYMMDD for the day you want
;
; KEYWORD PARAMETERS:
; SCHEDULE: output the schdule for this day as strcture with time, name, and number of elements 
; (i.e., number of schedule changes during the day + 1)
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
pro rad_fit_check_schedule, date, schedule=schedule

; Switch directory to be able to download the scheddule file
cd, current=pwd
cd, '/tmp'

; Directory where scheduling files are stored
swgdir = 'http://vt.superdarn.org/assets/scd/'

; Parse date
parse_date, date, year, month, day

; Create name of monthly schedule file
strmonth = strtrim( year*100L + month, 2)+'.swg'

; Get the file
spawn, 'wget '+swgdir+strmonth, res, err
; Make sure it the download worked
if total( stregex(err, 'OK', /extract) eq 'OK' ) eq 0 then begin
	prinfo, 'The schedule download failed... What did you do? Aborting!'
	; Empty schedule
	schedule = {nsch: 0, $
				time: 0L, $
				name: '' }
	; Swicth back to original directory
	cd, pwd
	return
endif

; Create a big empty temporary structure
tmp_sch = {	nsch: 0L, $
			time: lonarr(200), $
			name: replicate(' ',200) }

; Read the schedule file and find the date
openr, unit, strmonth, /get_lun
; Go through line by line untill it finds a XX:XX start of line
startline = ''		; Buffer to store start of lien characters
stday = 0L
sthour = 0L
eday = 0L
ehour = 0L
scname = ''
while ~eof(unit) do begin
	point_lun, -unit, pos
	readf, unit, startline, format='(a5)'
	match = stregex(startline, '^[0-9][0-9]:[0-9][0-9]')
	if match ne 0 then continue else point_lun, unit, pos

	; So, now you found the first DD:HH line...
	; Let's find the day you are looking for
	readf, unit, stday, sthour, eday, ehour, scname, format='(I2,1x,I2,4x,I2,1x,I2,4x,a)'
; 	print, stday, sthour, eday, ehour, ' : ', scname

	; Now for some wicked logic...
	if day ge stday then begin
		; If your day is not on the current line, on just starts at the end of the current line, then go to the next one
		if (day gt eday) or (day eq eday and ehour eq 00) then continue
		; if your day is on this line, then add 1 to the schedule count and save the name and start time
		if day le eday then begin
			tmp_sch.name[tmp_sch.nsch] = scname
			if day eq stday then $
				tmp_sch.time[tmp_sch.nsch] = sthour $
			else $
				tmp_sch.time[tmp_sch.nsch] = 0
			tmp_sch.nsch += 1L
		endif
	endif else break
; And repeat...
endwhile
; Close the file and remove it
free_lun, unit
spawn, 'rm -f '+strmonth, res, err

; Now make a proper size structure for output
schedule = {nsch: tmp_sch.nsch, $
			time: tmp_sch.time[0:tmp_sch.nsch-1], $
			name: tmp_sch.name[0:tmp_sch.nsch-1] }

; Swicth back to original directory
cd, pwd


end