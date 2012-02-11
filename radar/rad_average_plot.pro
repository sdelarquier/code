pro	rad_average_plot

nd = 0L
date = [0L,0L]
openr, unit, './rad_average.dat', /get_lun
readu, unit, nd, date
hist = dindgen(60,nd)
readu, unit, hist
free_lun, unit

; Generate time array for x-axis
parse_date, date[0], syear, smonth, sday
parse_date, date[1], fyear, fmonth, fday
if (fyear-syear) gt 0L then begin
	years = indgen(fyear-syear+1) + syear 
	if (fmonth-smonth) gt 0L then begin
		months = indgen(fmonth-smonth+1) + syear 
	endif
endif else $
	years = syear
if (fmonth-smonth) gt 0L then $
	months = indgen(fmonth-smonth+1) + syear $
else $
	months = smonth

juls = JULDAY(months,days,years)
help, juls, months, days, years
stop

yrange = [0,59]
xrange = [juls[0], juls[nd]]
position = [.1,.1,.9,.9]

; set up coordinate system for plot
plot, [0,0], /nodata, xstyle=5, ystyle=5, $
	yrange=yrange, xrange=xrange, position=position

; Set color scale parameters
scale = [0,30]
color_steps = 3
ncolors = 256
bottom = 0.
cin = FIX(FINDGEN(color_steps)/(color_steps-1.)*(ncolors-1))+bottom
lvl = scale[0]+FINDGEN(color_steps)*(scale[1]-scale[0])/color_steps

; Begin cycling through days
for ns=0,nd do begin
	start_day = juls[ns]
	end_day = juls[ns+1]
	; Begin cycle through range gates
	for r=0,59 do begin
		IF hist[r,ns] ne 10000. THEN BEGIN
			color_ind = (MAX(where(lvl le ((hist[r,ns] > scale[0]) < scale[1]))) > 0)
			col = cin[color_ind]
			
			; finally plot the point
			POLYFILL,[start_day,start_day,end_day,end_day], $
					[r,r], [r+1,r], $
					COL=col,NOCLIP=0
		ENDIF
		
	; End cycle through range gates 
	endfor
; End of cycling through days
endfor

end