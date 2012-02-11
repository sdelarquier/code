pro	rad_fit_plot_ionoscat2, mm, year, ps=ps


common rad_data_blk

Rav = 6370.

if ~(mm ge 1 and mm le 12) then $
	return

ib = 8

; Set time
time=[0,1200]

; Set begining date
shortdate = STRTRIM(string(mm,format='(I02)'))+STRTRIM(year, 2)
date = year*10000L + mm*100 + 15

if keyword_set(ps) then $
	ps_open, '~/Desktop/radscat2_'+shortdate+'.ps'

; Set plotting area
xrange = [0., 60.]
yrange = 2.*[-!pi, !pi]
title = STRTRIM(string(mm,format='(I02)'))+'/'+STRTRIM(year, 2)+', bks'
plot, xrange, yrange, /nodata, xstyle=1, ystyle=1, $
	xtitle='Slant range [45 km]', ytitle='Elevation', $
	title=title+', beam '+strtrim(ib,2)
oplot, [0,60],[!pi,!pi]
oplot, [0,60],-[!pi,!pi]

; Initialize arrays
nelev_steps = (yrange[1]-yrange[0])/0.1
elev_steps = yrange[0] + findgen(nelev_steps)*(yrange[1]-yrange[0])/nelev_steps
elevhist = fltarr(16,nelev_steps,100)

; loop through days
ndays = days_in_month(mm, year=date/10000L)
for id=15,ndays[0]-1 do begin
	; Read radar data
	rad_fit_read, date, 'bks', time=time;, /filter, /ajground, /catfile, catpath='/tmp/'

	; get index for current data
	data_index = rad_fit_get_data_index()
	if data_index eq -1 then $
		stop

	rad_fit_calculate_elevation, date=date, time=time, /overwrite, tdiff=-.324, interfer_pos=[0.,-58.9,-2.7], scan_boresite_offset=8.

	; Set time limits
	sfjul, date, time, sjul, fjul

	for ib=0,15 do begin
		binds = where((*rad_fit_data[data_index]).beam eq ib and $
					(*rad_fit_data[data_index]).juls ge sjul and $
					(*rad_fit_data[data_index]).juls le fjul and $
					(*rad_fit_data[data_index]).tfreq ge 10e3 and $
					(*rad_fit_data[data_index]).tfreq le 12e3, ccinds)
		if ccinds le 0 then $
			continue

; 		elev = (*rad_fit_data[data_index]).elevation[binds,*]
		elev = (*rad_fit_data[data_index]).phi0[binds,*]
		power = (*rad_fit_data[data_index]).power[binds,*]
		scat = (*rad_fit_data[data_index]).gscatter[binds,*]
		televhist = fltarr(nelev_steps,100)
		nev = 0L
		for ng=5,(*rad_fit_info[data_index]).ngates-1 do begin
			if ng lt xrange[1] then begin
				for nel=0,nelev_steps-2 do begin
					elinds = where(elev[*,ng] ge elev_steps[nel] and elev[*,ng] lt elev_steps[nel+1] and power[*,ng] gt 6., ccel)
					nev = nev + ccel
					if ccel gt 0. then $
						oplot, ng*(1+fltarr(ccel)), elev[elinds,ng], psym=3
; 						televhist[nel,ng] = televhist[nel,ng] + TOTAL(power[elinds,ng])
				endfor
			endif
		endfor
		

		if nev gt 0. then begin
			elevhist[ib,*,*] = elevhist[ib,*,*] + televhist/max(televhist)
; 			print, max(televhist), max(elevhist[ib,*,*]), max(elevhist)
			polyfill, [0.,0.,1.,1.], 1.+id+[0.,1.,1.,0.], col=0
		endif
	endfor
	
	date = date + 1
endfor


if keyword_set(ps) then $
	ps_close, /no_filename

end