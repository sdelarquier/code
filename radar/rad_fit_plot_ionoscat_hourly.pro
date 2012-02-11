pro	rad_fit_plot_ionoscat_hourly, mm, year, ps=ps


common rad_data_blk

Rav = 6370.

if ~(mm ge 1 and mm le 12) then $
	return

; Set time
time=[0,1200]

; Set begining date
shortdate = STRTRIM(string(mm,format='(I02)'))+STRTRIM(year, 2)
date = year*10000L + mm*100 + 1

if keyword_set(ps) then $
	ps_open, '~/Desktop/radscath_'+shortdate+'.ps'

; Set plotting area
xrange = [0., 60.]
yrange = [0., 60.]
title = STRTRIM(string(mm,format='(I02)'))+'/'+STRTRIM(year, 2)+', bks'

; Initialize arrays
nelev_steps = (yrange[1]-yrange[0])/0.1
elev_steps = yrange[0] + findgen(nelev_steps)*(yrange[1]-yrange[0])/nelev_steps
nhours = (time[1]-time[0])/100L
nbeams = 16
elevhist = fltarr(nbeams,nhours,nelev_steps,100)

; Set plot area to mark days with available data
ndays = days_in_month(mm, year=date/10000L)
plot, [0,1], [1,ndays], /nodata, xstyle=1, ystyle=1, $
	xtickname=replicate(' ', 60), ytickname=replicate(' ', 60), $
	xticks=1, yticks=ndays[0], yticklen=1, $
	position=[.95,.1,.97,.9]
axis, yax=1, yrange=[1,ndays], ystyle=1, yticks=1

; loop through days
for id=0,ndays[0]-1 do begin
	; Read radar data
	rad_fit_read, date, 'bks', time=time, /filter, /ajground, /catfile, catpath='/tmp/'
	parse_date, date, year, month, day

	; get index for current data
	data_index = rad_fit_get_data_index()
	if data_index eq -1 then $
		continue

	rad_fit_calculate_elevation, date=date, time=time, /overwrite, tdiff=-.324, interfer_pos=[0.,-58.9,-2.7], scan_boresite_offset=8.

	; Set time limits
	sfjul, date, time, sjul, fjul
	
	televhist = fltarr(nbeams,nhours,nelev_steps,100)
	ndev = 0L
	for ib=0,nbeams-1 do begin
		ttelevhist = fltarr(nhours,nelev_steps,100)
		nbev = 0L
		for it=0,nhours-1 do begin
			sjul = julday(month, day, year, time[0]/100L+it, 0)
			fjul = julday(month, day, year, time[0]/100L+(it+1), 0)
			binds = where((*rad_fit_data[data_index]).beam eq ib and $
						(*rad_fit_data[data_index]).juls ge sjul and $
						(*rad_fit_data[data_index]).juls le fjul and $
						(*rad_fit_data[data_index]).tfreq ge 10e3 and $
						(*rad_fit_data[data_index]).tfreq le 12e3, ccinds)
			if ccinds le 0 then $
				continue

			elev = (*rad_fit_data[data_index]).elevation[binds,*]
	; 		elev = (*rad_fit_data[data_index]).phi0[binds,*]
			power = (*rad_fit_data[data_index]).power[binds,*]
			scat = (*rad_fit_data[data_index]).gscatter[binds,*]
			tttelevhist = fltarr(nelev_steps,100)
			nev = 0L
			for ng=5,(*rad_fit_info[data_index]).ngates-1 do begin
				if ng lt xrange[1] then begin
					for nel=0,nelev_steps-2 do begin
						elinds = where(elev[*,ng] ge elev_steps[nel] and elev[*,ng] lt elev_steps[nel+1] and $
								(scat[*,ng] eq 0b or scat[*,ng] eq 3b), ccel)
						nev = nev + ccel
						if ccel gt 0. then $
							tttelevhist[nel,ng] = tttelevhist[nel,ng] + TOTAL(power[elinds,ng])
					endfor
				endif
			endfor
			

			if nev gt 0. then $
				ttelevhist[it,*,*] = ttelevhist[it,*,*] + tttelevhist/max(tttelevhist)
			nbev = nbev + nev
; 			print, date, ib, it, nev
		endfor

		if nbev gt 0. then $
			televhist[ib,*,*,*] = televhist[ib,*,*,*] + ttelevhist/max(ttelevhist)
		ndev = ndev + nbev
	endfor
	
	if ndev gt 0. then begin
		elevhist = elevhist + televhist/max(televhist)
		polyfill, [0.,0.,1.,1.], 1.+id+[0.,1.,1.,0.], col=0
	endif
	date = date + 1
endfor

; Scale elevation distribution
maxdist = fltarr(nbeams, nhours)
for ib=0,nbeams-1 do begin
	for it=0,nhours-1 do $
		maxdist[ib,it] = max(median(reform(elevhist[ib,it,*,*]),5))
	elevhist[ib,*,*,*] = elevhist[ib,*,*,*]/max(maxdist[ib,*])
endfor
elevhist = elevhist/max(maxdist)
; print, max(elevhist, /nan)

; Plot
for ib=0,nbeams-1 do begin
	!X.MARGIN = [4,3]	
	!X.OMARGIN = [0,10]
	for it=0,nhours-1 do begin
		!P.MULTI = [nhours-it,3,4]
		plot, xrange, yrange, /nodata, xstyle=1, ystyle=1, $
			xtitle='Slant range [45 km]', ytitle='Elevation', $
			title=title+', Beam '+strtrim(ib,2)+', Time '+strtrim(time[0]/100L+it,2)+':00-'+strtrim(time[0]/100L+(it+1),2)+':00'

		for ng=0,(*rad_fit_info[data_index]).ngates-1 do begin
			if ng lt xrange[1] then begin
				for nel=0,nelev_steps-2 do begin
					if elevhist[ib,it,nel,ng] gt 0. then begin
						col = bytscl(elevhist[ib,it,nel,ng], min=0., max=1., top=250) + 3b

						polyfill, ng+[0,0,1,1], [elev_steps[nel],elev_steps[nel+1],elev_steps[nel+1],elev_steps[nel]], color=col
					endif
				endfor
			endif
		endfor

		phielev = findgen(60)
		hs = 200.
		ran = sqrt((Rav+hs)^2 - Rav^2*cos(phielev*!dtor)^2) - Rav*sin(phielev*!dtor)
		oplot, (ran-180.)/45., phielev, thick=2

		hs = 300.
		ran = sqrt((Rav+hs)^2 - Rav^2*cos(phielev*!dtor)^2) - Rav*sin(phielev*!dtor)
		oplot, (ran-180.)/45., phielev, thick=2

		hs = 400.
		ran = sqrt((Rav+hs)^2 - Rav^2*cos(phielev*!dtor)^2) - Rav*sin(phielev*!dtor)
		oplot, (ran-180.)/45., phielev, thick=2
	endfor
	!P.MULTI = 0
	if ib lt nbeams-1 then $
		clear_page
endfor

if keyword_set(ps) then $
	ps_close, /no_filename

end