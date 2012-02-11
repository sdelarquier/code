pro rt_plot_phase_season

common rt_data_blk
common radarinfo
common rad_data_blk

Rav = 6370.

radar = 'bks'
year = 2010

; Set plot limits
xrange = [0.,40.]*45.
yrange = [5.,55.]
charsize = get_charsize(3,2)*.5

; Set elevation range
elrange = [5.,55.]
elstep = 0.5
nelev_steps = (elrange[1]-elrange[0])/elstep
elev_steps = elrange[0] + findgen(nelev_steps)*(elrange[1]-elrange[0])/nelev_steps
phase_steps = -!pi + findgen(nelev_steps)*2.*!pi/float(nelev_steps)
dphi = phase_steps[1]-phase_steps[0]

; Read aspect angle from file
openr, unit, '~/tmp/rt/'+radar+'/aspect.dat', /get_lun
aspnbeams = 0L
aspnranges = 0L
aspnelevs = 0L
readu, unit, aspnbeams, aspnranges, aspnelevs
aspabeams = intarr(aspnbeams)
asparanges = fltarr(aspnranges)
aspaelev = fltarr(aspnelevs)
readu, unit, aspabeams, asparanges, aspaelev
aspect = fltarr(aspnbeams,aspnranges,aspnelevs)
readu, unit, aspect
free_lun, unit

; Open postscript
ps_open, '~/Desktop/elevation_'+radar+'_'+strtrim(year*100L,2)+'.ps'
set_format, /landscape, /sardines, /tokyo

; Seasons
start_date = year*10000L + 1101
nmonths = 8
for im=0,nmonths-1 do begin
	clear_page

	print, calc_date(start_date,im, unit='month'), calc_date(start_date,im+1, unit='month')
	parse_date, calc_date(start_date,im, unit='month'), syear, smonth, sday
	parse_date, calc_date(start_date,im+1, unit='month'), fyear, fmonth, fday
	juls = timegen(start=julday(smonth,sday,syear), final=julday(fmonth,fday,fyear), units='Days')
	caldat, juls, month, day, year
	adate = year*10000L+month*100L+day
	ndates = n_elements(adate)

	
	;*******************************************
	; Ray-tracing
	;*******************************************
	hist = fltarr(3, 71, nelev_steps+1)
	rt_run, adate[0], radar
	rad_fit_calculate_phase, date=adate[0], time=[0,1200], tdiff=-.324, interfer_pos=[0.,-58.9,-2.7], scan_boresite_offset=8., phi0=phi0
	
	; Define beams to count
	nbeams = n_elements(rt_data.beam[0,*])
	
	; Find midnight
	rad_calc_sunset, adate[0], radar, 7, 70, $
		solnoon=solnoon
	julmidnight = solnoon[20] - 0.5d

	; Correct power
	pinds = where(rt_data.power le 0., ccpinds)
	if ccpinds gt 0 then $
		rt_data.power[pinds] = 0.

	; Build histogram
	indsmidnight = where(rt_data.juls ge julmidnight-3.5d/24.d and $
						rt_data.juls le julmidnight+3.5d/24.d, nnighttimes)
	if nnighttimes gt 0 then begin
		for it=0,nnighttimes-1 do begin
			caldat, rt_data.juls[indsmidnight[it]], mm, dd, yy, hr, mn
			tdate = yy*10000L + mm*100L + dd
			ttime = hr*100L + mn

			beams = [0, (nbeams-1)/2, nbeams-1]
			for ib=0,2 do begin
				tphi0 = reform(phi0[indsmidnight[it],beams[ib],*])
				for ng=0,50 do begin
					if ng lt xrange[1] then begin
						for nel=0,nelev_steps-2 do begin
							if tphi0[ng] ge phase_steps[nel] and tphi0[ng] lt phase_steps[nel+1] then $
								hist[ib,ng,nel] = hist[ib,ng,nel] + 1
						endfor
					endif
				endfor
			endfor
		; end time loop
		endfor
	endif
	hist = hist/max(hist[*,10:*,*])
	stop
	; Plot results
	for ib=0,2 do begin
		position = define_panel(3,2,ib,0, /bar)
		if ib ne 0 then begin
			ytickname = replicate(' ',60)
			ytitle = ''
		endif else begin
			ytickname = ''
			ytitle = 'Elevation'
		endelse
		xtickname = replicate(' ',60)
		xtitle = ''
		plot, xrange, yrange, /nodata, xstyle=1, ystyle=1, position=position, $
			xtickname=xtickname, ytickname=ytickname, xtitle=xtitle, ytitle=ytitle, $
			xticklen=1, xgridstyle=1, yticklen=1, ygridstyle=1, charsize=charsize
		for ir=0,70 do begin
			for iel=0,nelev_steps-2 do begin
				if ir*45.+180. ge xrange[0] and (ir+1)*45.+180. le xrange[1] and elev_steps[iel] ge yrange[0] and elev_steps[iel+1] le yrange[1] then begin
					col = bytscl(hist(ib,ir,iel), min=0, max=1, top=250) + 3b
					
					if col gt 3b then $
						polyfill, 180.+[ir,ir+1,ir+1,ir]*45., elev_steps[iel]*[1,1,0,0] + elev_steps[iel+1]*[0,0,1,1], col=col
				endif
			endfor
		endfor
		contour, aspect[beams[ib],*,*], asparanges, aspaelev, /overplot, levels=[60.,70., 80., 85., 89.], c_labels=1b+bytarr(5), c_charsize=charsize
		xyouts, position[0]+(position[2]-position[0])/2., position[3]*.97, 'Beam '+strtrim(beams[ib],2), /normal, charsize=charsize, align=.5
	endfor


	;*******************************************
	; Radar
	;*******************************************

	hist = fltarr(3,71,nelev_steps+1)
	for id=0,ndates-1 do begin
		rad_fit_read, adate[id], radar, time=[0,1200], /filter, /ajground, /catfile, catpath='~/tmp/'
		rad_fit_calculate_elevation, date=date, time=[0,1200], /overwrite, tdiff=-.324, interfer_pos=[0.,-58.9,-2.7], scan_boresite_offset=8.
		data_index = rad_fit_get_data_index()
		if data_index eq -1 then $
			continue
		print, min((*rad_fit_data[data_index]).phi0)
		; Find midnight
		rad_calc_sunset, adate[id], radar, 7, 70, $
			solnoon=solnoon
		julmidnight = solnoon[20] - 0.5d

		beams = [0, (nbeams-1)/2, nbeams-1]
		for ib=0,2 do begin
			binds = where((*rad_fit_data[data_index]).beam eq beams[ib] and $
						(*rad_fit_data[data_index]).juls ge julmidnight-3.d/24.d and $
						(*rad_fit_data[data_index]).juls le julmidnight+3.d/24.d and $
						(*rad_fit_data[data_index]).tfreq ge 10e3 and $
						(*rad_fit_data[data_index]).tfreq le 12e3, ccinds)
			if ccinds le 0 then $
				continue

			elev = (*rad_fit_data[data_index]).elevation[binds,*]
			phi0 = (*rad_fit_data[data_index]).phi0[binds,*]
			power = (*rad_fit_data[data_index]).power[binds,*]
			scat = (*rad_fit_data[data_index]).gscatter[binds,*]
			print, min(phi0)
			for ng=0,50 do begin
				if ng lt xrange[1] then begin
					for nel=0,nelev_steps-2 do begin
						elinds = where(phi0[*,ng] ge phase_steps[nel] and phi0[*,ng] lt phase_steps[nel+1], ccel)
						if ccel gt 0. then $
							hist[ib,ng,nel] = hist[ib,ng,nel] + ccel
					endfor
				endif
			endfor
		endfor
	endfor
	hist = hist/max(hist[*,10:*,*])

	; Plot results
	for ib=0,2 do begin
		position = define_panel(3,2,ib,1, /bar)
		if ib ne 0 then begin
			ytickname = replicate(' ',60)
			ytitle = ''
		endif else begin
			ytickname = ''
			ytitle = 'Phase'
		endelse
		xtickname = ''
		xtitle = 'Slant range [km]'
		plot, xrange, [-!pi,!pi], /nodata, xstyle=1, ystyle=1, position=position, $
			xtickname=xtickname, ytickname=ytickname, xtitle=xtitle, ytitle=ytitle, $
			xticklen=1, xgridstyle=1, yticklen=1, ygridstyle=1, charsize=charsize
		for ir=0,70 do begin
			for iel=0,nelev_steps-1 do begin
				if ir*45.+180. ge xrange[0] and (ir+1)*45.+180. le xrange[1] and elev_steps[iel] ge yrange[0] and elev_steps[iel]+dphi le yrange[1] then begin
					col = bytscl(hist(ib,ir,iel), min=0, max=1, top=250) + 3b
					
					if col gt 3b then $
						polyfill, 180.+[ir,ir+1,ir+1,ir]*45., phase_steps[iel]*[1,1,0,0] + (phase_steps[iel]+dphi)*[0,0,1,1], col=col
				endif
			endfor
		endfor
	endfor

	bpos = [.9,.3,.915,.65]
	plot_colorbar, /vert, charthick=charthick, /continuous, $
		nlevels=4, scale=[0,1], position=bpos, charsize=charsize, $
		legend='Scatter distribution', /no_rotate, $
		level_format='(F4.2)', /keep_first_last_label
	
	title = radar+' '+STRMID(format_juldate(rt_data.juls[0]),3,9)
	xyouts, .5, .85, title, align=.5, /normal, charsize=charsize

endfor

ps_close, /no_f


end