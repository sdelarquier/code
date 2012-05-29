pro rt_plot_ionoscat_season, radar, year

common rt_data_blk
common radarinfo
common rad_data_blk



ps_open, '~/Desktop/mapall_'+radar+'_'+strtrim(year,2)+'.ps'
set_format, /portrait, /sardines, /tokyo
clear_page

yrange = [-50., 0.]
xrange = [-50.,15.]

; Find range-gate locations
yrsec = 0.d
radID = where(network.code[0,*] eq radar)
tval = TimeYMDHMSToEpoch(year, 1, 1, 0, 0, 0)
for s=0,31 do begin
	if (network[radID].site[s].tval eq -1) then break
	if (network[radID].site[s].tval ge tval) then break
endfor
radarsite = network[radID].site[s]
nbeams = network[radID].site[s].maxbeam
rad_define_beams, network[radID].id, nbeams, 70, year, yrsec, coords='magn', $
		/normal, fov_loc_full=fov_loc_full


; Seasons
start_date = year*10000L + 0601
for is=0,3 do begin
	print, calc_date(start_date,is*3, unit='month'), calc_date(start_date,(is+1)*3, unit='month')
	parse_date, calc_date(start_date,is*3, unit='month'), syear, smonth, sday
	parse_date, calc_date(start_date,(is+1)*3, unit='month'), fyear, fmonth, fday
	juls = timegen(start=julday(smonth,sday,syear), final=julday(fmonth,fday,fyear), units='Days')
	caldat, juls, month, day, year
	adate = year*10000L+month*100L+day
	ndates = n_elements(adate)


	; Set plot area
	map_plot_panel, 2, 4, 0, is, date=date, coords='magn', /bar, /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1

	
	;*******************************************
	; Ray-tracing
	;*******************************************
	hist = fltarr(nbeams,71)
	for im=0,2 do begin
		rt_run, calc_date(adate[0], im, unit='month'), radar
		
		; Find midnight
		rad_calc_sunset, calc_date(adate[0], im, unit='month'), radar, 7, 70, $
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
				hist = hist + reform((rt_data.gscatter[indsmidnight[it],*,*] - 1.)*rt_data.gscatter[indsmidnight[it],*,*]/2.)
			endfor
		endif
	endfor

	; Plot histogram
	rt_data.power[0,*,*] = hist/max(hist,/nan)
	overlay_rt, 0
	map_plot_panel, 2, 4, 0, is, date=date, coords='magn', /bar, /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
	overlay_radar, name=radar, /anno, coords='magn', charsize=.5

	; Plot beam-range grid
	xx = fltarr(4)
	yy = fltarr(4)
	for ib=0,nbeams-1 do begin
		for ig=0,rt_info.ngates-1 do begin
			for p=0,3 do begin
				lat = fov_loc_full[0,p,ib,ig]
				lon = fov_loc_full[1,p,ib,ig]
				tmp = calc_stereo_coords(lat, lon)
				xx[p] = tmp[0]
				yy[p] = tmp[1]
			endfor
	; 		plots, [xx, xx[0]], [yy, yy[0]], thick=1
			if (ib eq nbeams-1) then $
				plots, xx[1:2], yy[1:2];, thick=2, col=200
			if ~(ib mod 4) then $
				plots, [xx[0],xx[3]], [yy[0],yy[3]];, thick=2, col=200
			if ~(ig mod 5) then $
				plots, xx[0:1], yy[0:1];, thick=2, col=200
			if (ig eq rt_info.ngates-1) then $
				plots, xx[2:3], yy[2:3];, thick=2, col=200
		endfor
	endfor


	;*******************************************
	; Radar
	;*******************************************
	map_plot_panel, 2, 4, 1, is, date=date, coords='magn', /bar, /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1

	hist = fltarr(nbeams,71)
	for id=0,ndates-1 do begin
		rad_fit_read, adate[id], radar, time=[0,1200], /filter, /ajground, /catfile, catpath='/tmp/'
		data_index = rad_fit_get_data_index()
		if data_index eq -1 then $
			continue

		; Find midnight
		rad_calc_sunset, adate[id], radar, 7, 70, $
			solnoon=solnoon
		julmidnight = solnoon[20] - 0.5d

		for ib=0,nbeams-1 do begin
			binds = where((*rad_fit_data[data_index]).beam eq ib and $
						(*rad_fit_data[data_index]).juls ge julmidnight-3.d/24.d and $
						(*rad_fit_data[data_index]).juls le julmidnight+3.d/24.d and $
						(*rad_fit_data[data_index]).tfreq ge 10e3 and $
						(*rad_fit_data[data_index]).tfreq le 12e3, ccinds)
			if ccinds le 0 then $
				continue

			elev = (*rad_fit_data[data_index]).elevation[binds,*]
			power = (*rad_fit_data[data_index]).power[binds,*]
			scat = (*rad_fit_data[data_index]).gscatter[binds,*]
			for ng=4,50 do begin
				pinds = where(power[*,ng] ne 10000. and power[*,ng] ge 6. and scat[*,ng] eq 0b, ccpinds)
				if ccpinds gt 0 then $
					hist[ib,ng] = hist[ib,ng] + ccpinds;total(power[pinds,ng])
			endfor
		endfor
	endfor

	; Plot histogram
	rt_data.power[0,*,*] = hist/max(hist,/nan)
	overlay_rt, 0
	map_plot_panel, 2, 4, 1, is, date=date, coords='magn', /bar, /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
	overlay_radar, name=radar, /anno, coords='magn', charsize=.5

	; Plot beam-range grid
	xx = fltarr(4)
	yy = fltarr(4)
	for ib=0,nbeams-1 do begin
		for ig=0,rt_info.ngates-1 do begin
			for p=0,3 do begin
				lat = fov_loc_full[0,p,ib,ig]
				lon = fov_loc_full[1,p,ib,ig]
				tmp = calc_stereo_coords(lat, lon)
				xx[p] = tmp[0]
				yy[p] = tmp[1]
			endfor
	; 		plots, [xx, xx[0]], [yy, yy[0]], thick=1
			if (ib eq nbeams-1) then $
				plots, xx[1:2], yy[1:2];, thick=2, col=200
			if ~(ib mod 4) then $
				plots, [xx[0],xx[3]], [yy[0],yy[3]];, thick=2, col=200
			if ~(ig mod 5) then $
				plots, xx[0:1], yy[0:1];, thick=2, col=200
			if (ig eq rt_info.ngates-1) then $
				plots, xx[2:3], yy[2:3];, thick=2, col=200
		endfor
	endfor

endfor


bpos = [.85,.3,.865,.65]
plot_colorbar, /vert, charthick=charthick, /continuous, $
	nlevels=4, scale=[0,1], position=bpos, charsize=charsize, $
	legend='Scatter distribution', /no_rotate, $
	level_format='(F4.2)', /keep_first_last_label

ps_close, /no_f

end