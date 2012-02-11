pro rt_plot_map_all, radar, date, ps=ps

common rt_data_blk
common radarinfo
common rad_data_blk


yrange = [-50., 10.]
xrange = [-43.,37.]

parse_date, calc_date(date,-15), syear, smonth, sday
parse_date, calc_date(date,15), fyear, fmonth, fday
juls = timegen(start=julday(smonth,sday,syear), final=julday(fmonth,fday,fyear), units='Days')
caldat, juls, month, day, year
adate = year*10000L+month*100L+day
ndates = n_elements(adate)

; radar list
; radar = ['wal','bks','fhe','fhw','cve','cvw']

if keyword_set(ps) then $
	ps_open, '~/Desktop/mapall_'+radar+'_'+strtrim(date,2)+'.ps'

; Set plot area
set_format, /landscape
clear_page
position = define_panel(2,1,0,0, /bar)
map_plot_panel, 2, 1, 0, 0, date=date, coords='magn', /bar, /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1

	
;*******************************************
; Ray-tracing
;*******************************************
rad_calc_sunset, date, radar, 7, 70, $
	solnoon=solnoon
julmidnight = solnoon[20] - 0.5d
; 	caldat, julmidnight, mm, dd, yy, hh, mn


rt_run, date, radar

; Find range-gate locations
sjul = rt_info.sjul
fjul = rt_info.fjul
ajul = (sjul+fjul)/2.d
caldat, ajul, mm, dd, year
yrsec = (ajul-julday(1,1,year,0,0,0))*86400.d
radID = where(network.ID eq rt_info.id)
tval = TimeYMDHMSToEpoch(year, mm, dd, 0, 0, 0)
for s=0,31 do begin
	if (network[radID].site[s].tval eq -1) then break
	if (network[radID].site[s].tval ge tval) then break
endfor
radarsite = network[radID].site[s]
nbeams = network[radID].site[s].maxbeam
rad_define_beams, rt_info.id, nbeams, rt_info.ngates, year, yrsec, coords='magn', $
		/normal, fov_loc_full=fov_loc_full

hist = fltarr(nbeams,rt_info.ngates+1)
indsmidnight = where(rt_data.juls ge julmidnight-3.5d/24.d and $
					rt_data.juls le julmidnight+3.5d/24.d, nnighttimes)
if nnighttimes gt 0 then begin
	for it=0,nnighttimes-1 do begin
		hist = hist + reform((rt_data.gscatter[indsmidnight[it],*,*] - 1.)*rt_data.gscatter[indsmidnight[it],*,*]/2.)
	endfor
endif
; rt_run, calc_date(date,1,unit='month'), radar
; rad_calc_sunset, calc_date(date,1,unit='month'), radar, 7, 70, $
; 	solnoon=solnoon
; julmidnight = solnoon[20] - 0.5d
; indsmidnight = where(rt_data.juls ge julmidnight-3.5d/24.d and $
; 					rt_data.juls le julmidnight+3.5d/24.d, nnighttimes)
; if nnighttimes gt 0 then begin
; 	for it=0,nnighttimes-1 do begin
; 		hist = hist + $	
; 			reform((rt_data.gscatter[indsmidnight[it],*,*] - 1.)*rt_data.gscatter[indsmidnight[it],*,*]/2.)
; 	endfor
; endif

rt_data.power[0,*,*] = hist/max(hist,/nan)
; overlay_rt, 0
map_plot_panel, 2, 1, 0, 0, date=date, coords='magn', /bar, /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
overlay_radar, name=radar, /anno, coords='magn'
loadct,8
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
; 		if (ib eq nbeams-1) then $
; 			plots, xx[1:2], yy[1:2];, thick=2, col=200
; 		if ~(ib mod 4) then $
; 			plots, [xx[0],xx[3]], [yy[0],yy[3]];, thick=2, col=200
		if (ib eq 11) then begin
			polyfill, xx, yy, col=220
			plots, xx[1:2], yy[1:2], thick=2
			plots, [xx[0],xx[3]], [yy[0],yy[3]], thick=2
		endif
; 		if ~(ig mod 5) then $
; 			plots, xx[0:1], yy[0:1];, thick=2, col=200
; 		if (ig eq rt_info.ngates-1) then $
; 			plots, xx[2:3], yy[2:3];, thick=2, col=200
		plots, [xx, xx[0]], [yy, yy[0]], thick=.25
	endfor
endfor

map_plot_panel, 2, 1, 0, 0, date=date, coords='magn', /bar, /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1

;*******************************************
; Radar
;*******************************************
; map_plot_panel, 2, 1, 1, 0, date=date, coords='magn', /bar, /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
; 
; hist = fltarr(nbeams,rt_info.ngates+1)
; for id=0,ndates-1 do begin
; 	rad_fit_read, adate[id], radar, time=[0,1200], /filter, /ajground, /catfile, catpath='~/tmp/'
; 	data_index = rad_fit_get_data_index()
; 	if data_index eq -1 then $
; 		continue
; 
; 	rad_calc_sunset, adate[id], radar, 7, 70, $
; 		solnoon=solnoon
; 	julmidnight = solnoon[20] - 0.5d
; 	for ib=0,nbeams-1 do begin
; 		binds = where((*rad_fit_data[data_index]).beam eq ib and $
; 					(*rad_fit_data[data_index]).juls ge julmidnight-3.d/24.d and $
; 					(*rad_fit_data[data_index]).juls le julmidnight+3.d/24.d and $
; 					(*rad_fit_data[data_index]).tfreq ge 10e3 and $
; 					(*rad_fit_data[data_index]).tfreq le 12e3, ccinds)
; 		if ccinds le 0 then $
; 			continue
; 
; 		elev = (*rad_fit_data[data_index]).elevation[binds,*]
; 		power = (*rad_fit_data[data_index]).power[binds,*]
; 		scat = (*rad_fit_data[data_index]).gscatter[binds,*]
; 		for ng=4,50 do begin
; 			pinds = where(power[*,ng] ne 10000. and power[*,ng] ge 6. and scat[*,ng] eq 0b, ccpinds)
; 			if ccpinds gt 0 then $
; 				hist[ib,ng] = hist[ib,ng] + ccpinds;total(power[pinds,ng])
; 		endfor
; 	endfor
; endfor
; 
; rt_data.power[0,*,*] = hist/max(hist,/nan)
; overlay_rt, 0
; map_plot_panel, 2, 1, 1, 0, date=date, coords='magn', /bar, /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
; overlay_radar, name=radar, /anno, coords='magn'
; 
; xx = fltarr(4)
; yy = fltarr(4)
; for ib=0,nbeams-1 do begin
; 	for ig=0,rt_info.ngates-1 do begin
; 		for p=0,3 do begin
; 			lat = fov_loc_full[0,p,ib,ig]
; 			lon = fov_loc_full[1,p,ib,ig]
; 			tmp = calc_stereo_coords(lat, lon)
; 			xx[p] = tmp[0]
; 			yy[p] = tmp[1]
; 		endfor
; ; 		plots, [xx, xx[0]], [yy, yy[0]], thick=1
; 		if (ib eq nbeams-1) then $
; 			plots, xx[1:2], yy[1:2];, thick=2, col=200
; 		if ~(ib mod 4) then $
; 			plots, [xx[0],xx[3]], [yy[0],yy[3]];, thick=2, col=200
; 		if ~(ig mod 5) then $
; 			plots, xx[0:1], yy[0:1];, thick=2, col=200
; 		if (ig eq rt_info.ngates-1) then $
; 			plots, xx[2:3], yy[2:3];, thick=2, col=200
; 	endfor
; endfor
; 
; bpos = [.85,.3,.865,.65]
; plot_colorbar, /vert, charthick=charthick, /continuous, $
; 	nlevels=4, scale=[0,1], position=bpos, charsize=charsize, $
; 	legend='Scatter distribution', /no_rotate, $
; 	level_format='(F4.2)', /keep_first_last_label

if keyword_set(ps) then $
	ps_close, /no_f

end