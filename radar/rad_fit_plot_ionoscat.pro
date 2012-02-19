pro rad_fit_plot_ionoscat, date, radar, ps=ps, xrange=xrange, yrange=yrange, rt=rt

common radarinfo
common rad_data_blk

if keyword_set(ps) then $
	ps_open, '~/Desktop/ionoscat_'+radar+'_'+strtrim(date,2)+'.ps'
set_format, /landscape, /sardines, /tokyo
clear_page

; Parse date
parse_date, date, yy, mm, dd

; Find range-gate locations
yrsec = (julday(mm,dd,yy) - julday(1,1,yy))*86400.d
radID = where(network.code[0,*] eq radar)
tval = TimeYMDHMSToEpoch(yy, mm, dd, 0, 0, 0)
for s=0,31 do begin
	if (network[radID].site[s].tval eq -1) then break
	if (network[radID].site[s].tval ge tval) then break
endfor
radarsite = network[radID].site[s]
nbeams = network[radID].site[s].maxbeam
ngates = 75
rad_define_beams, network[radID].id, nbeams, ngates, yy, yrsec, coords='magn', $
		/normal, fov_loc_full=fov_loc_full

; Calculate stereographic projection
for ib=0,nbeams do begin
	for ig=0,ngates do begin
		for p=0,3 do begin
			lat = fov_loc_full[0,p,ib,ig]
			lon = fov_loc_full[1,p,ib,ig]
			tmp = calc_stereo_coords(lat, lon)
			fov_loc_full[0,p,ib,ig] = tmp[0]
			fov_loc_full[1,p,ib,ig] = tmp[1]
		endfor
	endfor
endfor

; Set plot limits
txrange = [min(fov_loc_full[0,*,*,*],xmin)-5, max(fov_loc_full[0,*,*,*],xmax)+5]
tyrange = [min(fov_loc_full[1,*,*,*],ymin)-5, max(fov_loc_full[1,*,*,*],ymax)+5]
if ~keyword_set(xrange) then $
	xrange = txrange
if ~keyword_set(yrange) then $
	yrange = tyrange

; Adjust plot limits so that they cover the same extent
ext = abs(abs(xrange[1]-xrange[0]) - abs(yrange[1]-yrange[0]))
if abs(xrange[1]-xrange[0]) gt abs(yrange[1]-yrange[0]) then begin
	yrange[1] = yrange[1] + ext/2.
	yrange[0] = yrange[0] - ext/2.
endif else if abs(xrange[1]-xrange[0]) lt abs(yrange[1]-yrange[0]) then begin
	xrange[1] = xrange[1] + ext/2.
	xrange[0] = xrange[0] - ext/2.
endif

if keyword_set(rt) then $
	xmaps = 3 $
else $
	xmaps = 2

if keyword_set(rt) then begin
	;*******************************************
	; Ray-tracing
	;*******************************************
	; Set plot area
	map_plot_panel, xmaps, 1, 0, 2, date=date, coords='magn', /bar, /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
	
	hist = fltarr(nbeams,71)
	rt_run, date, radar
	
	; Find midnight
	rad_calc_sunset, date, radar, 7, 70, $
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

	; Plot histogram
	rt_data.power[0,*,*] = hist/max(hist,/nan)
	overlay_rt, 0
	map_plot_panel, xmaps, 1, 0, 2, date=date, coords='magn', /bar, /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
	overlay_radar, name=radar, /anno, coords='magn', charsize=.5

	; Plot beam-range grid
	for ib=0,nbeams-1 do begin
		for ig=0,rt_info.ngates-1 do begin
			xx = fov_loc_full[0,*,ib,ig]
			yy = fov_loc_full[1,*,ib,ig]
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
	
	plot_colorbar, xmaps, 1, 0, 2, /vert, charthick=charthick, /continuous, $
		nlevels=4, scale=[0,1], position=bpos, charsize=charsize, $
		legend='Scatter distribution', /no_rotate, $
		level_format='(F4.2)', /keep_first_last_label
endif


;*******************************************
; Radar: SCATTER 
;*******************************************
map_plot_panel, xmaps, 1, 0, 0, date=date, coords='magn', /bar, /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1

rad_fit_read, date, radar, time=[0,1200], /filter, /ajground, /catfile, catpath='/tmp/'
data_index = rad_fit_get_data_index()
if data_index eq -1 then $
	return

; Find midnight
rad_calc_sunset, date, radar, 7, 70, $
	solnoon=solnoon
julmidnight = solnoon[20] - 0.5d

; Histogram construction
hist = fltarr(nbeams,71)
for ib=0,nbeams-1 do begin
	; scatter histogram
	binds = where((*rad_fit_data[data_index]).beam eq ib and $
				(*rad_fit_data[data_index]).juls ge julmidnight-3.d/24.d and $
				(*rad_fit_data[data_index]).juls le julmidnight+3.d/24.d and $
				(*rad_fit_data[data_index]).tfreq ge 10e3 and $
				(*rad_fit_data[data_index]).tfreq le 12e3, ccinds)
	if ccinds le 0 then $
		continue

	power = (*rad_fit_data[data_index]).power[binds,*]
	scat = (*rad_fit_data[data_index]).gscatter[binds,*]
	for ng=0,70 do begin
		pinds = where(power[*,ng] ne 10000. and power[*,ng] ge 6. and scat[*,ng] eq 0b, ccpinds)
		if ccpinds gt 0 then $
			hist[ib,ng] = hist[ib,ng] + ccpinds;total(power[pinds,ng])
	endfor
endfor
hist = hist/max(hist, /nan)

; Plot beam-range grid
for ib=0,nbeams-1 do begin
	for ig=0,ngates do begin
		xx = fov_loc_full[0,*,ib,ig]
		yy = fov_loc_full[1,*,ib,ig]
		polyfill, xx, yy, col=bytscl(hist[ib,ig], min=0, max=1, top=252)+2b
; 		plots, [xx, xx[0]], [yy, yy[0]], thick=1
		if (ib eq nbeams-1) then $
			plots, xx[1:2], yy[1:2];, thick=2, col=200
		if ~(ib mod 4) then $
			plots, [xx[0],xx[3]], [yy[0],yy[3]];, thick=2, col=200
		if ~(ig mod 5) then $
			plots, xx[0:1], yy[0:1];, thick=2, col=200
		if (ig eq ngates) then $
			plots, xx[2:3], yy[2:3];, thick=2, col=200
	endfor
endfor

; Plot histogram
map_plot_panel, xmaps, 1, 0, 0, date=date, coords='magn', /bar, /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
overlay_radar, name=radar, /anno, coords='magn', charsize=.5

plot_colorbar, xmaps, 1, 0, 0, /vert, charthick=charthick, /continuous, $
	nlevels=4, scale=[0,1], position=bpos, charsize=charsize, $
	legend='Scatter distribution', /no_rotate, $
	level_format='(F4.2)', /keep_first_last_label


;*******************************************
; Radar: VELOCITY
;*******************************************
vscale = [-50,50]
loadct, 4, file='/tmp/colors2.tbl'
map_plot_panel, xmaps, 1, 0, 1, date=date, coords='magn', /bar, /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1

rad_fit_read, date, radar, time=[0,1200], /filter, /ajground, /catfile, catpath='/tmp/'
data_index = rad_fit_get_data_index()
if data_index eq -1 then $
	return

; Find midnight
rad_calc_sunset, date, radar, 7, 70, $
	solnoon=solnoon
julmidnight = solnoon[20] - 0.5d

; Histogram construction
nvel = fltarr(nbeams,71)
velhist = fltarr(nbeams,71)
for ib=0,nbeams-1 do begin
	; velocity median
	binds = where((*rad_fit_data[data_index]).beam eq ib and $
				(*rad_fit_data[data_index]).juls ge julmidnight-0.5d/24.d and $
				(*rad_fit_data[data_index]).juls le julmidnight+0.5d/24.d and $
				(*rad_fit_data[data_index]).tfreq ge 10e3 and $
				(*rad_fit_data[data_index]).tfreq le 12e3, ccinds)
	if ccinds le 0 then $
		continue

	velocity = (*rad_fit_data[data_index]).velocity[binds,*]
	power = (*rad_fit_data[data_index]).power[binds,*]
	scat = (*rad_fit_data[data_index]).gscatter[binds,*]
	for ng=0,70 do begin
		pinds = where(velocity[*,ng] ne 10000. and power[*,ng] ge 6. and scat[*,ng] eq 0b, ccpinds)
		if ccpinds gt 0 then $
			velhist[ib,ng] = MEDIAN(velocity[pinds])
	endfor
endfor

; Plot beam-range grid
for ib=0,nbeams-1 do begin
	for ig=0,ngates do begin
		xx = fov_loc_full[0,*,ib,ig]
		yy = fov_loc_full[1,*,ib,ig]
		polyfill, xx, yy, col=bytscl(velhist[ib,ig], min=vscale[0], max=vscale[1], top=252)+2b
; 		plots, [xx, xx[0]], [yy, yy[0]], thick=1
		if (ib eq nbeams-1) then $
			plots, xx[1:2], yy[1:2];, thick=2, col=200
		if ~(ib mod 4) then $
			plots, [xx[0],xx[3]], [yy[0],yy[3]];, thick=2, col=200
		if ~(ig mod 5) then $
			plots, xx[0:1], yy[0:1];, thick=2, col=200
		if (ig eq ngates) then $
			plots, xx[2:3], yy[2:3];, thick=2, col=200
	endfor
endfor

; Plot histogram
map_plot_panel, xmaps, 1, 0, 1, date=date, coords='magn', /bar, /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
overlay_radar, name=radar, /anno, coords='magn', charsize=.5

plot_colorbar, xmaps, 1, 0, 1, /vert, charthick=charthick, /continuous, $
	nlevels=4, scale=vscale, position=bpos, charsize=charsize, $
	legend='Velocity [m/s]', /no_rotate, $
	level_format='(F4.2)', /keep_first_last_label

if keyword_set(ps) then $
	ps_close, /no_f

loadct, 0, file='/tmp/colors2.tbl'

end