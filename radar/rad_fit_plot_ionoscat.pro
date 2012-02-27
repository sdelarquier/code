;- Plots a field of view representation of scatter distribution for a given radar and day for at midnight +/- 3 hours
; By defults, only radar power data is plotted (as a scatter distribution). But ray-tracing can also be added, 
; as well as median velocities
;
; Last update: Feb, 23, 2012
pro rad_fit_plot_ionoscat, date, radar, ps=ps, xrange=xrange, yrange=yrange, rt=rt, vel=vel

common rt_data_blk
common radarinfo
common rad_data_blk

if keyword_set(ps) then $
	ps_open, '~/Desktop/ionoscat_'+radar+'_'+strtrim(date,2)+'.ps'
set_format, /landscape
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
ngates = 65
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

if keyword_set(rt) and keyword_set(vel) then $
	xmaps = 3 $
else if keyword_set(rt) or keyword_set(vel) then $
	xmaps = 2 $
else $
	xmaps = 1
; Initilalize position
xmap = 0


if keyword_set(rt) then begin
	;*******************************************
	; Ray-tracing
	;*******************************************
	; Set plot area
	map_plot_panel, xmaps, 1, xmaps-xmap-1, 0, date=date, coords='magn', /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
	
	hist = fltarr(nbeams,71)
	rt_run, date, radar, time=[0,1200], /no_rays
	
	; Find midnight
	rad_calc_sunset, date, radar, 7, 70, $
		solnoon=solnoon
	julmidnight = solnoon[20] - 0.5d

	; zeros power where undefined
	pinds = where(rt_data.power eq 10000. and rt_data.gscatter ne 2b, ccpinds)
	if ccpinds gt 0 then $
		rt_data.power[pinds] = 0.

	; Build histogram
	indsmidnight = where(rt_data.juls ge julmidnight-3.d/24.d and $
				rt_data.juls le julmidnight+3.d/24.d, nnighttimes)
	if nnighttimes gt 0 then begin
		for it=0,nnighttimes-1 do begin
			hist = hist + 1./( rt_data.power[indsmidnight[it],*,*] / min(rt_data.power[indsmidnight,*,*]) )
			; hist = hist + rt_data.lagpower[indsmidnight[it],*,*]
		endfor
		; print, transpose([[indgen(71)], [reform(hist[20,*]/max(hist))]]), indsmidnight
		rt_data.power[0,*,*] = hist/max(hist)
	endif
	
	; Plot histogram
	overlay_rt, 0
	map_plot_panel, xmaps, 1, xmaps-xmap-1, 0, date=date, coords='magn', /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
	overlay_radar, name=radar, /anno, coords='magn', charsize=.5

	; Plot beam-range grid
	for ib=0,nbeams-1 do begin
		for ig=0,ngates do begin
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
	
; 	plot_colorbar, xmaps, 1, xmaps-xmap-1, 0, charthick=charthick, /continuous, $
; 		nlevels=4, scale=[0,1], charsize=charsize, $
; 		legend='Scatter distribution', /no_rotate, $
; 		level_format='(F4.2)', /keep_first_last_label, /horizontal
	pos = define_panel(xmaps, 1, xmaps-xmap-1, 0)
	print, pos
	xyouts, pos[0]+(pos[2]-pos[0])/2., pos[3]*1.03, $
			STRMID(format_juldate(julmidnight),0,17)+textoidl('\pm')+'3:00 UT', $
			align=.5, /normal;, charsize=get_charsize(xmaps,1)
	xmap = xmap + 1
endif


if keyword_set(vel) then begin
	;*******************************************
	; Radar: VELOCITY
	;*******************************************
	vscale = [-50,50]
	map_plot_panel, xmaps, 1, xmaps-xmap-1, 0, date=date, coords='magn', /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1

	rad_fit_read, date, radar, time=[0,1200], /filter, /ajground, /catfile, catpath='~/tmp/'
	data_index = rad_fit_get_data_index()
	if data_index eq -1 then $
		return

	; Find midnight
	rad_calc_sunset, date, radar, 7, 70, $
		solnoon=solnoon
	julmidnight = solnoon[20] - 0.5d

	; Histogram construction
	nvel = fltarr(nbeams,ngates+1)
	velhist = fltarr(nbeams,ngates+1)
	for ib=0,nbeams-1 do begin
		; velocity median
		binds = where((*rad_fit_data[data_index]).beam eq ib and $
					(*rad_fit_data[data_index]).juls ge julmidnight-3.d/24.d and $
					(*rad_fit_data[data_index]).juls le julmidnight+3.d/24.d and $
					(*rad_fit_data[data_index]).tfreq ge 10e3 and $
					(*rad_fit_data[data_index]).tfreq le 12e3, ccinds)
		if ccinds le 0 then begin
			velhist[ib,*] = 10000.
			continue
		endif

		velocity = (*rad_fit_data[data_index]).velocity[binds,*]
		power = (*rad_fit_data[data_index]).power[binds,*]
		scat = (*rad_fit_data[data_index]).gscatter[binds,*]
		for ng=0,ngates do begin
			pinds = where(velocity[*,ng] ne 10000. and power[*,ng] ge 6. and scat[*,ng] eq 0b, ccpinds)
			if ccpinds gt 0 then $
				velhist[ib,ng] = MEDIAN(velocity[pinds]) $
			else $
				velhist[ib,ng] = 10000.
		endfor
	endfor

	; Plot beam-range grid
	;loadct, 4, file='/tmp/colors2.tbl'
	for ib=0,nbeams-1 do begin
		for ig=0,ngates do begin
			xx = fov_loc_full[0,*,ib,ig]
			yy = fov_loc_full[1,*,ib,ig]
			if velhist[ib,ig] ne 10000. then $
				polyfill, xx, yy, col=(bytscl(-velhist[ib,ig], min=vscale[0], max=vscale[1], top=252)+126b mod 252)+2b
	; 		plots, [xx, xx[0]], [yy, yy[0]], thick=1
			if (ib eq nbeams-1) then $
				plots, xx[1:2], yy[1:2];, thick=2, col=200
			if ~(ib mod 4) then $
				plots, [xx[0],xx[3]], [yy[0],yy[3]];, thick=2, col=200
			if ~(ig mod 5) then $
				plots, xx[0:1], yy[0:1];, thick=2, col=200
			if (ig eq ngates-1) then $
				plots, xx[2:3], yy[2:3];, thick=2, col=200
		endfor
	endfor
	;loadct, 0, file='/tmp/colors2.tbl'

	; Plot map
	map_plot_panel, xmaps, 1, xmaps-xmap-1, 0, date=date, coords='magn', /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
	overlay_radar, name=radar, /anno, coords='magn', charsize=.5

	;loadct, 4, file='/tmp/colors2.tbl'
	plot_colorbar, xmaps, 1, xmaps-xmap-1, 0, charthick=charthick, /continuous, $
		nlevels=4, scale=vscale, charsize=charsize, $
		legend='Velocity [m/s]', $
		level_format='(F6.1)', /keep_first_last_label, /horizontal
	;loadct, 0, file='/tmp/colors2.tbl'
	xmap = xmap + 1
endif


;*******************************************
; Radar: SCATTER 
;*******************************************
map_plot_panel, xmaps, 1, 0, 0, date=date, coords='magn', /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1

rad_fit_read, date, radar, time=[0,1200], /filter, /ajground, /catfile, catpath='~/tmp/'
data_index = rad_fit_get_data_index()
if data_index eq -1 then $
	return

; Find midnight
rad_calc_sunset, date, radar, 7, 70, $
	solnoon=solnoon
julmidnight = solnoon[20] - 0.5d

; Histogram construction
hist = fltarr(nbeams,ngates+1)
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
	for ng=0,ngates do begin
		pinds = where(power[*,ng] ne 10000. and power[*,ng] ge 6. and scat[*,ng] eq 0b, ccpinds)
		if ccpinds gt 0 then $
			hist[ib,ng] = hist[ib,ng] + ccpinds
	endfor
endfor
hist = hist/max(hist, /nan)

; Plot beam-range grid
for ib=0,nbeams-1 do begin
	for ig=0,ngates do begin
		xx = fov_loc_full[0,*,ib,ig]
		yy = fov_loc_full[1,*,ib,ig]
		if hist[ib,ig] gt 0. then $
			polyfill, xx, yy, col=bytscl(hist[ib,ig], min=0, max=1, top=252)+2b
; 		plots, [xx, xx[0]], [yy, yy[0]], thick=1
		if (ib eq nbeams-1) then $
			plots, xx[1:2], yy[1:2];, thick=2, col=200
		if ~(ib mod 4) then $
			plots, [xx[0],xx[3]], [yy[0],yy[3]];, thick=2, col=200
		if ~(ig mod 5) then $
			plots, xx[0:1], yy[0:1];, thick=2, col=200
		if (ig eq ngates-1) then $
			plots, xx[2:3], yy[2:3];, thick=2, col=200
		if (ib eq 20) then begin
			plots, xx[1:2], yy[1:2], thick=2
			plots, [xx[0],xx[3]], [yy[0],yy[3]], thick=2
		endif
	endfor
endfor

; Plot map
map_plot_panel, xmaps, 1, 0, 0, date=date, coords='magn', /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
overlay_radar, name=radar, /anno, coords='magn', charsize=.5

plot_colorbar, xmaps, 1, 0, 0, charthick=charthick, /continuous, $
	nlevels=4, scale=[0,1], charsize=charsize, $
	legend='Scatter distribution', /no_rotate, $
	level_format='(F4.2)', /keep_first_last_label, /horizontal




if keyword_set(ps) then $
	ps_close, /no_f

end
