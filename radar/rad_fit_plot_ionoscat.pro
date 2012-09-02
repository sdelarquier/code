;- Plots a field of view representation of scatter distribution for a given radar and day for at midnight +/- 3 hours
; By defults, only radar power data is plotted (as a scatter distribution). But ray-tracing can also be added, 
; as well as median velocities.
; The time and dtime keywords can be used together to look at different time inervals centered on different times.
; edges outputs the leading and trailing edge of the scatter distribution for each beam (threshold at 75% scatter)
;
; Last update: Feb, 23, 2012
pro rad_fit_plot_ionoscat, date, radar, ps=ps, xrange=xrange, yrange=yrange, rtrun=rtrun, time=time, dtime=dtime, edges=edges, $
    radhist=radhist, rthist=rthist, radalt=radalt, rtalt=rtalt

common rt_data_blk
common radarinfo
common rad_data_blk

if keyword_set(ps) then begin
	if size(ps,/type) eq 7 then $
		ps_open, ps $
	else $
		ps_open, '~/Desktop/ionoscat_'+radar+'_'+strtrim(date,2)+'.ps'
endif
set_format, /landscape
clear_page

if ~keyword_set(dtime) then $
	dtime = 4.d $
else $
	dtime = dtime*1.d

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
		/normal, fov_loc_full=fov_loc_full, fov_loc_center=fov_loc_center


; Set scatter edge finding
scathresh = .75
edges = fltarr(nbeams,2,2)

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
		if ib lt nbeams then begin
			lat = fov_loc_center[0,ib,ig]
			lon = fov_loc_center[1,ib,ig]
			tmp = calc_stereo_coords(lat, lon)
			fov_loc_center[0,ib,ig] = tmp[0]
			fov_loc_center[1,ib,ig] = tmp[1]
		endif
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

if keyword_set(rtrun) and keyword_set(vel) then $
	xmaps = 3 $
else if keyword_set(rtrun) or keyword_set(vel) then $
	xmaps = 2 $
else $
	xmaps = 1
; Initilalize position
xmap = 0


if keyword_set(rtrun) then begin
	;*******************************************
	; Ray-tracing
	;*******************************************
	; Set plot area
	map_plot_panel, xmaps, 1, xmaps-xmap-1, 0, date=date, coords='magn', /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
	
	hist = fltarr(nbeams,ngates+1)
    alt = 100. + findgen(40)*10.
    rtalt = fltarr(nbeams,n_elements(alt))
	rthist = hist
	parse_date, date, tyy, tmm, tdd
	rtdate = tyy*10000L+tmm*100L+01
; 	rtdate = date
	rt_run, rtdate, radar, time=[0,1200], /no_rays
	
	; Find midnight
	rad_calc_sunset, rtdate, radar, 7, 70, $
		solnoon=solnoon
	julmidnight = solnoon[20] - 0.5d
	if keyword_set(time) then begin
		parse_date, date, yy, mm, dd
		parse_time, time, hr, mn
		julmidnight = julday(dd, mm, yy, hr, mn)
	endif

	; zeros power where undefined
	pinds = where(rt_data.power eq 10000. and rt_data.gscatter ne 2b, ccpinds)
	if ccpinds gt 0 then $
		rt_data.power[pinds] = 0.

	; Build histogram
	indsmidnight = where(rt_data.juls ge julmidnight-dtime/24.d and $
				rt_data.juls le julmidnight+dtime/24.d, nnighttimes)
	if nnighttimes gt 0 then begin
		for it=0,nnighttimes-1 do begin
			for ib=0,nbeams-1 do begin
				for ig=0,ngates do begin
                    if rt_data.altitude[indsmidnight[it],ib,ig] lt 150. then continue
                    if rt_data.power[indsmidnight[it],ib,ig] lt 0. and rt_data.gscatter[indsmidnight[it],ib,ig] eq 2b then begin
                        hist[ib,ig] = hist[ib,ig] + 1./( rt_data.power[indsmidnight[it],ib,ig] / min(rt_data.power[indsmidnight,*,*]) )
                    endif
				endfor
			endfor
		endfor
		; print, transpose([[indgen(71)], [reform(hist[20,*]/max(hist))]]), indsmidnight
		rt_data.power[0,*,0:ngates] = hist/max(hist)
		rthist = hist
	endif
	
	; Plot histogram
	overlay_rt, 0
	map_plot_panel, xmaps, 1, xmaps-xmap-1, 0, date=date, coords='magn', /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
	overlay_radar, name=radar, /anno, coords='magn', charsize=charsize

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
		; Find scatter limits (within threshold)
		beamhist = hist[ib,*]/max(hist)
		thinds = where(beamhist gt scathresh, cc)
		if cc gt 0 then begin
			edges[ib,1,0] = thinds[0]
			edges[ib,1,1] = thinds[cc-1]
		endif
	endfor
	
; 	plot_colorbar, xmaps, 1, xmaps-xmap-1, 0, charthick=charthick, /continuous, $
; 		nlevels=4, scale=[0,1], charsize=charsize, $
; 		legend='Scatter distribution', /no_rotate, $
; 		level_format='(F4.2)', /keep_first_last_label, /horizontal
	pos = define_panel(xmaps, 1, xmaps-xmap-1, 0)
	print, pos
	xyouts, pos[0]+(pos[2]-pos[0])/2., pos[3]*1.03, $
			STRMID(format_juldate(julmidnight),0,17)+textoidl('\pm')+'3:00 UT', $
			align=.5, /normal, charsize=charsize;, charsize=get_charsize(xmaps,1)
	xmap = xmap + 1
endif


;*******************************************
; Radar: SCATTER 
;*******************************************
hist = fltarr(nbeams,ngates+1)
radhist = hist
map_plot_panel, xmaps, 1, 0, 0, date=date, coords='magn', /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1


rad_fit_read, date, radar, time=[0,1200], /filter, /ajground, /catfile, catpath='~/tmp/', /force
data_index = rad_fit_get_data_index()
if data_index eq -1 then $
	return

; Find midnight
rad_calc_sunset, date, radar, 7, 70, $
	solnoon=solnoon
julmidnight = solnoon[20] - 0.5d
caldat, julmidnight, mmm, ddd, yyy, hhh, mnn
print, hhh, mnn
if keyword_set(time) then begin
	parse_date, date, yy, mm, dd
	parse_time, time, hr, mn
	julmidnight = julday(mm, dd, yy, hr, mn)
endif
; print, julmidnight, format='(f20.10)'

; Histogram construction
hist = fltarr(nbeams,ngates+1)
alt = 100. + findgen(40)*10.
radalt = fltarr(nbeams,n_elements(alt))
for ib=0,nbeams-1 do begin
	; scatter histogram
	binds = where((*rad_fit_data[data_index]).beam eq ib and $
				(*rad_fit_data[data_index]).juls ge julmidnight-dtime/24.d and $
				(*rad_fit_data[data_index]).juls le julmidnight+dtime/24.d and $
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
radhist = hist
hist = hist/max(hist, /nan)

; Plot beam-range grid
for ib=0,nbeams-1 do begin
	for ig=0,ngates do begin
		xx = fov_loc_full[0,*,ib,ig]
		yy = fov_loc_full[1,*,ib,ig]
		if hist[ib,ig] gt 0. then $
			polyfill, xx, yy, col=bytscl(hist[ib,ig], min=0, max=1, top=250)+2b
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
	; Find scatter limits (within threshold)
	beamhist = hist[ib,*]
	thinds = where(beamhist gt scathresh, cc)
	if cc gt 0 then begin
		edges[ib,0,0] = thinds[0]
		edges[ib,0,1] = thinds[cc-1]
	endif
; 	if ib eq 12 then print, date, edges[ib,0,0], edges[ib,0,1]
endfor

if keyword_set(rtrun) then begin
	contour, hist, reform(fov_loc_center[0,*,*]), reform(fov_loc_center[1,*,*]), /overplot, levels=[0.,.25,.5,.75]
endif

; Plot map
map_plot_panel, xmaps, 1, 0, 0, date=date, coords='magn', /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
overlay_radar, name=radar, /anno, coords='magn', charsize=charsize

plot_colorbar, xmaps, 1, 0, 0, charthick=charthick, /continuous, $
	nlevels=4, scale=[0,1], charsize=charsize, $
	legend='Scatter distribution', /no_rotate, $
	level_format='(F4.2)', /keep_first_last_label, /horizontal



if keyword_set(ps) then $
	ps_close, /no_f

end
