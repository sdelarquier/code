pro	rad_plot_fov, radar, date=date, beam=beam, coords=coords, $
		yrange=yrange, xrange=xrange, grid=grid, ps=ps

common radarinfo

radptr = PTRARR(n_elements(radar), /allocate_heap)
for ir=0,n_elements(radar)-1 do begin
	; First find radar site structure
	radID = where(network.code[0,*] eq radar[ir], cc)
	if cc le 0 then begin
		print, 'Unknown radar'
		return
	end
	
	; Set coordinate system
	if ~keyword_set(coords) then $
		coords = 'magn'
	if coords ne 'magn' and coords ne 'geog' then begin
		print, 'Invalid coordinate system. Using magn'
		coords = 'magn'
	endif
	
	; Find range-gate locations
	if ~keyword_set(date) then $
		ajul = systime(/julian, /utc) $
	else $
		ajul = calc_jul(date,1200)
	caldat, ajul, mm, dd, year
	tval = TimeYMDHMSToEpoch(year, mm, dd, 12, 0, 0)
	if tval lt network[radID].st_time then begin
		tval = network[radID].st_time
		jul0 = julday(1,1,1970)
		ajul = (jul0 + tval/86400.d)
		caldat, ajul, mm, dd, year
	endif
	for s=0,31 do begin
		if (network[radID].site[s].tval eq -1) then break
		if (network[radID].site[s].tval ge tval) then break
	endfor
	radarsite = network[radID].site[s]
	yrsec = (ajul-julday(1,1,year,0,0,0))*86400.d
	ngates = 75
	nbeams = radarsite.maxbeam
	rad_define_beams, network[radID].id, nbeams, ngates, year, yrsec, coords=coords, $
			/normal, fov_loc_full=fov_loc_full
	
	; +1 for North hemisphere, -1 for south
	hemi = fix( radarsite.geolat/abs(radarsite.geolat) )
	
	; Calculate stereographic projection and plot
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
	*radptr[ir] = {fov_loc_full: fov_loc_full, nbeams: nbeams, ngates:ngates}
	
	; Set plot limits
	if ~keyword_set(xrange) then $
		txrange = [min(fov_loc_full[0,*,*,*],xmin)-5, max(fov_loc_full[0,*,*,*],xmax)+5]
	if ~keyword_set(yrange) then $
		tyrange = [min(fov_loc_full[1,*,*,*],ymin)-5, max(fov_loc_full[1,*,*,*],ymax)+5]
	; Adjust plot limits so that they cover the same extent
	ext = abs(abs(txrange[1]-txrange[0]) - abs(tyrange[1]-tyrange[0]))
	if abs(txrange[1]-txrange[0]) gt abs(tyrange[1]-tyrange[0]) then begin
		tyrange[1] = tyrange[1] + ext/2.
		tyrange[0] = tyrange[0] - ext/2.
	endif else if abs(txrange[1]-txrange[0]) lt abs(tyrange[1]-tyrange[0]) then begin
		txrange[1] = txrange[1] + ext/2.
		txrange[0] = txrange[0] - ext/2.
	endelse

	if ir eq 0 then begin
		xrange = txrange
		yrange = tyrange
	endif else begin
		if abs(txrange[0]) gt abs(xrange[0]) then $
			xrange[0] = txrange[0]
		if abs(txrange[1]) gt abs(xrange[1]) then $
			xrange[1] = txrange[1]
		if abs(tyrange[0]) gt abs(yrange[0]) then $
			yrange[0] = tyrange[0]
		if abs(tyrange[1]) gt abs(yrange[1]) then $
			yrange[1] = tyrange[1]
	endelse

endfor

; Set plot area
set_format, /landscape
if keyword_set(ps) then $
	ps_open, '~/Desktop/rad_plot_fov.ps', /no_init
clear_page
loadct, 0
map_plot_panel, 1, 1, 0, 0, coords=coords, /iso, yrange=yrange, xrange=xrange, hemi=hemi, $
	coast_linecolor=150, grid_linecolor=200, lake_fillcolor=255
	
; Calculate stereographic projection and plot
loadct,8
xx = fltarr(4)
yy = fltarr(4)
ibeam = 0L
for ir=0,n_elements(radar)-1 do begin
	overlay_radar, name=radar[ir], /anno, coords=coords
	for ib=0,(*radptr[ir]).nbeams-1 do begin
		for ig=0,(*radptr[ir]).ngates-1 do begin
			xx = reform((*radptr[ir]).fov_loc_full[0,*,ib,ig])
			yy = reform((*radptr[ir]).fov_loc_full[1,*,ib,ig])
			; Highlight selected beam(s)
			if n_elements(beam) eq 1 then begin
				if (ib eq beam) then begin
					polyfill, xx, yy, col=220
					plots, xx[1:2], yy[1:2], thick=2
					plots, [xx[0],xx[3]], [yy[0],yy[3]], thick=2
				endif
			endif else if n_elements(beam) eq n_elements(radar) then begin
				if (ib eq beam[ir]) then begin
					polyfill, xx, yy, col=220
					plots, xx[1:2], yy[1:2], thick=2
					plots, [xx[0],xx[3]], [yy[0],yy[3]], thick=2
				endif
			endelse else if n_elements(beam) gt 1 then begin
				if (ib eq beam[ibeam]) then begin
					polyfill, xx, yy, col=220
					plots, xx[1:2], yy[1:2], thick=2
					plots, [xx[0],xx[3]], [yy[0],yy[3]], thick=2
					ibeam = ibeam + 1
				endif
			endelse
			if keyword_set(grid) then $
				plots, [xx, xx[0]], [yy, yy[0]], thick=.25
			; Plot fov limits
			if ib eq 0 then $
				plots, [xx[0],xx[3]], [yy[0],yy[3]], thick=2
			if (ib eq (*radptr[ir]).nbeams-1) then $
				plots, xx[1:2], yy[1:2], thick=2
			if (ig eq (*radptr[ir]).ngates-1) then $
				plots, xx[2:3], yy[2:3], thick=2
			if (ig eq 0) then $
				plots, xx[0:1], yy[0:1], thick=2
		endfor
	endfor
endfor	

loadct, 0
map_plot_panel, 1, 1, 0, 0, coords=coords, /iso, /no_fill, yrange=yrange, xrange=xrange, hemi=hemi, $
	coast_linecolor=150, /no_grid

if keyword_set(ps) then $
	ps_close, /no_filename, /no_init
	
end
