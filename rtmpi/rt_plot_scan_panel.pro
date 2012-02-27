pro rt_plot_scan_panel, time, xmaps, ymaps, xmap, ymap, bar=bar, $
		date=date, xrange=xrange, yrange=yrange, $
		param=param, coords=coords, scale=scale, $
		silent=silent, ground=ground, ionos=ionos, $
		charthick=charthick, charsize=charsize, $
		last=last, first=first, legend=legend

common rt_data_blk
common radarinfo

help, rt_info, /st, output=infout
if n_elements(infout) le 2 then begin
	print, 'No data present'
	return
endif

; Set plot parameters
if ~keyword_set(param) then $
	param = 'power'
	

if ~keyword_set(coords) then $
	coords = 'magn'

if ~keyword_set(charsize) then $
	charsize = get_charsize(xmaps, ymaps)

if n_params() lt 4 then begin
	if ~keyword_set(silent) then $
		prinfo, 'XMAPS, YMAPS, XMAP and YMAP not set, using default.'
	xmaps = 1
	ymaps = 1
	xmap = 0
	ymap = 0
endif

if ~keyword_set(position) then $
	position = define_panel(xmaps, ymaps, xmap, ymap, bar=bar, with_info=with_info, no_title=no_title)

if ~keyword_set(date) then begin
	if ~keyword_set(silent) then $
		prinfo, 'No DATE given, trying for scan date.'
	caldat, rt_data.juls[0], month, day, year
	date = year*10000L + month*100L + day
endif
parse_date, date, tyear, tmonth, tday

; Retrieve raytracing parameters from structure
radar = rt_info.name
caldat, rt_data.juls[*,0], months, days, year, hours, minutes

; Find time index
parse_time, time, thour, tminute
timeind = where(days eq tday and hours eq thour and minutes eq tminute)
juls = julday(months[timeind], days[timeind], year[timeind], hours[timeind], minutes[timeind])

; Find hardware parameters
radID = where(network.ID eq rt_info.id)
tval = TimeYMDHMSToEpoch(tyear, tmonth, tday, 0, 0, 0)
yrsec = (juls[0]-julday(1,1,tyear,0,0,0))*86400.d
for s=0,31 do begin
	if (network[radID].site[s].tval eq -1) then break
    	if (network[radID].site[s].tval ge tval) then break
endfor
radarsite = network[radID].site[s]
nbeams = radarsite.maxbeam
ngates = rt_info.ngates
hbw = radarsite.bmsep/2.	; Set coordinate system
;rotate = -rt_info.glon
rad_define_beams, network[radID].id, nbeams, ngates, tyear, yrsec, coords=coords, $
		/normal, fov_loc_full=fov_loc_full

; +1 for North hemisphere, -1 for south
hemisphere = fix( radarsite.geolat/abs(radarsite.geolat) )

; Calculate stereographic projection and plot
for ib=0,nbeams do begin
	for ig=0,ngates do begin
		for p=0,3 do begin
			lat = fov_loc_full[0,p,ib,ig]
			lon = fov_loc_full[1,p,ib,ig]
			tmp = calc_stereo_coords(lat, lon, rotate=rotate)
			fov_loc_full[0,p,ib,ig] = tmp[0]
			fov_loc_full[1,p,ib,ig] = tmp[1]
		endfor
	endfor
endfor

; Set plot limits
xrange = [min(fov_loc_full[0,*,*,*],xmin)-5, max(fov_loc_full[0,*,*,*],xmax)+5]
yrange = [min(fov_loc_full[1,*,*,*],ymin)-5, max(fov_loc_full[1,*,*,*],ymax)+5]

; Adjust plot limits so that they cover the same extent
ext = abs(abs(xrange[1]-xrange[0]) - abs(yrange[1]-yrange[0]))
if abs(xrange[1]-xrange[0]) gt abs(yrange[1]-yrange[0]) then begin
	yrange[1] = yrange[1] + ext/2.
	yrange[0] = yrange[0] - ext/2.
endif else if abs(xrange[1]-xrange[0]) lt abs(yrange[1]-yrange[0]) then begin
	xrange[1] = xrange[1] + ext/2.
	xrange[0] = xrange[0] - ext/2.
endif

; Select parameter to plot
s = execute('ydata = rt_data.'+param)
case param of
	'power':	begin
						if ~keyword_set(legend) then $
							legend = 'Power'
						if ~keyword_set(scale) then $
							scale = [-30., 0.]
					end
	'elevation':	begin
						if ~keyword_set(legend) then $
							legend = 'Elevation'
						if ~keyword_set(scale) then $
							scale = [10., 45.]
					end
	'altitude':		begin
						if ~keyword_set(legend) then $
							legend = 'Altitude [km]'
						if ~keyword_set(scale) then $
							scale = [100., 500.]
					end
	'valtitude':	begin
						if ~keyword_set(legend) then $
							legend = 'Virtual height [km]'
						if ~keyword_set(scale) then $
							scale = [100., 500.]
					end
	'nr':			begin
						if ~keyword_set(legend) then $
							legend = 'Refractive index'
						if ~keyword_set(scale) then $
							scale = [.8, 1.]
					end
endcase

; Set plot area
magco = cnvcoord(rt_info.glat, rt_info.glon, 0.)
proj = calc_stereo_coords(magco[0], magco[1], rotate=rotate)

; Start plotting
map_plot_panel, position=position, date=date, coords=coords, charsize=charsize, grid_linecolor=1, $
		hemisphere=hemisphere, coast_linecolor=0, /iso, /no_fill, yrange=yrange, xrange=xrange, rotate=rotate, grid_charsize=charsize, /no_label
overlay_radar, rotate=rotate, charsize=charsize, coords=coords, names=radar, /annotate

lati = rt_info.glat
longi = rt_info.glon
fov_loc_full = fltarr(2,4,rt_info.ngates,nbeams)
for ib=0,nbeams-1 do begin
	for ng=0,rt_info.ngates-1 do begin
		if rt_data.grange[timeind[0],ib,ng] gt 0. then begin
		; Calculate range-cell position
			success = calc_pos(lati, longi, 0., rt_data.azim[timeind[0],ib]-hbw, rt_data.grange[timeind[0],ib,ng]-22.5, 0., latiout, longiout)
			if success eq 1 then begin
				magco = cnvcoord(latiout, longiout, 0.)
				proj = calc_stereo_coords(magco[0], magco[1], rotate=rotate)
				fov_loc_full[0,0,ng] = proj[0]
				fov_loc_full[1,0,ng] = proj[1]
			endif
			success = calc_pos(lati, longi, 0., rt_data.azim[timeind[0],ib]-hbw, rt_data.grange[timeind[0],ib,ng]+22.5, 0., latiout, longiout)
			if success eq 1 then begin
				magco = cnvcoord(latiout, longiout, 0.)
				proj = calc_stereo_coords(magco[0], magco[1], rotate=rotate)
				fov_loc_full[0,1,ng] = proj[0]
				fov_loc_full[1,1,ng] = proj[1]
			endif
			success = calc_pos(lati, longi, 0., rt_data.azim[timeind[0],ib]+hbw, rt_data.grange[timeind[0],ib,ng]+22.5, 0., latiout, longiout)
			if success eq 1 then begin
				magco = cnvcoord(latiout, longiout, 0.)
				proj = calc_stereo_coords(magco[0], magco[1], rotate=rotate)
				fov_loc_full[0,2,ng] = proj[0]
				fov_loc_full[1,2,ng] = proj[1]
			endif
			success = calc_pos(lati, longi, 0., rt_data.azim[timeind[0],ib]+hbw, rt_data.grange[timeind[0],ib,ng]-22.5, 0., latiout, longiout)
			if success eq 1 then begin
				magco = cnvcoord(latiout, longiout, 0.)
				proj = calc_stereo_coords(magco[0], magco[1], rotate=rotate)
				fov_loc_full[0,3,ng] = proj[0]
				fov_loc_full[1,3,ng] = proj[1]
			endif

		; Plot range cell
			if ydata[timeind[0],ib,ng] ne 10000. then begin
				col = bytscl(ydata[timeind[0],ib,ng], min=scale[0], max=scale[1], top=251) + 3b

				polyfill, reform(fov_loc_full[0,*,ng]), $
						reform(fov_loc_full[1,*,ng]), color=col
			endif

		endif
	endfor
endfor

; Add coast
overlay_coast, coords=coords, /no_fill, rotate=rotate, hemisphere=hemisphere, coast_linecolor=0

; Plot colorbar
rrpos = !D.x_size*(position[2]-position[0])/!D.y_size/(position[3]-position[1])
if rrpos gt 1 then $
	ppos = [position[0], position[1], position[0]+(position[2]-position[0])/rrpos, position[3]] $
else if rrpos lt 1 then $
	ppos = [position[0], position[1], position[2], position[1]+(position[3]-position[1])*rrpos] $
else $
	ppos = position
bpos = [ppos[2]*1.01, ppos[1], ppos[2]*1.05, ppos[3]]
plot_colorbar, charthick=charthick, /continuous, $
 	nlevels=4, scale=scale, charsize=charsize, $
 	legend=legend, /no_rotate, /vert, $
 	position=bpos, level_format='(F5.1)', /keep_first_last_label



end
