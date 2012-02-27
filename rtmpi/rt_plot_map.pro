pro rt_plot_map, time, ground=ground, ionos=ionos, param=param

common rt_data_blk
common radarinfo

; Set plot parameters
if ~keyword_set(param) then $
	param = 'power'

; Retrieve raytracing parameters from structure
radar = rt_info.name
caldat, rt_data.juls[*,0], month, day, year, hours, minutes
tdate 	= year*10000L + month*100L + day
date = tdate[0]

; Find time index
parse_time, time, hour, minute
if ~keyword_set(date) then $
	date = tdate[0]
timeind = where(tdate eq date and hours eq hour and minutes eq minute)
juls = julday(month[timeind], day[timeind], year[timeind], hours[timeind], minutes[timeind])

; Find hardware parameters
radID = where(network.ID eq rt_info.id)
tval = TimeYMDHMSToEpoch(year[0], month[0], day[0], 0, 0, 0)
for s=0,31 do begin
	if (network[radID].site[s].tval eq -1) then break
    	if (network[radID].site[s].tval ge tval) then break
endfor
radarsite = network[radID].site[s]
nbeams = radarsite.maxbeam
hbw = radarsite.bmsep/2.

; Select parameter to plot
s = execute('ydata = rt_data.'+param)
case param of
	'power':	begin
						legend = 'Power'
						scale = [-30., 0]
					end
	'elevation':	begin
						legend = 'Elevation'
						scale = [10., 45.]
					end
	'altitude':		begin
						legend = 'Altitude [km]'
						scale = [100., 500.]
					end
	'valtitude':	begin
						legend = 'Virtual height [km]'
						scale = [100., 500.]
					end
	'nr':			begin
						legend = 'Refractive index'
						scale = [.8, 1.]
					end
endcase

; Set plot area
rotate = 90.+rt_info.glon
magco = cnvcoord(rt_info.glat, rt_info.glon, 0.)
proj = calc_stereo_coords(magco[0], magco[1], rotate=rotate)
xrange = proj[0] + [-20,20] 
yrange = proj[1] + rt_info.glat/abs(rt_info.glat)*[-10,30] 

; Start plotting
set_format, /landscape
clear_page
position = define_panel(1,1,0,0)
map_plot_panel, 1, 1, 0, 0, date=date, coords='magn', /iso, /no_fill, yrange=yrange, xrange=xrange, rotate=rotate
load_usersym, /circle
plots, proj[0], proj[1], psym=8, symsize=.5

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

; Add fov
; overlay_fov, names=radar, coords='magn', date=date, /no_fill, nranges=rt_info.ngates

; overlays wallops flight center
if keyword_set(rocket) then begin
    walfc = [37.93, -75.47,0.]
    magco = cnvcoord(walfc[0], walfc[1], 0.)
    proj = calc_stereo_coords(magco[0], magco[1], rotate=rotate)
    load_usersym, /circle
    plots, proj[0], proj[1], psym=8, symsize=.5
    is = calc_pos(walfc[0], walfc[1], 0., 90., 300., 0., latiout, longiout)
    magco = cnvcoord(latiout, longiout, 0.)
    projaz = calc_stereo_coords(magco[0], magco[1], rotate=rotate)
    plots, [proj[0],projaz[0]], [proj[1],projaz[1]]
    is = calc_pos(walfc[0], walfc[1], 0., 160., 300., 0., latiout, longiout)
    magco = cnvcoord(latiout, longiout, 0.)
    projaz = calc_stereo_coords(magco[0], magco[1], rotate=rotate)
    plots, [proj[0],projaz[0]], [proj[1],projaz[1]]
endif

;map_plot_panel, 1, 1, 0, 0, date=date, coords='magn', /bar, /iso, /no_fill, yrange=[-50., 0.]
overlay_coast, coords='magn', /no_fill, rotate=rotate
; Plot colorbar
plot_colorbar, 1, 1, 0, 0, charthick=charthick, /continuous, $
	nlevels=4, scale=scale, charsize=!P.charsize, $
	legend=legend, /no_rotate, /horizontal, position=[.3, position[3]*1.01, .74, position[3]*1.03], $
	level_format='(F5.1)', /keep_first_last_label



end
