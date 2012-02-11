pro rt_plot_map_beams, date, radar, ps=ps

common rt_data_blk
common radarinfo


; Run ray-tracing
rt_run, date, radar


; Retrieve raytracing parameters from structure
radar = rt_info.name
caldat, rt_data.juls[*,0], month, day, year, hours, minutes
tdate 	= year*10000L + month*100L + day
date = tdate[0]

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
rad_define_beams, rt_info.id, nbeams, rt_info.ngates, year, yrsec, coords='geog', $
		/normal, fov_loc_full=fov_loc_full


if keyword_set(ps) then $
	ps_open, '~/Desktop/map_beams.ps'

; Set plot area
set_format, /landscape
clear_page
map_plot_panel, 1, 1, 0, 0, date=date, coords='geog', /bar, /iso, /no_fill, xrange=[-60., 00.], yrange=[-30., 20.], coast_linecolor=1

; Plot
load_usersym, /circle
xx = fltarr(4)
yy = fltarr(4)
for it=5,16 do begin
	for ib=0,n_elements(rt_data.beam[0,*])-1 do begin
		for ig=0,n_elements(rt_data.power[0,0,*])-1 do begin
			lat = rt_data.latitude[it,ib,ig]
			lon = rt_data.longitude[it,ib,ig]
			tmp = calc_stereo_coords(lat, lon)

			plots, tmp[0], tmp[1], psym=8, symsize=.2, color=(ib*10+50)

			for p=0,3 do begin
				lat = fov_loc_full[0,p,ib,ig]
				lon = fov_loc_full[1,p,ib,ig]
				tmp = calc_stereo_coords(lat, lon)
				xx[p] = tmp[0]
				yy[p] = tmp[1]
			endfor

			plots, [xx, xx[0]], [yy, yy[0]], thick=thick
		endfor
	endfor
endfor

if keyword_set(ps) then $
	ps_close, /no_f

end