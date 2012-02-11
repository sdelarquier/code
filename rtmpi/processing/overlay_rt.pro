pro overlay_rt, time, thick=thick, scale=scale


common rt_data_blk
common radarinfo

if ~keyword_set(scale) then $
	scale = [0., 1.]


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

xx = fltarr(4)
yy = fltarr(4)
for ib=0,nbeams-1 do begin
	for ig=0,rt_info.ngates-1 do begin
		if rt_data.power[timeind[0],ib,ig] gt 0. then begin
			for p=0,3 do begin
				lat = fov_loc_full[0,p,ib,ig]
				lon = fov_loc_full[1,p,ib,ig]
				tmp = calc_stereo_coords(lat, lon)
				xx[p] = tmp[0]
				yy[p] = tmp[1]
			endfor
			
			col = bytscl(rt_data.power[timeind[0],ib,ig], min=scale[0], max=scale[1], top=250) + 2b

			polyfill, xx, yy, color=col
	; 		plots, [xx, xx[0]], [yy, yy[0]], thick=thick
	; 			print, ib, ig, xx, yy
		endif
	endfor
endfor


end