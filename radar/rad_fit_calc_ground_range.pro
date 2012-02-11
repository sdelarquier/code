pro rad_calc_ground_range, date=date, radar=radar, beam=beam, fov_loc_center=foc_loc_center

common radarinfo
common rad_data_blk

if ~keyword_set(date) then begin
	juldate = systime(/julian, /utc)
	caldat, juldate, month, day, year
	date = year*10000L + month*100L + day
endif

if ~keyword_set(radar) then begin
	data_index = rad_fit_get_data_index()
	if data_index eq -1 then begin
		prinfo, 'No data present, you must specify a radar'
		return
	endif
	radar = (*rad_fit_info[data_index]).code
endif

if ~keyword_set(beam) then $
	beam = 7

radarind = where(network.code[0,*] eq radar)
id = network[radarind].id
nbeams = network[radarind].site[where(network[radarind].site.tval eq -1)].maxbeam
ngates = network[radarind].site[where(network[radarind].site.tval eq -1)].maxrange
bmsep = network[radarind].site[where(network[radarind].site.tval eq -1)].bmsep
txlat = network[radarind].site[where(network[radarind].site.tval eq -1)].geolat
txlon = network[radarind].site[where(network[radarind].site.tval eq -1)].geolon

parse_date, date, year, month, day
juldate = julday(month, day, year)
yrsec = (juldate-julday(1,1,year,0,0,0))*86400.d

rad_define_beams, id, nbeams, ngates, year, yrsec, coords='geog', height=300., bmsep=bmsep, /normal, fov_loc_center=fov_loc_center


; Convert radar position to geocentric and correct Earth radius
RadarGeoTGC, 1, txlat, txlon, Re, txglat, txglon, del

; Find Earth radius in radar fov
RadarGeoTGC, 1, fov_loc_center[0,beam,*], fov_loc_center[0,beam,*], fov_Re, fov_glat, fov_glon

; Convert to global cartesian coordinates
txx = Re * sin(!pi/2. - txglat*!dtor) * cos(txglon*!dtor)
txy = Re * sin(!pi/2. - txglat*!dtor) * sin(txglon*!dtor)
txz = Re * cos(!pi/2. - txglat*!dtor)

fov_x = fov_Re * sin(!pi/2. - fov_glat*!dtor) * cos(fov_glon*!dtor)
fov_y = fov_Re * sin(!pi/2. - fov_glat*!dtor) * sin(fov_glon*!dtor)
fov_z = fov_Re * cos(!pi/2. - fov_glat*!dtor)

; Calculate dot product to find angle with respect to radar position
pdot = txx*fov_x + txy*fov_y + txz*fov_z
tht = acos( pdot//Re/fov_Re )*!radeg

; Find ground range
Rgnd = fov_Re * tht

end