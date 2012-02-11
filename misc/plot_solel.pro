pro plot_solel, date, UT

parse_date, date, year, month, day
jd = getJD(day, month, year)

latitude = -90. + findgen(91)*2.
longitude = ((ut lt 12.) ? abs(ut-12.) : - abs(ut-12.))*360./24. -180. + findgen(181)*2.
localtime = ut*60.d +fltarr(181);+ longitude/360.d*1440.d
; print, [transpose(longitude), transpose(localtime/60.d)]
t = calcTimeJulianCent(jd + localtime/1440.d)
zone = 0.

Zenarr = fltarr(181,91)
term = 90. + fltarr(181,2)

for nlon = 0,180 do begin 
	for nlat = 0,90 do begin
		az = calcAzEl( solarZen, t[nlon], localtime[nlon], latitude[nlat], longitude[nlon], zone)
		Zenarr[nlon, nlat] = solarZen
		if abs(90.-solarZen) lt term[nlon,0] then begin
			term[nlon,0] = abs(90.-solarZen)
			term[nlon,1] = latitude[nlat]
		endif
		if nlat eq 45 then $
			print, (90.-solarZen)
	endfor
endfor
clear_page
plot_map_data, 90.-Zenarr, [0.,90.], legend='Elevation angle', $
	lats=latitude, lons=longitude

oplot, longitude, term[*,1], thick=2
; trace2iono, 

plot_colorbar, /vert, charthick=2, /bar, /continuous, $
	scale=[0.,90.], position=bpos, charsize=charsize, $
	legend='Elevation angle', nlevels=6, /no_rotate

end