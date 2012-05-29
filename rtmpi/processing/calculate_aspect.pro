pro calculate_aspect, radar, date, corr=corr, disp=disp, force=force, ps=ps

common radarinfo

; Read radar location and boresight
parse_date, date, year, month, day
radID = where(network.code[0,*] eq radar)
tval = TimeYMDHMSToEpoch(year, month, day, 0, 0, 0)
for s=0,31 do begin
	if (network[radID].site[s].tval eq -1) then break
	if (network[radID].site[s].tval ge tval) then break
endfor
radarsite = network[radID].site[s]
lat = radarsite.geolat
lon = radarsite.geolon

Rav = 6370.

; Beams and azimuths
nbeams = radarsite.maxbeam
abeams = indgen(nbeams)
azim = fltarr(nbeams)
for n=0,nbeams-1 do $
	azim[n] = radarsite.boresite - rt_get_azim(radar, abeams[n], date)

; Elevation and range
aranges = 180. + findgen(50)*45.
aelev = findgen(121)*.5
nranges = n_elements(aranges)
nelevs = n_elements(aelev)
dran = aranges[1] - aranges[0]
del = aelev[1] - aelev[0]

; Calculate aspect angle in each range-beam-elevation cell
if ~file_test('~/tmp/rt/'+radar+'/aspect.dat', /read) or keyword_set(force) then begin
	aspect = fltarr(nbeams,nranges,nelevs)
	for ib=0,nbeams-1 do begin
		for ir=0,nranges-1 do begin
			for iel=0,nelevs-1 do begin
				; ALtitude, elevation at current position and azimuth
				alt = sqrt(Rav^2 + aranges[ir]^2 - 2.*Rav*aranges[ir]*cos(!pi/2.+aelev[iel]*!dtor))
				ttht = asin(aranges[ir]/alt*sin(!pi/2.+aelev[iel]*!dtor))
				elevproj = aelev[iel]*!dtor + asin(aranges[ir]*cos(aelev[iel]*!dtor)/alt)
				if keyword_set(corr) then $
					tazim = azim[ib]/abs(azim[ib])* acos( sqrt( (cos(azim[ib]*!dtor)^2 - sin(aelev[iel]*!dtor)^2) / cos(aelev[iel]*!dtor)^2 ) ) * !radeg $
				else $
					tazim = azim[ib]

				; wave vector
				kx = cos(elevproj)
				kz = sin(elevproj)

				; Position
				is = calc_pos(lat, lon, 0., tazim, aranges[ir], aelev[iel], latiout, longiout)

				; Dip and declination at this position
				igrf_run, date, lati=latiout, longi=longiout, alti=[alt,alt,1.], diparr=dip, decarr=dec
; 				if iel eq 10. then print, tazim, azim[ib], alt-Rav, ttht*Rav, dip[0], dec[0]
				middip = dip[0]
				middec = dec[0]

				; calculate vector magnetic field
				Bx = cos(-middip*!dtor) * cos(tazim*!dtor - middec*!dtor)
				Bz = sin(-middip*!dtor)
	; 			reldip = abs(acos(Bx/sqrt(Bx^2+Bz^2))*!radeg)

	; 			aspect[ib,ir,iel] = 90.-abs(90. - (aelev[iel] + reldip + ttht*!radeg))
				aspect[ib,ir,iel] = 90. - abs(90. - acos( kx*Bx + kz*Bz )*!radeg)
				
			endfor
		endfor
	endfor

	; Write to files
	openw, unit, '~/tmp/rt/'+radar+'/aspect.dat', /get_lun
	writeu, unit, nbeams, nranges, nelevs
	writeu, unit, abeams, aranges, aelev
	writeu, unit, aspect
	free_lun, unit

endif

; Plot
if keyword_set(disp) then begin
	openr, unit, '~/tmp/rt/'+radar+'/aspect.dat', /get_lun
	aspnbeams = 0L
	aspnranges = 0L
	aspnelevs = 0L
	readu, unit, aspnbeams, aspnranges, aspnelevs
	aspabeams = intarr(aspnbeams)
	asparanges = fltarr(aspnranges)
	aspaelev = fltarr(aspnelevs)
	readu, unit, aspabeams, asparanges, aspaelev
	aspect = fltarr(aspnbeams,aspnranges,aspnelevs)
	readu, unit, aspect
	free_lun, unit

	if keyword_set(ps) then $
		ps_open, '~/Desktop/aspect.ps'

	ib = 0
	plot, [0.,2000.], [0., 60.], /nodata, charsize=charsize, position=[.1,.1, .45, .9], $
		xtitle='Slant range [km]', xstyle=1, xticklen=1, xgridstyle=1, $
		ytitle='Elevation [deg]', ystyle=1, yticklen=1, ygridstyle=1
	contour, aspect[ib,*,*], aranges, aelev, /overplot, levels=[60.,70., 80., 85., 89.], c_labels=1b+bytarr(5), c_charsize=charsize
	xyouts, 1000., 55., 'Beam '+strtrim(ib,2), align=.5

	ib = nbeams/2-1
	plot, [0.,2000.], [0., 60.], /nodata, charsize=charsize, position=[.55,.1, .9, .9], $
		xtitle='Slant range [km]', xstyle=1, xticklen=1, xgridstyle=1, $
		ytitle='Elevation [deg]', ystyle=1, yticklen=1, ygridstyle=1
	contour, aspect[ib,*,*], aranges, aelev, /overplot, levels=[60.,70., 80., 85., 89.], c_labels=1b+bytarr(5), c_charsize=charsize
	xyouts, 1000., 55., 'Beam '+strtrim(ib,2), align=.5

	if keyword_set(ps) then $
		ps_close, /no_f
endif

end