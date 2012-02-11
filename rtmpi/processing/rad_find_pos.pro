pro rad_find_pos, lat, lon, which_radar=which_radar, which_beam=which_beam

common radarinfo

if lat gt 0. then $
	hemisphere = 1. $
else $
	hemisphere = -1.

which_radar = ''
which_beam = 0
nr = 0L
for ir=1,n_elements(network)-1 do begin
	radarsite = network[ir].site[where(network[ir].site.tval eq -1)]
	; Only look at radars in the same hemisphere
	if radarsite.geolat*hemisphere gt 0. then begin
		dist = calc_azdist([radarsite.geolat, radarsite.geolon], [lat, lon], azimuth=az)
		; Calculate radar extreme azimuths
		offset = radarsite.maxbeam/2. - .5
		b0 = radarsite.boresite
		sradaz = b0 + (0 - offset)*radarsite.bmsep
		fradaz = b0 + (radarsite.maxbeam-1 - offset)*radarsite.bmsep
		radaz = b0 + (indgen(radarsite.maxbeam) - offset)*radarsite.bmsep
		if dist ge 200. and dist le 5000.  and az ge sradaz-1. and az le fradaz+1. then begin
			minaz = min(abs(az-radaz), min_ind)
			which_beam = [which_beam, min_ind]
			which_radar = [which_radar, network[ir].code[0]]
			nr = nr + 1
			; print, which_radar[nr], which_beam[nr], az, radaz[min_ind], sradaz, fradaz
		endif
	endif
endfor

if nr ge 1 then begin
	which_radar = which_radar[1:*]
	which_beam = which_beam[1:*]
endif

end
