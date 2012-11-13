pro radarFoVsS, date, filename, ignore_radar=ignore_radar, nops=nops, fillfov=fillfov, showplanned=showplanned, rotate=rotate, annotate=annotate, range=range, bardate=bardate, legend=legend

common radarinfo

coords = 'magn'
; Force some radars to be ignored
if ~keyword_set(ignore_radar) then $
	ignore_radar = ''
ignore_radar = [ignore_radar, 'svb']

if date gt 20071215 and date lt 20110224 then $
	ignore_radar = [ignore_radar, 'hal']

if keyword_set(range) then begin
	xrange = range
	yrange = range
endif else begin
	xrange = 0
	yrange = 0
endelse

; Find radars
ajul = calc_jul(date,1200)
caldat, ajul, mm, dd, year
tval = TimeYMDHMSToEpoch(year, mm, dd, 12, 0, 0)
nrad = 0L
radarS = ['']
radarN = ['']
radarSlat = [0]
radarNlat = [0]
radIDN = [0]
radIDS = [0]
for ir=1,n_elements(network)-1 do begin
	; Ignore some radars
	if keyword_set(ignore_radar) then $
		ign = where(ignore_radar eq network[ir].code[0], ccign) $
	else $
		ccign = -1
	if ccign gt 0 then continue
	if ~keyword_set(showplanned) and network[ir].status eq 0 then continue
	if tval lt network[ir].st_time then continue
	if tval gt network[ir].ed_time then continue
	for s=0,31 do begin
		if (network[ir].site[s].tval eq -1) then break
		if (network[ir].site[s].tval ge tval) then break
	endfor
	radarsite = network[ir].site[s]
	if radarsite.geolat lt 0 then begin
		radarS = [radarS, network[ir].code[0]]
		magpos = cnvcoord(radarsite.geolat, radarsite.geolon, 0.)
		radarSlat = [radarSlat, abs(magpos[0])]
		radIDS = [radIDS, network[ir].status]
	endif else begin
		radarN = [radarN, network[ir].code[0]]
		magpos = cnvcoord(radarsite.geolat, radarsite.geolon, 0.)
		radarNlat = [radarNlat, abs(magpos[0])]
		radIDN = [radIDN, network[ir].status]
	endelse
	nrad += 1
endfor
if n_elements(radarN) gt 1 then begin
	radarN = radarN[1:*]
	radarNlat = radarNlat[1:*]
	radIDN = radIDN[1:*]
endif
if n_elements(radarS) gt 1 then begin
	radarS = radarS[1:*]
	radarSlat = radarSlat[1:*]
	radIDS = radIDS[1:*]
endif

; Sort radars by ascending geomagnetic latitude
radarN = radarN[sort(radarNlat)]
radIDN = radIDN[sort(radarNlat)]
radarNlat = radarNlat[sort(radarNlat)]
radarS = radarS[sort(radarSlat)]
radIDS = radIDS[sort(radarSlat)]
radarSlat = radarSlat[sort(radarSlat)]

; Select field of view color if required
newcol	= 200
midlcol = 240
highcol = 100
polccol = 150
if keyword_set(fillfov) then begin
	radarNcol = fltarr(n_elements(radarN))
	midl = where(radarNlat le 50., ccmidl)
	if ccmidl gt 0 then radarNcol[midl] = midlcol
	high = where(radarNlat gt 50. and radarNlat le 70., cchigh)
	if cchigh gt 0 then radarNcol[high] = highcol
	polc = where(radarNlat gt 70., ccpolc)
	if ccpolc gt 0 then radarNcol[polc] = polccol
	newr = where(radIDN eq 0, ccnewr)
	if ccnewr gt 0 then radarNcol[newr] = newcol

	radarScol = fltarr(n_elements(radarS))
	midl = where(radarSlat le 50., ccmidl)
	if ccmidl gt 0 then radarScol[midl] = midlcol
	high = where(radarSlat gt 50. and radarSlat le 70., cchigh)
	if cchigh gt 0 then radarScol[high] = highcol
	polc = where(radarSlat gt 70., ccpolc)
	if ccpolc gt 0 then radarScol[polc] = polccol
	newr = where(radIDS eq 0, ccnewr)
	if ccnewr gt 0 then radarScol[newr] = newcol

	if n_elements(fillfov) gt 1 then begin
                radarNcol = radarNcol*fillfov[0:n_elements(radarNcol)-1]
		if n_elements(radarNcol) lt n_elements(fillfov) then $
                  radarScol = radarScol*fillfov[n_elements(radarNcol):*]
	endif
endif

; Plotting
set_format, /portrait, /sardines, /tokyo
if nrad gt 0 then begin

		; List radars found with beam and distance from point
		print, '!#radars=', strtrim(string(nrad,format='(I3)'),2), format='(A9,A3)'

		; Open postscript unless told otherwise
		if ~keyword_set(nops) then $
			ps_open, '/var/www/data/plots/'+filename+'.ps'
		set_format, /landscape, /sardines, /tokyo
		clear_page

		; Plot radars
		loadct, 0
		ttxrange = xrange
		ttyrange = yrange
		if (radarS[0] ne '') then begin
			print, '-Southern hemisphere-'
			for ir=0,n_elements(radarS)-1 do begin
					print, '!Radar found: ', radarS[ir], $
							format='(A13,2x,A3)'
			endfor
			rad_plot_fov, radarS, date=date, beam=beam, coords=coords, rotate=rotate, xpanel=[1,0], fovcolor=radarScol, fillannotate=annotate, $
				xrange=ttxrange, yrange=ttyrange
		endif else $
			map_plot_panel, 1, 1, 0, 0, coords=coords, /iso, yrange=ttyrange, xrange=ttxrange, hemi=-1, $
					coast_linecolor=150, grid_linecolor=200, lake_fillcolor=255, rotate=rotate
		ttxrange = xrange
		ttyrange = yrange

		xyouts, .45, .86, STRMID(format_juldate(ajul),0,2), orientation=0, /normal, charsize=1.2
		xyouts, .49, .86, '/', orientation=0, /normal, charsize=1.2
		xyouts, .51, .86, STRMID(format_juldate(ajul),3,3), orientation=0, /normal, charsize=1.2
		xyouts, .56, .86, '/', orientation=0, /normal, charsize=1.2
		xyouts, .58, .86, STRMID(format_juldate(ajul),7,4), orientation=0, /normal, charsize=1.2

		; Add progress bar if required
		if keyword_set(bardate) then begin
			today = systime(/julian, /utc)
			caldat, today, month, day, year
			month = 0
			year = 2013
			start_year = 2005
			step = 1
			plot, [start_year, year + (month+1.)/12.], [0,1], /nodata, position=[.25, .9, .8, .91], $
				xstyle=1, ystyle=1, ytickname=replicate(' ', 60), yticks=1, $
				xticks=round( (year-start_year)/step )-1, xtickv=2005.+indgen(round( (year-start_year)/step ))*step, xminor=1, xticklen=1., xtickname=replicate(' ', 60)
			axis, /xax, xticks=round( (year-start_year)/step )-1, xtickv=2005.+indgen(round( (year-start_year)/step ))*step, xminor=step, xticklen=.2, charsize=.8
			parse_date, date, year, month, day
			polyfill, [start_year,  year + (month+1.)/12.,  year + (month+1.)/12., start_year], [0,0,1,1], col=0
		endif

		; Add legend if required
		if keyword_set(legend) then begin
			loadct, 13
			ybase = .04
			; Mid latitudes
			plot, [0,1], [0,1], xstyle=5, ystyle=5, xtickname=replicate(' ',60), ytickname=replicate(' ',60), position=[.45, ybase, .5, ybase+.03], /nodata
			polyfill, [0,1,1,0], [0,0,1,1], col=midlcol
			plot, [0,1], [0,1], xstyle=1, ystyle=1, xtickname=replicate(' ',60), ytickname=replicate(' ',60), position=[.45, ybase, .5, ybase+.03], /nodata
			xyouts, .51, ybase+.01, 'Mid-latitude', /normal
			; High latitudes
			plot, [0,1], [0,1], xstyle=5, ystyle=5, xtickname=replicate(' ',60), ytickname=replicate(' ',60), position=[.2, ybase, .25, ybase+.03], /nodata
			polyfill, [0,1,1,0], [0,0,1,1], col=highcol
			plot, [0,1], [0,1], xstyle=1, ystyle=1, xtickname=replicate(' ',60), ytickname=replicate(' ',60), position=[.2, ybase, .25, ybase+.03], /nodata
			xyouts, .26, ybase+.01, 'High-latitude', /normal
			; Polar latitudes
			plot, [0,1], [0,1], xstyle=5, ystyle=5, xtickname=replicate(' ',60), ytickname=replicate(' ',60), position=[.7, ybase, .75, ybase+.03], /nodata
			polyfill, [0,1,1,0], [0,0,1,1], col=polccol
			plot, [0,1], [0,1], xstyle=1, ystyle=1, xtickname=replicate(' ',60), ytickname=replicate(' ',60), position=[.7, ybase, .75, ybase+.03], /nodata
			xyouts, .76, ybase+.01, 'Polar cap', /normal
		endif

		; Close postscript unless told otherwise
		if ~keyword_set(nops) then $
			ps_close, /no_filename, /no_init

endif else begin
		; Open postscript unless told otherwise
		if ~keyword_set(nops) then $
			ps_open, '/var/www/data/plots/'+filename+'.ps'
		set_format, /landscape
		clear_page

		; Only plot position of point of interest
		loadct, 0
		map_plot_panel, 1, 1, 0, 0, coords=coords, /iso, yrange=range, xrange=range, hemi=1, $
			coast_linecolor=150, grid_linecolor=200, lake_fillcolor=255, rotate=rotate
		map_plot_panel, 1, 1, 1, 0, coords=coords, /iso, yrange=range, xrange=range, hemi=-1, $
			coast_linecolor=150, grid_linecolor=200, lake_fillcolor=255, rotate=rotate

		xyouts, .45, .76, STRMID(format_juldate(ajul),0,2), orientation=0, /normal, charsize=1.2
		xyouts, .49, .76, '/', orientation=0, /normal, charsize=1.2
		xyouts, .51, .76, STRMID(format_juldate(ajul),3,3), orientation=0, /normal, charsize=1.2
		xyouts, .56, .76, '/', orientation=0, /normal, charsize=1.2
		xyouts, .58, .76, STRMID(format_juldate(ajul),7,4), orientation=0, /normal, charsize=1.2

		; Add progress bar if required
		if keyword_set(bardate) then begin
			today = systime(/julian, /utc)
			caldat, today, month, day, year
			start_year = 1983
			plot, [start_year, year + (month+1.)/12.], [0,1], /nodata, position=[.15, .8, .85, .81], $
				xstyle=1, ystyle=1, ytickname=replicate(' ', 60), yticks=1, $
				xticks=round( (year-start_year)/5. )-1, xtickv=1985.+indgen(round( (year-start_year)/5. ))*5., xminor=5, xticklen=1., xtickname=replicate(' ', 60)
			axis, /xax, xticks=round( (year-start_year)/5. )-1, xtickv=1985.+indgen(round( (year-start_year)/5. ))*5., xminor=5, xticklen=.2, charsize=.8
			parse_date, date, year, month, day
			polyfill, [start_year,  year + (month+1.)/12.,  year + (month+1.)/12., start_year], [0,0,1,1], col=0
		endif

		; Add legend if required
		if keyword_set(legend) then begin
			loadct, 13
			ybase = .14
			; Mid latitudes
			plot, [0,1], [0,1], xstyle=5, ystyle=5, xtickname=replicate(' ',60), ytickname=replicate(' ',60), position=[.45, ybase, .5, ybase+.03], /nodata
			polyfill, [0,1,1,0], [0,0,1,1], col=midlcol
			plot, [0,1], [0,1], xstyle=1, ystyle=1, xtickname=replicate(' ',60), ytickname=replicate(' ',60), position=[.45, ybase, .5, ybase+.03], /nodata
			xyouts, .51, ybase+.01, 'Mid-latitude', /normal
			; High latitudes
			plot, [0,1], [0,1], xstyle=5, ystyle=5, xtickname=replicate(' ',60), ytickname=replicate(' ',60), position=[.15, ybase, .2, ybase+.03], /nodata
			polyfill, [0,1,1,0], [0,0,1,1], col=highcol
			plot, [0,1], [0,1], xstyle=1, ystyle=1, xtickname=replicate(' ',60), ytickname=replicate(' ',60), position=[.15, ybase, .2, ybase+.03], /nodata
			xyouts, .21, ybase+.01, 'High-latitude', /normal
			; Polar latitudes
			plot, [0,1], [0,1], xstyle=5, ystyle=5, xtickname=replicate(' ',60), ytickname=replicate(' ',60), position=[.75, ybase, .8, ybase+.03], /nodata
			polyfill, [0,1,1,0], [0,0,1,1], col=polccol
			plot, [0,1], [0,1], xstyle=1, ystyle=1, xtickname=replicate(' ',60), ytickname=replicate(' ',60), position=[.75, ybase, .8, ybase+.03], /nodata
			xyouts, .81, ybase+.01, 'Polar cap', /normal
		endif

		; Close postscript unless told otherwise
		if ~keyword_set(nops) then $
			ps_close, /no_filename, /no_init
endelse

; Show ignored radars
for ii=0,n_elements(ignore_radar)-1 do $
	print, 'Radar ignor: ',ignore_radar[ii]

end
