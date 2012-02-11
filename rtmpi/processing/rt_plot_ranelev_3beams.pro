pro	rt_plot_ranelev_3beams, time, date=date, xrange=xrange, yrange=yrange

common rt_data_blk


Rav = 6370.

; Set beam number
nbeams = n_elements(rt_data.beam[0,*])
beams = [0,floor((nbeams-1)/2.), nbeams-1]
binds = where(rt_data.beam[0,*] eq beams)

; Retrieve raytracing parameters from structure
radar = rt_info.name
caldat, rt_data.juls[*,0], month, day, year, hours, minutes
tdate 	= year*10000L + month*100L + day

; Find time index
parse_time, time, hour, minute
if ~keyword_set(date) then $
	date = tdate[0]
timeind = where(tdate eq date and hours eq hour and minutes eq minute)
juls = julday(month[timeind], day[timeind], year[timeind], hours[timeind], minutes[timeind])
it = timeind[0]

set_format, /sardines, /tokyo
ps_open, '~/Desktop/ranelev3beams_'+radar+'_'+STRTRIM(date,2)+'_'+ $
		STRTRIM(STRING(time,format='(I04)'),2)+rt_info.timez+'.ps', /no_init

; Set plot limits
if ~keyword_set(xrange) then $
	xrange = [0,40]*45. + [0., 180.]
if ~keyword_set(yrange) then $
	yrange = [5., 40.]
title = STRMID(format_juldate(rt_data.juls[timeind]),0,17)
subtitle = 'Radar: '+rt_info.name+', Beams ['+STRTRIM(beams[0],2)+','+STRTRIM(beams[1],2)+','+STRTRIM(beams[2],2)+'], Freq. '+$
	STRTRIM(string(rt_data.tfreq[timeind],format='(F5.2)'),2)+' MHz'
ytitle = 'Elevation'
xtitle = 'Slant range [km]'
scale = [85.,90.]
xyouts, .5, .9, title+'!C'+subtitle, align=.5, charsize=charsize, /normal

for iib=0,2 do begin
	ib = beams[iib]
	if iib gt 0 then begin
		ytitle = ' '
		ytickname = replicate(' ',60)
	endif

	; Start plot
	charsize = get_charsize(3,2)
	position = define_panel(3,2,iib,0, /bar)
	plot, xrange, yrange, /nodata, xstyle=5, ystyle=5, position=position, $
			xtickname=replicate(' ',60), ytickname=ytickname, xtitle='', ytitle=ytitle, $
			xticklen=1, xgridstyle=1, yticklen=1, ygridstyle=1, charsize=charsize

	aranges = 180. + findgen(40)*45.
	aelev = yrange[0] + findgen((yrange[1]-yrange[0])/.5)*.5
	nranges = n_elements(aranges)
	nelevs = n_elements(aelev)
	dran = aranges[1] - aranges[0]
	del = aelev[1] - aelev[0]

	; latitude/longitude distribution (expressed as angle from radar with Earth center)
	ndip = n_elements(rt_data.dip[0,0,0,*])
	thtdip = findgen(ndip)*2500./Rav/ndip
	dthtdip = thtdip[1]-thtdip[0]

	aspect = fltarr(nranges,nelevs)
	for ir=0,nranges-1 do begin
		for iel=0,nelevs-1 do begin
			alt = sqrt(Rav^2 + aranges[ir]^2 - 2.*Rav*aranges[ir]*cos(!pi/2.+aelev[iel]*!dtor))
			ttht = asin(aranges[ir]/alt*sin(!pi/2.+aelev[iel]*!dtor))
			thtind = (ttht - thtdip[0])/dthtdip

			; Dip and declination at this position
			middip = rt_data.dip[it,ib,0,thtind]
			middec = rt_data.dip[it,ib,1,thtind]

			; calculate vector magnetic field
			Bx = cos(-middip*!dtor) * cos(rt_data.azim[it,ib]*!dtor - middec*!dtor)
			Bz = sin(-middip*!dtor)
			reldip = abs(acos(Bx/sqrt(Bx^2+Bz^2))*!radeg)

			aspect[ir,iel] = 90.-abs(90. - (aelev[iel] + reldip + ttht*!radeg))
			col = bytscl(aspect[ir,iel], min=scale[0], max=scale[1], top=250) + 3b

			polyfill, aranges[ir]*[1,1,0,0] + (aranges[ir]+dran)*[0,0,1,1], $
						aelev[iel]*[1,0,0,1] + (aelev[iel]+del)*[0,1,1,0], $
						col=col
		endfor
	endfor


	; Over plot weighed scatter location
	ydata = rt_data.elevation
	oplot, findgen(rt_info.ngates+1)*45.+180., ydata[timeind,ib,*], psym=4, thick=4, symsize=1, min_val=.1

	plot, xrange, yrange, /nodata, xstyle=1, ystyle=1, position=position, $
			xtickname=replicate(' ',60), ytickname=ytickname, xtitle='', ytitle=ytitle, $
			xticklen=1, xgridstyle=1, yticklen=1, ygridstyle=1, $
			charsize=charsize

	if iib eq 2 then begin
		plot_colorbar, 1,2,0,0, /vert, charthick=charthick, /continuous, $
			nlevels=5, scale=scale, position=bpos, charsize=charsize, $
			legend='Aspect angle', /no_rotate, $
			level_format='(F5.0)', /keep_first_last_label
	endif



	; Plot ray-tracing results
	position = define_panel(3,2,iib,1, /bar)
	plot, xrange, yrange, /nodata, xstyle=5, ystyle=5, position=position

	; Test for data availability
	rt_read_rays, date, radar, time, radpos=radpos, thtpos=thtpos, grppth=grppth, raysteps=raysteps, code=code
	if ~code then $
		return

	; If data is present, limit it to selected beam
	radpos = reform(radpos[ib,*,*])
	thtpos = reform(thtpos[ib,*,*])
	grppth = reform(grppth[ib,*,*])
	raysteps = reform(raysteps[ib,*])
	xdata = grppth*1e-3

	; Count element of matrix
	nrays = n_elements(radpos[*,0])

	; Set tolerance (in degree) for how much deviation from perfect aspect condition is allowed
	tol = 1.

	; latitude/longitude distribution (expressed as angle from radar with Earth center)
	ndip = n_elements(rt_data.dip[timeind,ib,0,*])
	thtdip = findgen(ndip)*2500./Rav/ndip

	for nr=0,nrays-1 do begin
		nrsteps = raysteps[nr]
		for ns=1,nrsteps-1 do begin
			if grppth[nr,ns-1] gt 180e3 and xdata[nr,ns] le xrange[1] then begin

				; Calculate k vector
				kx = radpos[nr,ns]*sin(thtpos[nr,ns]-thtpos[nr,ns-1])
				kz = radpos[nr,ns]*cos(thtpos[nr,ns]-thtpos[nr,ns-1]) - radpos[nr,ns-1]
				kvect = sqrt( kx^2. + kz^2. )

				; Plot coordinates
				xx = [xdata[nr,ns-1], xdata[nr,ns]]
				zz = [1., 1.]*(rt_info.elev_beg + nr*rt_info.elev_stp)
				if zz[1] ge yrange[1] then $
					continue
				
				; Middle of the step: position and index in B grid
				midtht = (thtpos[nr,ns]-thtpos[nr,ns-1])/2. + thtpos[nr,ns-1]
				diff = min(midtht-thtdip, thtind, /abs)
				
				; Dip and declination at this position
				middip = rt_data.dip[timeind,ib,0,thtind]
				middec = rt_data.dip[timeind,ib,1,thtind]

				; calculate vector magnetic field
				Bx = cos(-middip*!dtor) * cos(rt_data.azim[timeind[0],ib]*!dtor - middec*!dtor)
				Bz = sin(-middip*!dtor)
				
				; calculate cosine of aspect angle
				cos_aspect = (Bx*kx + Bz*kz)/kvect
				
				if abs(cos_aspect) le cos(!pi/2. - tol*!dtor) then begin
					plots, xx, zz, color=200, thick=2
	; 					print, nr*.1+10., ns-1, (grppth[nr,ns-1]*1e-3-180.)/45., radpos[nr,ns-1]*1e-3-Rav
				endif

			endif
		; end ray steps loop
		endfor
	; end rays loop
	endfor

	contour, aspect, aranges, aelev, /overplot, levels=[70., 80., 85., 89.], c_labels=1b+bytarr(4)


	; Over plot weighed scatter location
	ydata = rt_data.elevation
	oplot, findgen(rt_info.ngates+1)*45.+180., ydata[timeind,ib,*], psym=4, thick=2, symsize=1, min_val=.1

	; Plot axis
	plot, xrange, yrange, /nodata, xstyle=1, ystyle=1, charsize=charsize, $
		xticklen=1, xgridstyle=1, yticklen=1, ygridstyle=1, $
		xtitle=xtitle, ytitle=ytitle, position=position
endfor

ps_close

end