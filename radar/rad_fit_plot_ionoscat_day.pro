pro rad_fit_plot_ionoscat_day, date, ps=ps


common rad_data_blk
common rt_data_blk

Rav = 6370.

; Set time
time=[0,1200]

; Set begining date
; date = 20101118

if keyword_set(ps) then $
	ps_open, '~/Desktop/radscat.ps'

; Set plotting area
xrange = [0., 60.]
yrange = [0., 60.]
title = format_date(date,/human)+', bks'

; Initialize arrays
nelev_steps = (yrange[1]-yrange[0])/0.5
elev_steps = yrange[0] + findgen(nelev_steps)*(yrange[1]-yrange[0])/nelev_steps
elevhist = fltarr(16,nelev_steps,100)

; Read radar data
rad_fit_read, date, 'bks', time=time, /filter, /ajground, /catfile, catpath='/tmp/'

; get index for current data
data_index = rad_fit_get_data_index()
if data_index eq -1 then $
	return

rad_fit_calculate_elevation, date=date, time=time, /overwrite, tdiff=-.324, interfer_pos=[0.,-58.9,-2.7], scan_boresite_offset=8.

; Set time limits
sfjul, date, time, sjul, fjul

for ib=0,15 do begin
	binds = where((*rad_fit_data[data_index]).beam eq ib and $
				(*rad_fit_data[data_index]).juls ge sjul and $
				(*rad_fit_data[data_index]).juls le fjul and $
				(*rad_fit_data[data_index]).tfreq ge 10e3 and $
				(*rad_fit_data[data_index]).tfreq le 12e3, ccinds)
	if ccinds le 0 then $
		return

	elev = (*rad_fit_data[data_index]).elevation[binds,*]
	power = (*rad_fit_data[data_index]).power[binds,*]
	scat = (*rad_fit_data[data_index]).gscatter[binds,*]
	for ng=5,(*rad_fit_info[data_index]).ngates-1 do begin
		if ng lt xrange[1] then begin
			for nel=0,nelev_steps-2 do begin
				elinds = where(elev[*,ng] ge elev_steps[nel] and elev[*,ng] lt elev_steps[nel+1] and $
								(scat[*,ng] eq 0b or scat[*,ng] eq 0b), ccel)
				if ccel gt 0. then $
					elevhist[ib,nel,ng] = elevhist[ng] + TOTAL(power[elinds,ng])/(14.*60.*max(power[elinds,ng]))
			endfor
		endif
	endfor
endfor




; Scale elevation distribution
maxdist = fltarr(n_elements(elevhist[*,0,0]))
for ib=0,n_elements(elevhist[*,0,0])-1 do $
	maxdist[ib] = max(median(reform(elevhist[ib,*,*]),3))
elevhist = elevhist/max(maxdist)

; Plot
xrange = [0.,2000.]
yrange = [0.,1500.]
; xtitle = 'Slant range [45 km]'
; ytitle = 'Elevation'
xtitle = 'Ground range [km]'
ytitle = ' '
!X.MARGIN = [4,3]	
for ib=0,n_elements(elevhist[*,0,0])-1 do begin
	!P.MULTI = [n_elements(elevhist[*,0,0])-ib,4,4]
; 	clear_page
	plot, xrange, yrange, /nodata, xstyle=1, ystyle=1, $
		xtitle=xtitle, ytitle=ytitle, xticklen=1, yticklen=1, $
		xgridstyle=1, ygridstyle=1, $
		title=title+', beam '+strtrim(ib,2)

	for ng=5,(*rad_fit_info[data_index]).ngates-1 do begin
; 		if ng lt xrange[1] then begin
			for nel=0,nelev_steps-2 do begin
				if elevhist[ib,nel,ng] gt 0. then begin
					if (180.+(ng+1)*45.)*cos(elev_steps[nel]*!dtor) gt xrange[1] or (180.+(ng+1)*45.)*sin(elev_steps[nel]*!dtor) gt yrange[1] then $
						continue

					col = bytscl(elevhist[ib,nel,ng], min=0., max=1., top=250) + 3b

; 					polyfill, ng+[0,0,1,1], [elev_steps[nel],elev_steps[nel+1],elev_steps[nel+1],elev_steps[nel]], color=col
					polyfill, (180.+(ng+[0,0,1,1])*45.)*cos(elev_steps[nel]*!dtor), $
						(180.+(ng+[0,0,1,1])*45.)*sin([elev_steps[nel],elev_steps[nel+1],elev_steps[nel+1],elev_steps[nel]]*!dtor), color=col
				endif
			endfor
; 		endif
	endfor

; 	phielev = findgen(40)
; 	hs = 200.
; 	ran = sqrt((Rav+hs)^2 - Rav^2*cos(phielev*!dtor)^2) - Rav*sin(phielev*!dtor)
; 	oplot, (ran-180.)/45., phielev, thick=2
; 
; 	hs = 300.
; 	ran = sqrt((Rav+hs)^2 - Rav^2*cos(phielev*!dtor)^2) - Rav*sin(phielev*!dtor)
; 	oplot, (ran-180.)/45., phielev, thick=2
; 
; 	hs = 400.
; 	ran = sqrt((Rav+hs)^2 - Rav^2*cos(phielev*!dtor)^2) - Rav*sin(phielev*!dtor)
; 	oplot, (ran-180.)/45., phielev, thick=2


	;*****************************************************************************
	;- plot magnetic field lines
	;*****************************************************************************
	; Calculates angle between ray direction (i.e., beam azimuth) and magnetic field declination
	rel_az = rt_data.azim[0,ib]*!dtor - rt_data.dip[0,ib,1,*]*!dtor
	; Calculates apparent dip angle in the plane of ray propagation
	dipa = -atan( tan(rt_data.dip[0,ib,0,*]*!dtor) / cos(rel_az) )
	; latitude/longitude distribution (expressed as angle from radar with Earth center)
	thtdip = findgen(n_elements(rt_data.dip[0,ib,0,*]))*2500./Rav/n_elements(rt_data.dip[0,ib,0,*])

	; Goes through the available dip angle values to plot 10 lines
	ntht = 0
	nblines = 0
	while ntht lt n_elements(thtdip)-2 and thtdip[ntht+1] le xrange[1]/Rav do begin
		if dipa[0] lt 0. then $
			alt = yrange[1] $
		else $
			alt = 0.
		dalt = 0.
		; Altitude loop for a given line
		while alt le yrange[1] and alt ge 0. do begin
			; local slope of the field line
			dalt = (Rav + alt)*(thtdip[ntht+1] - thtdip[ntht]) * tan(dipa[ntht])
			xx = [(alt + Rav)*thtdip[ntht], $
						(alt + dalt + Rav)*thtdip[ntht+1]]
			yy = [alt, alt + dalt]
			oplot, xx, yy, color=1
			; Move on to the next position and corresponding altitude
			ntht = ntht + 1
			alt = alt + dalt
		endwhile
		; Move on to the next line, spaced so that we fit 10 lines
		nblines = nblines + 1
		ntht = nblines*round(n_elements(thtdip)/10.)
	endwhile


	
	;*****************************************************************************
	;- plot aspect contour
	;*****************************************************************************
	nalts = 300
	nran = 400
	dr = 5.
	dz = 5.
	aspect = fltarr(nran,nalts)
	for ir=0,nran-1 do begin
		thtind = where(thtdip*Rav ge (ir+1)*dr, cc)
		dip = rt_data.dip[0,ib,0,thtind[0]]
		dec = rt_data.dip[0,ib,1,thtind[0]]
		if cc gt 0 then begin
			for iz=0,nalts-1 do begin
				; find k vector
				kr = (ir+1)*dr
				kz = (iz+1)*dz
				kvec = sqrt(kr^2+kz^2)
				kr = kr/kvec
				kz = kz/kvec

				; find B vector
				Br = cos(-dip*!dtor) * cos(rt_data.azim[0,ib]*!dtor - dec*!dtor)
				Bz = sin(-dip*!dtor)

				; find aspect angle
				aspect[ir,iz] = abs(90. - acos(Br*kr + Bz*kz)*!radeg)
				
; 				col = bytscl(aspect[ir,iz], min=0., max=90., top=250) + 3b
; 				polyfill, dr*(ir+[0,0,1,1]), dz*(iz+[0,1,1,0]), col=col
			endfor
		endif
	endfor
	contour, aspect, findgen(nran)*dr, findgen(nalts)*dz, /overplot, levels=[0., 5., 10., 20., 30., 40., 50., 60.], $
		c_labels=(1+intarr(8)), c_linestyle=2, c_charsize=.5

endfor

if keyword_set(ps) then $
	ps_close, /no_filename

end