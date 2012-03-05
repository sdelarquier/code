;- Plots a range-elevation scatter distribution for a given radar on a given day at midnight +/- 3 hours
; Both radar data and ray tracing are compared for a given beam
; For now, the aspect contours are only available for bks and for the configuration in 2011 (before the new electronics)
;
; Last update: Feb, 23, 2012
pro rad_fit_plot_ionoscat_elevation, date, radar, beam, ps=ps, xrange=xrange, yrange=yrange

common rt_data_blk
common radarinfo
common rad_data_blk

Rav = 6371.

set_format, /portrait, /sardines, /tokyo
if keyword_set(ps) then $
	ps_open, '~/Desktop/ionoscat_elevation_'+radar+'_'+strtrim(date,2)+'.ps'
clear_page

; Parse date
parse_date, date, yy, mm, dd

; Find range-gate locations
yrsec = (julday(mm,dd,yy) - julday(1,1,yy))*86400.d
radID = where(network.code[0,*] eq radar)
tval = TimeYMDHMSToEpoch(yy, mm, dd, 0, 0, 0)
for s=0,31 do begin
	if (network[radID].site[s].tval eq -1) then break
	if (network[radID].site[s].tval ge tval) then break
endfor
radarsite = network[radID].site[s]
nbeams = radarsite.maxbeam
beams = beam

; Set plot limits
if ~keyword_set(xrange) then $
	xrange = [0.,40.]*45.
if ~keyword_set(yrange) then $
	yrange = [5.,55.]
charsize = get_charsize(n_elements(beams),2)

; Set elevation range
elrange = [5.,55.]
elstep = 0.5
nelev_steps = (elrange[1]-elrange[0])/elstep
elev_steps = elrange[0] + findgen(nelev_steps)*(elrange[1]-elrange[0])/nelev_steps

; Read aspect angle contours from file
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


;*******************************************
; Ray-tracing
;*******************************************
; Run ray tracing and set histogram
hist = fltarr(3, 71, nelev_steps+1)
rt_run, date, radar, time=[0,1200]

; Find midnight
rad_calc_sunset, date, radar, 7, 70, $
	solnoon=solnoon
julmidnight = solnoon[20] - 0.5d

; zeros power where undefined
pinds = where(rt_data.power eq 10000. and rt_data.gscatter ne 2b, ccpinds)
if ccpinds gt 0 then $
	rt_data.power[pinds] = 0.

; Build histogram
indsmidnight = where(rt_data.juls ge julmidnight-3.d/24.d and $
			rt_data.juls le julmidnight+3.d/24.d, nnighttimes)
if nnighttimes gt 0 then begin
	for it=0,nnighttimes-1 do begin
		caldat, rt_data.juls[indsmidnight[it]], mm, dd, yy, hr, mn
		tdate = yy*10000L + mm*100L + dd
		ttime = hr*100L + mn

		; Set tolerance (in degree) for how much deviation from perfect aspect condition is allowed
		tol = 1.

		; latitude/longitude distribution (expressed as angle from radar with Earth center)
		ndip = n_elements(rt_data.dip[indsmidnight[it],0,0,*])
		thtdip = findgen(ndip)*2500./Rav/ndip

		for nb=0,n_elements(beams)-1 do begin
			ib = beams[nb]

			; Test for data availability
			rt_read_rays, date, rt_info.name, ttime, beams[nb], rt_data.tfreq[indsmidnight[it]], radpos=radpos, thtpos=thtpos, grppth=grppth, raysteps=raysteps, code=code
			if ~code then $
				return
			; Count element of matrix
			nrays = n_elements(radpos[*,0])

			for nr=0,nrays-1 do begin
				telev = rt_info.elev_beg + nr*rt_info.elev_stp
				nrsteps = raysteps[nr]
				for ns=1,nrsteps-1 do begin
					if grppth[nr,ns-1] gt 180e3 then begin
						; Calculate k vector
						kx = radpos[nr,ns]*sin(thtpos[nr,ns]-thtpos[nr,ns-1])
						kz = radpos[nr,ns]*cos(thtpos[nr,ns]-thtpos[nr,ns-1]) - radpos[nr,ns-1]
						kvect = sqrt( kx^2. + kz^2. )
						
						; Middle of the step: position and index in B grid
						midtht = (thtpos[nr,ns]-thtpos[nr,ns-1])/2. + thtpos[nr,ns-1]
						diff = min(midtht-thtdip, thtind, /abs)
						
						; Dip and declination at this position
						middip = rt_data.dip[indsmidnight[it],ib,0,thtind]
						middec = rt_data.dip[indsmidnight[it],ib,1,thtind]

						; calculate vector magnetic field
						Bx = cos(-middip*!dtor) * cos(rt_data.azim[indsmidnight[it],ib]*!dtor - middec*!dtor)
						Bz = sin(-middip*!dtor)
						
						; calculate cosine of aspect angle
						cos_aspect = (Bx*kx + Bz*kz)/kvect
						
						if abs(cos_aspect) le cos(!pi/2. - tol*!dtor) then begin
							nel = round((telev - elrange[0])/elstep)
							ng = round((grppth[nr,ns-1]*1e-3-180.)/45.)
							hist[nb,ng,nel] = hist[nb,ng,nel] + 1./( rt_data.power[indsmidnight[it],ib,ng] / min(rt_data.power[indsmidnight,*,*]) )
						endif

					endif
				; end ray steps loop
				endfor
			; end rays loop
			endfor
		; end beam loop
		endfor
	; end time loop
	endfor
endif
hist = hist/max(hist[*,10:*,*])

; Plot results
for ib=0,n_elements(beams)-1 do begin
	pos = define_panel(n_elements(beams),2,ib,0, /bar)
	if ib ne 0 then begin
		ytickname = replicate(' ',60)
		ytitle = ''
	endif else begin
		ytickname = ''
		ytitle = 'Elevation'
	endelse
	xtickname = replicate(' ',60)
	xtitle = ''
	plot, xrange, yrange, /nodata, xstyle=1, ystyle=1, position=pos, $
		xtickname=xtickname, ytickname=ytickname, xtitle=xtitle, ytitle=ytitle, $
		xticklen=1, xgridstyle=1, yticklen=1, ygridstyle=1, charsize=charsize
	for ir=0,70 do begin
		for iel=0,nelev_steps-2 do begin
			if ir*45.+180. ge xrange[0] and (ir+1)*45.+180. le xrange[1] and elev_steps[iel] ge yrange[0] and elev_steps[iel+1] le yrange[1] then begin
				col = bytscl(hist(ib,ir,iel), min=0, max=1, top=250) + 3b
				
				if col gt 3b then $
					polyfill, 180.+[ir,ir+1,ir+1,ir]*45., elev_steps[iel]*[1,1,0,0] + elev_steps[iel+1]*[0,0,1,1], col=col
			endif
		endfor
	endfor
	contour, aspect[beams[ib],*,*], asparanges, aspaelev, /overplot, levels=[60.,70., 80., 85., 89.], c_labels=1b+bytarr(5), c_charsize=charsize
	xyouts, pos[0]+(pos[2]-pos[0])/2., pos[3]*.97, 'Beam '+strtrim(beams[ib],2), /normal, charsize=charsize, align=.5
endfor

xyouts, .5, pos[3]*1.03, $
		radar+', '+STRMID(format_juldate(julmidnight),0,17)+textoidl('\pm')+'3:00 UT', $
		align=.5, /normal, charsize=charsize



;*******************************************
; Radar: SCATTER 
;*******************************************
hist = fltarr(3,71,nelev_steps+1)
rad_fit_read, date, radar, time=[0,1200], /filter, /ajground, /catfile, catpath='~/tmp/'
rad_fit_calculate_elevation, date=date, time=[0,1200], /overwrite, tdiff=-.324, interfer_pos=[0.,-58.9,-2.7], scan_boresite_offset=8.
data_index = rad_fit_get_data_index()
if data_index eq -1 then $
	return

; Find midnight
rad_calc_sunset, date, radar, 7, 70, $
	solnoon=solnoon
julmidnight = solnoon[20] - 0.5d

for ib=0,n_elements(beams)-1 do begin
	binds = where((*rad_fit_data[data_index]).beam eq beams[ib] and $
				(*rad_fit_data[data_index]).juls ge julmidnight-3.d/24.d and $
				(*rad_fit_data[data_index]).juls le julmidnight+3.d/24.d and $
				(*rad_fit_data[data_index]).tfreq ge 10e3 and $
				(*rad_fit_data[data_index]).tfreq le 12e3, ccinds)
	if ccinds le 0 then $
		continue

	elev = (*rad_fit_data[data_index]).elevation[binds,*]
	power = (*rad_fit_data[data_index]).power[binds,*]
	scat = (*rad_fit_data[data_index]).gscatter[binds,*]
	for ng=5,50 do begin
		if ng lt xrange[1] then begin
			for nel=0,nelev_steps-2 do begin
				elinds = where(elev[*,ng] ge elev_steps[nel] and elev[*,ng] lt elev_steps[nel+1] and $
							scat[*,ng] eq 0b and power[*,ng] ne 10000. and power[*,ng] ge 6., ccel)
				if ccel gt 0. then $
					hist[ib,ng,nel] = hist[ib,ng,nel] + ccel
			endfor
		endif
	endfor
endfor
hist = hist/max(hist[*,10:*,*])

; Plot results
for ib=0,n_elements(beams)-1 do begin
	position = define_panel(n_elements(beams),2,ib,1, /bar)
	if ib ne 0 then begin
		ytickname = replicate(' ',60)
		ytitle = ''
	endif else begin
		ytickname = ''
		ytitle = 'Elevation'
	endelse
	xtickname = ''
	xtitle = 'Slant range [km]'
	plot, xrange, yrange, /nodata, xstyle=1, ystyle=1, position=position, $
		xtickname=xtickname, ytickname=ytickname, xtitle=xtitle, ytitle=ytitle, $
		xticklen=1, xgridstyle=1, yticklen=1, ygridstyle=1, charsize=charsize
	for ir=0,70 do begin
		for iel=0,nelev_steps-2 do begin
			if ir*45.+180. ge xrange[0] and (ir+1)*45.+180. le xrange[1] and elev_steps[iel] ge yrange[0] and elev_steps[iel+1] le yrange[1] then begin
				col = bytscl(hist(ib,ir,iel), min=0, max=1, top=250) + 3b
				
				if col gt 3b then $
					polyfill, 180.+[ir,ir+1,ir+1,ir]*45., elev_steps[iel]*[1,1,0,0] + elev_steps[iel+1]*[0,0,1,1], col=col
			endif
		endfor
	endfor
	contour, aspect[beams[ib],*,*], asparanges, aspaelev, /overplot, levels=[60.,70., 80., 85., 89.], c_labels=1b+bytarr(5), c_charsize=charsize
endfor

bpos = [.9,.3,.915,.65]
plot_colorbar, /vert, charthick=charthick, /continuous, $
	nlevels=4, scale=[0,1], position=bpos, charsize=charsize, $
	legend='Scatter distribution', /no_rotate, $
	level_format='(F4.2)', /keep_first_last_label


if keyword_set(ps) then $
	ps_close, /no_f

end
