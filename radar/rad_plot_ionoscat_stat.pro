pro	rad_plot_ionoscat_stat, radar, beam

openr, unit, 'ionoscat_edges_'+radar+'.dat', /get_lun
njuls = 0L
readu, unit, njuls
print, njuls
juls = dblarr(njuls)
edges = fltarr(njuls,4)
readu, unit, juls, edges
nbeams = 0L
ngates = 0L
readu, unit, nbeams, ngates
radhist = fltarr(njuls,nbeams,ngates)
rthist = fltarr(njuls,nbeams,ngates)
readu, unit, radhist, rthist
nmf2 = fltarr(njuls)
hmf2 = fltarr(njuls)
readu, unit, nmf2, hmf2
free_lun, unit
; help, njuls, juls, edges, nbeams, ngates, radhist, rthist, nmf2, hmf2

; Generate time array with 1 month steps for the given time period
julbins = timegen(start=juls[0], final=juls[njuls-1], units='Months', step_size=1)


; ********************************************************
; Radar data distribution - range
; ********************************************************
set_format, /portrait
; charsize = 1.2
ps_open, '~/Desktop/tsr_ran_distrib_'+radar+'.ps'

position = [.1, .51, .8, .9]
xrange = [juls[0], juls[njuls-1]]
yrange = [0, 2000]/100
date_label = label_date(date_format=['%M','%Y'])
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=4, xstyle=5, ystyle=5
histfull = dblarr(n_elements(julbins), ngates)
for id=0,njuls-1 do begin
	inds = where(julbins gt juls[id], cc)
	if cc le 0 then continue
	histfull[inds[0]-1, *] = radhist[id,beam,*]
endfor
histfull = histfull / max(histfull)
for ibin=0,n_elements(julbins)-2 do begin
	for ir=0,ngates-2 do begin
		col = bytscl(histfull[ibin,ir], min=0., max=1., top=250)+2b
		if ((180.+(ir+1)*45.)/100. le yrange[1]) and col gt 0 then $
			polyfill, julbins[ibin]*[1,1,0,0] + julbins[ibin+1]*[0,0,1,1], (180.+[ir,ir+1,ir+1,ir]*45.)/100., color=col
	endfor
endfor
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat='label_date', xtickunits='Month', xtickinterval=4, ytickname=replicate(' ',60), $
	xstyle=1, ystyle=9, xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, xcharsize=.001
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat=['',''], xtickunits=['Month','Year'], xtickinterval=4, $
	ytitle='Slant range [x100 km]', xstyle=9, xtickname=replicate(' ',60), xcharsize=.001

; ********************************************************
; RT distribution - range
; ********************************************************
position = [.1, .15, .8, .5]
xrange = [juls[0], juls[njuls-1]]
yrange = [0, 2000]/100
date_label = label_date(date_format=['%M','%Y'])
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=4, xstyle=5, ystyle=5
for id=dav,njuls-2-dav do begin
; 	if edges[id,1] gt 0 then begin
; 		nzinds = where( reform(max(rthist[id-dav:id+dav,beam,*], dimension=3)) ne 0., ccnz )
		maxav = total( max(rthist[id-dav:id+dav,beam,*], dimension=3)*weights/total(weights),1);[nzinds]), 1 )
; 		caldat, juls[id], tmm, tdd, tyy
; 		if tyy*10000L+tmm*100L+tdd eq 20090101 then $
; 			print, reform(rthist[id,beam,0:30])
; 		print, tyy, tmm, tdd, maxav, reform(rthist[id,beam,10:15])
		for ir=0,ngates-1 do begin
; 			nzinds = where( reform(rthist[id-dav:id+dav,beam,ir]) ne 0., ccnz )
; 			if ccnz ne 0 then $
				runav = total( rthist[id-dav:id+dav,beam,ir]*weights/total(weights),1);[nzinds]), 1 ) $
; 			else $
; 				runav = 0.
			col = bytscl(runav / maxav, min=0., max=1., top=250)+2b
			if ((180.+(ir+1)*45.)/100. le yrange[1]) and col gt 0 then $
				polyfill, juls[id]*[1,1,0,0] + juls[id+1]*[0,0,1,1], (180.+[ir,ir+1,ir+1,ir]*45.)/100., color=col
		endfor
; 	endif
endfor
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat='label_date', xtickunits='Month', xtickinterval=4, ytickname=replicate(' ',60), $
	xstyle=1, xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, xcharsize=.001
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=4, $
	ytitle='Slant range [x100 km]', xstyle=1, xtickname=replicate(' ',60)

xyouts, .5, .91, radar+', beam '+strtrim(beam,2), align=.5, /normal, charsize=charsize
plot_colorbar, position=[.91,.1,.93,.9], scale=[0,1], /continuous, legend='Scatter distribution', $
	/no_rotate, level_format='(f4.1)', /keep_first_last, charsize=charsize, nlevels=5


ps_close, /no_f

;- Fit a sinusoidal seasonal variation
; histfull = reform(radhist[*,beam,*])
; scathresh = .5
; x = (dblarr(1,ngates)+1.) ## juls
; y = transpose(ranges) ## (dblarr(njuls,1)+1.)
; pinds = where(histfull lt scathresh, pcc)
; if pcc gt 0 then histfull[pinds] = 0.
; print, min(histfull, /nan), max(histfull, /nan)
; expr = 'P[0] + P[1]*cos(2*!PI/P[2]*(X-X[0]+P[3]))'
; P0 = [600.d, 500.d, 365.d, 60.d]
; P = mpfitexpr(expr, x, y, histfull*0.d, P0, weights=histfull^2., /nan, status=status)
; print, status
; err = execute('f = '+expr)
; oplot, x, f, psym=8, symsize=.1



; ********************************************************
; Radar data distribution - azim
; ********************************************************
set_format, /portrait
; charsize = 1.2
ps_open, '~/Desktop/tsr_azim_distrib_'+radar+'.ps'

position = [.1, .51, .8, .9]
xrange = [juls[0], juls[njuls-1]]
yrange = [0, nbeams]
date_label = label_date(date_format=['%M','%Y'])
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=4, xstyle=5, ystyle=5
dav = 15
weights = ( (dav+1-abs(findgen(2*dav)-dav))/float(dav+1) )
weights = weights/total(weights)
for id=dav,njuls-2-dav do begin
	if edges[id,1] gt 0 then begin
		Arunav = fltarr(nbeams)
		for ib=0,nbeams-1 do begin
			nzinds = where( reform(total(radhist[id-dav:id+dav,ib,*],3)) ne 0., ccnz)
			if ccnz ne 0 then $
				Arunav[ib] = total( total(radhist[id-dav:id+dav,ib,*],3)*weights/total(weights[nzinds]), 1 ) $
			else $
				Arunav[ib] = 0.
		endfor
		maxav = max(Arunav, nan)
		for ib=0,nbeams-1 do begin
			runav = Arunav[ib]
			col = bytscl(runav / maxav, min=0., max=1., top=250)+2b
			if (ib+1 le yrange[1]) and col gt 0 then $
				polyfill, juls[id]*[1,1,0,0] + juls[id+1]*[0,0,1,1], [ib,ib+1,ib+1,ib], color=col
		endfor
	endif
endfor
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat='label_date', xtickunits='Month', xtickinterval=4, ytickname=replicate(' ',60), $
	xstyle=1, ystyle=9, xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, xcharsize=.001
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat=['',''], xtickunits=['Month','Year'], xtickinterval=4, $
	ytitle='Beam #', xstyle=9, xtickname=replicate(' ',60), xcharsize=.001


; ********************************************************
; RT distribution - azim
; ********************************************************
position = [.1, .15, .8, .5]
xrange = [juls[0], juls[njuls-1]]
yrange = [0, nbeams]
date_label = label_date(date_format=['%M','%Y'])
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=4, xstyle=5, ystyle=5
dav = 15
weights = ( (dav+1-abs(findgen(2*dav)-dav))/float(dav+1) )
weights = weights/total(weights)
for id=dav,njuls-2-dav do begin
		Arunav = fltarr(nbeams)
		for ib=0,nbeams-1 do begin
			nzinds = where( reform(total(rthist[id-dav:id+dav,ib,*],3)) ne 0., ccnz)
			if ccnz ne 0 then $
				Arunav[ib] = total( total(rthist[id-dav:id+dav,ib,*],3)*weights/total(weights[nzinds]), 1) $
			else $
				runav = 0.
		endfor
		maxav = max(Arunav, /nan)
		for ib=0,nbeams-1 do begin
			runav = Arunav[ib]
			col = bytscl(runav / maxav, min=0., max=1., top=250)+2b
			if (ib+1 le yrange[1]) and col gt 0 then $
				polyfill, juls[id]*[1,1,0,0] + juls[id+1]*[0,0,1,1], [ib,ib+1,ib+1,ib], color=col
		endfor
; 	endif
endfor
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat='label_date', xtickunits='Month', xtickinterval=4, ytickname=replicate(' ',60), $
	xstyle=1, ystyle=1, xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, yminor=4, yticks=4
plot, xrange, yrange, /nodata, position=position, $
	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=4, $
	ytitle='Beam #', xstyle=1, ystyle=1, yminor=4, yticks=4, charsize=charsize, ytickformat='(I4)'

plot_colorbar, position=[.91,.1,.93,.9], scale=[0,1], /continuous, legend='Scatter distribution', $
	/no_rotate, level_format='(f4.1)', /keep_first_last, charsize=charsize, nlevels=5

ps_close, /no_f




; ********************************************************
; IRI NmF2 and hmF2 over the radar data period
; ********************************************************
; set_format, /portrait
; charsize = 1.2
; ps_open, '~/Desktop/iri_var_'+radar+'.ps'
; 
; ionosonde_read, juls=iojuls, fof2=fof2, date=iodate, time=iotime
; position = [.12, .66, .9, .93]
; xrange = [juls[0], juls[njuls-1]]
; yrange = [0., 3.]
; date_label = label_date(date_format=['%M','%Y'])
; plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
; 	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=4, xstyle=5, ystyle=5
; inds = where(iojuls ge xrange[0] and iojuls le xrange[1], ccjuls)
; f2n = (1e6*2.*!pi)^2 * ( 8.854e-12*9.11e-31/1.6e-19^2 ) ; converts from MHz to m-3
; iojulsr = fltarr(njuls)
; ioner = fltarr(njuls)
; idr = 0L
; for id=1,ccjuls-1 do begin
; 	dayinds = where(iodate[inds] eq iodate[inds[id]], ccday)
; 	if iodate[inds[id]] eq iodate[inds[id-1]] then continue
; 	midnightinds = where( abs(600 - iotime[inds[dayinds]]) lt 600, ccmids)
; 	if ccmids gt 0 then $
; 		ioner[idr] = mean( fof2[inds[dayinds[midnightinds]]]^2 * f2n * 1e-11 )
; 	sfjul, [iodate[inds[id-1]],iodate[inds[id]]], 0, sjul, fjul, no_days=no_days
; 	idr += no_days-1
; endfor
; dav = 15
; weights = ( (dav+1-abs(findgen(2*dav)-dav))/float(dav+1) )
; ; weights = weights/total(weights)
; for id=dav,njuls-1-dav do begin
; 	nonzinds = where(ioner[id-dav:id+dav] ne 0., cc)
; 	if cc gt 0 then $
; 		weights = weights/total(weights[nonzinds-(id-dav)]) $
; 	else $
; 		continue
; 	caldat, juls[id], month, day, year
; ; 	print, year*10000L+month*100L+day, cc, total( ioner[id-dav:id+dav] * weights ), total( ioner[id-dav:id+dav] )
; 	plots, juls[id], total( ioner[id-dav:id+dav] * weights ), psym=1, symsize=.5
; endfor
; plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
; 	xtickformat='label_date', xtickunits='Month', xtickinterval=4, ytickname=replicate(' ',60), $
; 	xstyle=1, ystyle=1, xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, xcharsize=.001
; plot, xrange, yrange, /nodata, position=position, charsize=charsize, ystyle=1, $
; 	xtickformat=['',''], xtickunits=['Month','Year'], xtickinterval=4, $
; 	ytitle=textoidl('NmF_2 [\times 10^{11} m^{-3}]'), xstyle=1, xtickname=replicate(' ',60), xcharsize=.001
; 
; 
; position = [.12, .38, .9, .65]
; xrange = [juls[0], juls[njuls-1]]
; yrange = [0., 3.]
; date_label = label_date(date_format=['%M','%Y'])
; plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
; 	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=4, xstyle=5, ystyle=5
; for id=0,njuls-1 do begin
; 	plots, juls[id], nmf2[id], psym=1, symsize=.5
; endfor
; plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
; 	xtickformat='label_date', xtickunits='Month', xtickinterval=4, ytickname=replicate(' ',60), $
; 	xstyle=1, xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, xcharsize=.001
; plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
; 	xtickformat=['',''], xtickunits=['Month','Year'], xtickinterval=4, $
; 	ytitle=textoidl('NmF_2 [\times 10^{11} m^{-3}]'), xstyle=1, xtickname=replicate(' ',60), xcharsize=.001
; 
; 
; 
; position = [.12, .1, .9, .37]
; xrange = [juls[0], juls[njuls-1]]
; yrange = [250., 350.]
; date_label = label_date(date_format=['%M','%Y'])
; plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
; 	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=4, xstyle=5, ystyle=5
; for id=0,njuls-1 do begin
; 	plots, juls[id], hmf2[id], psym=1, symsize=.5
; endfor
; plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
; 	xtickformat='label_date', xtickunits='Month', xtickinterval=4, ytickname=replicate(' ',60), $
; 	xstyle=1, ystyle=1, xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, xcharsize=.001
; plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
; 	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=4, $
; 	ytitle=textoidl('hmF_2 [km]'), xstyle=1, ystyle=1
; 
; 
; ps_close, /no_f


end