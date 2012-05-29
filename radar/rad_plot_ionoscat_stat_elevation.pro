pro	rad_plot_ionoscat_stat_elevation

openr, unit, 'ionoscat_elev_edges.dat', /get_lun
njuls = 0L
readu, unit, njuls
print, njuls
juls = dblarr(njuls)
edges = fltarr(njuls,4)
slopes = fltarr(njuls,4)
readu, unit, juls, edges, slopes
nelev = 0L
ngates = 0L
readu, unit, ngates, nelev
radhist = fltarr(njuls,ngates,nelev)
rthist = fltarr(njuls,ngates,nelev)
readu, unit, radhist, rthist
nmf2 = fltarr(njuls)
hmf2 = fltarr(njuls)
readu, unit, nmf2, hmf2
free_lun, unit
; Slope contains the slope and y-intercept
; Edges contains the highest measured elevation and the corresponding range


; Set elevation range
elrange = [5.,55.]
elstep = 0.5
nelev_steps = (elrange[1]-elrange[0])/elstep
elev_steps = elrange[0] + findgen(nelev_steps)*(elrange[1]-elrange[0])/nelev_steps


set_format, /portrait
charsize = 1.2
ps_open, '~/Desktop/tsr_elev_distrib.ps'

position = [.12, .51, .9, .9]
xrange = [juls[0], juls[njuls-1]]
yrange = [-.05, .05]
date_label = label_date(date_format=['%M','%Y'])
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=4, xstyle=5, ystyle=5
dav = 15
weights = ( (dav+1-abs(findgen(2*dav)-dav))/float(dav+1) )
weights = weights/total(weights)
for id=dav,njuls-1-dav do begin
	if slopes[id,0] ne 0 then plots, juls[id], total( slopes[id-dav:id+dav,0]*weights ), psym=1, symsize=.5
	if slopes[id,2] ne 0 then plots, juls[id], total( slopes[id-dav:id+dav,2]*weights ), psym=4, symsize=.5
endfor
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat='label_date', xtickunits='Month', xtickinterval=4, ytickname=replicate(' ',60), $
	xstyle=1, xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, xcharsize=.001
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat=['',''], xtickunits=['Month','Year'], xtickinterval=4, $
	ytitle=textoidl('Slope [^\circ/km]'), xstyle=1, xtickname=replicate(' ',60), xcharsize=.001



position = [.12, .1, .9, .5]
xrange = [juls[0], juls[njuls-1]]
yrange = [0, 40]
date_label = label_date(date_format=['%M','%Y'])
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=4, xstyle=5, ystyle=5
dav = 15
weights = ( (dav+1-abs(findgen(2*dav)-dav))/float(dav+1) )
weights = weights/total(weights)
for id=dav,njuls-1-dav do begin
	if edges[id,0] ne 0 then plots, juls[id], total( edges[id-dav:id+dav,1]*weights ), psym=1, symsize=.5
	if edges[id,2] ne 0 then plots, juls[id], total( edges[id-dav:id+dav,3]*weights ), psym=4, symsize=.5
endfor
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat='label_date', xtickunits='Month', xtickinterval=4, ytickname=replicate(' ',60), $
	xstyle=1, xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, xcharsize=.001
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=4, $
	ytitle=textoidl('Elevation [^\circ]'), xstyle=1

ps_close, /no_f



; ********************************************************
; 2D elevation distribution from data and RT
; ********************************************************
set_format, /portrait
charsize = 1.2
ps_open, '~/Desktop/tsr_elev_2d_distrib.ps'

position = [.12, .51, .9, .9]
xtickinterval = 1
dav = 15
xrange = [juls[dav], juls[njuls-1-dav]]
yrange = [5., 40.]
date_label = label_date(date_format=['%M','%Y'])
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=4, xstyle=5, ystyle=5
; dav = 1
weights = ( (dav+1-abs(findgen(2*dav)-dav))/float(dav+1) )
weights = weights/total(weights)
for id=15,njuls-2-15 do begin
	if slopes[id,0] ne 0 then begin
		nzinds = where( reform(slopes[id-dav:id+dav,0]) ne 0., ccnz)
		Arunav = fltarr(nelev)
		if ccnz ne 0 then begin
			for iel=0,nelev-2 do begin
				Arunav[iel] = total( total(radhist[id-dav:id+dav,*,iel],2)*weights/total(weights[nzinds]), 1 ) 
			endfor
		endif else Arunav[*] = 0.
		maxav = max(Arunav, nan)
		for iel=0,nelev-2 do begin
			runav = Arunav[iel]
			col = bytscl(runav / maxav, min=0., max=1., top=250)+2b
			if (elev_steps[iel]+elstep le yrange[1]) and col gt 0 then $
				polyfill, juls[id]*[1,1,0,0] + juls[id+1]*[0,0,1,1], elev_steps[iel]+[0,elstep,elstep,0], color=col
		endfor
	endif
endfor
plot, xrange, yrange, /nodata, position=position, charsize=charsize, /xminor, $
	xtickformat='label_date', xtickunits='Month', xtickinterval=xtickinterval, ytickname=replicate(' ',60), $
	xstyle=1, ystyle=1, xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, xcharsize=.001
plot, xrange, yrange, /nodata, position=position, charsize=charsize, /xminor, $
	xtickformat=['',''], xtickunits=['Month','Year'], xtickinterval=xtickinterval, $
	ytitle=textoidl('Elevation [^{\circ}]'), xstyle=1, ystyle=1, xtickname=replicate(' ',60), xcharsize=.001



position = [.12, .1, .9, .5]
dav = 15
xrange = [juls[dav], juls[njuls-1-dav]]
yrange = [5., 40.]
date_label = label_date(date_format=['%M','%Y'])
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=4, xstyle=5, ystyle=5
weights = ( (dav+1-abs(findgen(2*dav)-dav))/float(dav+1) )
weights = weights/total(weights)
for id=dav,njuls-2-dav do begin
	Arunav = fltarr(nelev)
	for iel=0,nelev-2 do begin
		Arunav[iel] = total( total(rthist[id-dav:id+dav,*,iel],2)*weights, 1 ) 
	endfor
	maxav = max(Arunav, nan)
	for iel=0,nelev-2 do begin
		runav = Arunav[iel]
		col = bytscl(runav / maxav, min=0., max=1., top=250)+2b
		if (elev_steps[iel]+elstep le yrange[1]) and col gt 0 then $
			polyfill, juls[id]*[1,1,0,0] + juls[id+1]*[0,0,1,1], elev_steps[iel]+[0,elstep,elstep,0], color=col
	endfor
; 	stop
endfor
plot, xrange, yrange, /nodata, position=position, charsize=charsize, /xminor, $
	xtickformat='label_date', xtickunits='Month', xtickinterval=xtickinterval, ytickname=replicate(' ',60), $
	xstyle=1, ystyle=1, xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, xcharsize=.001
plot, xrange, yrange, /nodata, position=position, charsize=charsize, /xminor, $
	xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=xtickinterval, $
	ytitle=textoidl('Elevation [^{\circ}]'), xstyle=1, ystyle=1

xyouts, .5, .91, 'Blackstone, beam 12', align=.5, /normal, charsize=charsize
plot_colorbar, position=[.91,.1,.93,.9], scale=[0,1], /continuous, legend='Scatter distribution', $
	/no_rotate, level_format='(f4.1)', /keep_first_last, charsize=charsize


ps_close, /no_f





end