pro rad_plot_ionoscat_altitude_stat, radar, maxvar=maxvar, tsample=tsample

if ~keyword_set(tsample) then $
    tsample=1.

openr, unit, '~/Documents/code/radar/data/ionoscat_altitude_'+radar+'.dat', /get_lun
njuls = 0L
readu, unit, njuls
juls = dblarr(njuls)
readu, unit, juls
nalts = 0L
readu, unit, nalts
alts = fltarr(nalts)
readu, unit, alts
ngates = 0L
nalts2 = 0L
readu, unit, ngates, nalts2
radhist = fltarr(njuls,ngates,nalts2)
rthist = fltarr(njuls,ngates,nalts2)
readu, unit, radhist, rthist
free_lun, unit
help, njuls, juls, nalts, alts, ngates, nalts2, radhist, rthist

; Generate time array with 1 month steps for the given time period
julbins = timegen(start=julday(11,01,2010), final=julday(05,01,2011), units='months', step_size=1)
njbins = n_elements(julbins)

; ********************************************************
; Radar data distribution - altitude
; ********************************************************
set_format, /portrait
; charsize = 1.2
ps_open, '~/Desktop/tsr_alt_distrib_'+radar+'.ps'

position = [.1, .51, .9, .9]
xrange = [julbins[0], julbins[njbins-1]]
yrange = [100., 500.]
date_label = label_date(date_format=['%M','%Y'])
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
    xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=1, xminor=1, xstyle=5, ystyle=5
; Rebin histogram
datadays = lonarr(njbins)
maxindsd = lonarr(njbins)
histfull = dblarr(njbins, nalts2)
for ibin=0,njbins-2 do begin
    inds = where(juls ge julbins[ibin] and juls lt julbins[ibin+1], cc)
    if cc le 0 then continue
    histfull[ibin, *] = total( total( radhist[inds,*,*], 2 ), 1 )
    dinds = where( total( total( radhist[inds,*,*], 2 ), 2 ) ne 0, dcc)
    histfull[ibin, *] = histfull[ibin, *]/float(dcc)
    datadays[ibin] = dcc
    print, dcc, total( radhist[inds,*,*] )
    tmax = max(histfull[ibin, *], max_ind)
    maxindsd[ibin] = alts[max_ind]
endfor
histfull = histfull / max(histfull)
for ibin=0,njbins-2 do begin
    for ir=0,nalts2-3 do begin
        col = bytscl(histfull[ibin,ir], min=0., max=1., top=250)+2b
        if (alts[ir] le yrange[1] and alts[ir] gt yrange[0]) and histfull[ibin,ir] gt 0 then $
            polyfill, julbins[ibin]*[1,1,0,0] + julbins[ibin+1]*[0,0,1,1], [alts[ir],alts[ir+1],alts[ir+1],alts[ir]], color=col
    endfor
endfor
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
    xtickformat='label_date', xtickunits='Month', xtickinterval=1, xminor=1, ytickname=replicate(' ',60), $
    xstyle=1, ystyle=9, xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, xcharsize=.001
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
    xtickformat=['',''], xtickunits=['Month','Year'], xtickinterval=1, xminor=1, $
    ytitle='Altitude [km]', xstyle=9, ystyle=1, xtickname=replicate(' ',60), xcharsize=.001
nsamples = njbins*1./tsample
jshift = julbins + (julbins[1:*]-julbins[0:njbins-2])/2.
maxvar = interpol(maxindsd[0:njbins-2],nsamples)
oplot, interpol(jshift,nsamples), maxvar, thick=4
; Show number of days with data for each time bin
plot, xrange, [0,31], /nodata, xstyle=1, ystyle=1, $
    xtickname=replicate(' ',60), ytickname=replicate(' ',60), $
    position=[position[0], position[3], position[2], position[3]+.03]
for ibin=0,njbins-2 do begin
    polyfill, julbins[ibin]*[1,1,0,0] + julbins[ibin+1]*[0,0,1,1], datadays[ibin]*[0,1,1,0], col=0
endfor

; ********************************************************
; RT distribution - altitude
; ********************************************************
position = [.1, .15, .9, .5]
date_label = label_date(date_format=['%M','%Y'])
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
    xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=2, xstyle=5, ystyle=5
; Rebin histogram
rthistfull = dblarr(njbins, nalts2)
for ibin=0,njbins-2 do begin
    inds = where(juls ge julbins[ibin] and juls lt julbins[ibin+1], cc)
    if cc le 0 then continue
    rthistfull[ibin, *] = total( total( rthist[inds,*,*], 2 ), 1 )
    dinds = where( total( total( rthist[inds,*,*], 3 ), 2 ) ne 0, dcc)
    rthistfull[ibin, *] = rthistfull[ibin, *]/float(dcc)
endfor
rthistfull = rthistfull / max(rthistfull)
for ibin=0,njbins-2 do begin
    for ir=0,nalts2-3 do begin
        col = bytscl(rthistfull[ibin,ir], min=0., max=1., top=250)+2b
        if (alts[ir] le yrange[1] and alts[ir] gt yrange[0]) and rthistfull[ibin,ir] gt 0 then $
            polyfill, julbins[ibin]*[1,1,0,0] + julbins[ibin+1]*[0,0,1,1], [alts[ir],alts[ir+1],alts[ir+1],alts[ir]], color=col
    endfor
endfor
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
    xtickformat='label_date', xtickunits='Month', xtickinterval=1, xminor=1, ytickname=replicate(' ',60), $
    xstyle=1, ystyle=9, xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, xcharsize=.001
plot, xrange, yrange, /nodata, position=position, charsize=charsize, $
    xtickformat=['label_date','label_date'], xtickunits=['Month','Year'], xtickinterval=1, xminor=1, $
    ytitle='Altitude [km]', xstyle=1, ystyle=1, xtickname=replicate(' ',60)

xyouts, .5, .94, radar+', beam 12', align=.5, /normal, charsize=charsize
plot_colorbar, position=[.91,.1,.93,.9], scale=[0,1], /continuous, legend='Scatter distribution', $
    /no_rotate, level_format='(f4.1)', /keep_first_last, charsize=charsize, nlevels=5


ps_close, /no_f

end