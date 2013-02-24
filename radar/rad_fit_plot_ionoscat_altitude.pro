;- Plots a range-altitude scatter distribution for a given radar on a given day at midnight +/- 3 hours
; Both radar data and ray tracing are compared for a given beam
; For now, the aspect contours are only available for bks and for the configuration in 2011 (before the new electronics)
;
; Last update: Feb, 23, 2012
pro rad_fit_plot_ionoscat_altitude, date, radar, beam, ps=ps, xrange=xrange, yrange=yrange, dtime=dtime, radhist=radhist, rthist=rthist, alt_steps=alt_steps

common rt_data_blk
common radarinfo
common rad_data_blk

Rav = 6371.

if keyword_set(ps) then begin
	if size(ps,/type) eq 7 then $
		ps_open, ps $
	else $
		ps_open, '~/Desktop/ionoscatalti_'+radar+'_'+strtrim(date,2)+'.ps'
endif
set_format, /landscape
clear_page

if ~keyword_set(dtime) then $
	dtime = 4.d $
else $
	dtime = dtime*1.d

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
	yrange = [100.,500.]
charsize = 1.0

; Set elevation range
elrange = [5.,55.]
elstep = 0.5
nelev_steps = (elrange[1]-elrange[0])/elstep
elev_steps = elrange[0] + findgen(nelev_steps)*(elrange[1]-elrange[0])/nelev_steps

; Set altitude range
altrange = yrange
nalt_steps = nelev_steps;(altrange[1]-altrange[0])/altstep
altstep = (altrange[1]-altrange[0])/nalt_steps;5.
alt_steps = altrange[0] + findgen(nalt_steps)*(altrange[1]-altrange[0])/nalt_steps

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
hist = fltarr(n_elements(beams), 71, nelev_steps+1)
histalt = fltarr(n_elements(beams), 71, nalt_steps+1)
corrvalt = fltarr(71, nelev_steps+1)
parse_date, date, tyy, tmm, tdd
; rtdate = date
rtdate = tyy*10000L+tmm*100L+01
rt_run, rtdate, radar, time=[0,1200], hmf2=

; Find midnight
rad_calc_sunset, rtdate, radar, 7, 70, $
    solnoon=solnoon
julmidnight = solnoon[20] - 0.5d

; zeros power where undefined
pinds = where(rt_data.power eq 10000. and rt_data.gscatter ne 2b, ccpinds)
if ccpinds gt 0 then $
    rt_data.power[pinds] = 0.

; Build histogram
indsmidnight = where(rt_data.juls ge julmidnight-dtime/24.d and $
    rt_data.juls le julmidnight+dtime/24.d, nnighttimes)
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
            rt_read_rays, rtdate, rt_info.name, ttime, beams[nb], rt_data.tfreq[indsmidnight[it]], radpos=radpos, thtpos=thtpos, grppth=grppth, raysteps=raysteps, code=code
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
                            nalt = round((radpos[nr,ns]*1e-3-Rav - altrange[0])/altstep)
                            ng = round((grppth[nr,ns-1]*1e-3-180.)/45.)
                            addhist = 1./( rt_data.power[indsmidnight[it],ib,ng] / min(rt_data.power[indsmidnight,*,*]) )
                            if finite(addhist) then begin
                                tran = 180.+ng*45.
                                talt = radpos[nr,ns]*1e-3-Rav
                                tvalt = sqrt(tran^2 + Rav^2 + 2.*tran*Rav*sin(telev*!dtor)) - Rav
                                corrvalt[ng,nel] = (tvalt - talt)/tran
                                if talt lt 150. then continue
                                if nel ge 0 and nel lt nelev_steps then hist[nb,ng,nel] = hist[nb,ng,nel] + addhist
                                if nalt ge 0 and nalt lt nalt_steps then histalt[nb,ng,nalt] = histalt[nb,ng,nalt] + addhist
                            endif
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
rthist = histalt
hist = hist/max(hist[*,10:*,*])
histalt = histalt/max(histalt[*,10:*,*])
valt2palt = mean(corrvalt[where(corrvalt gt 0.)])
print, 'Valt -> Palt: ', valt2palt

; Plot virtual altitude
ib = 0
pos = [.51,.51,.9,.9]
ytickname = replicate(' ',60)
ytitle = ''
xtickname = replicate(' ',60)
xtitle = ''
plot, xrange, yrange, /nodata, xstyle=1, ystyle=1, position=pos, $
	xtickname=xtickname, ytickname=ytickname, xtitle=xtitle, ytitle=ytitle, $
	xticklen=1, xgridstyle=1, yticklen=1, ygridstyle=1, charsize=charsize
for ir=0,70 do begin
	for iel=0,nelev_steps-2 do begin
		if ir*45.+180. ge xrange[0] and (ir+1)*45.+180. le xrange[1] then begin
			col = bytscl(hist(ib,ir,iel), min=0, max=1, top=250) + 3b
			
			rancell = 180.+[ir,ir+1,ir+1,ir]*45.
			elevcell = elev_steps[iel]*[1,1,0,0] + elev_steps[iel+1]*[0,0,1,1]
			alticell = sqrt(rancell^2 + Rav^2 + 2.*rancell*Rav*sin(elevcell*!dtor)) - Rav
			if col gt 3b and max(alticell) le yrange[1] and min(alticell) ge yrange[0] then $
				polyfill, rancell, alticell, col=col
		endif
	endfor
endfor
; Plot physical altitude
pos = [.51,.1,.9,.5]
ytickname = replicate(' ',60)
ytitle = ''
xtickname = ''
xtitle = 'Ground range [km]'
plot, xrange, yrange, /nodata, xstyle=1, ystyle=1, position=pos, $
	xtickname=xtickname, ytickname=ytickname, xtitle=xtitle, ytitle=ytitle, $
	xticklen=1, xgridstyle=1, yticklen=1, ygridstyle=1, charsize=charsize
for ir=0,70 do begin
	for ialt=0,nalt_steps-2 do begin
		if ir*45.+180. ge xrange[0] then begin; and (ir+1)*45.+180. le xrange[1] then begin
			col = bytscl(histalt(ib,ir,ialt), min=0, max=1, top=250) + 3b
			
			rancell = 180.+[ir,ir+1,ir+1,ir]*45.
			alticell = alt_steps[ialt]*[1,1,0,0] + alt_steps[ialt+1]*[0,0,1,1]
			valticell = alticell + valt2palt*rancell
			elevcell = !radeg*asin( ((alticell+Rav)^2 - (rancell^2 + Rav^2))/(2.*rancell*Rav) )
			grancell = Rav*asin( rancell*cos(elevcell*!dtor)/(valticell+Rav) )
            if max(grancell) gt xrange[1] then continue
			if col gt 3b and max(alticell) le yrange[1] and min(alticell) ge yrange[0] then begin
				polyfill, grancell, alticell, col=col
			endif
		endif
	endfor
endfor
xarr = asparanges # Replicate(1, N_Elements(aspaelev))
yarr = Replicate(1, N_Elements(asparanges)) # aspaelev
aspalt = sqrt(xarr^2 + Rav^2 + 2.*xarr*Rav*sin(yarr*!dtor)) - Rav
aspgran = Rav*asin( xarr*cos(yarr*!dtor)/(aspalt+Rav) )
contour, aspect[beam,*,*], aspgran, aspalt, /overplot, levels=[89.], c_charsize=charsize, c_label=[0]
contour, aspect[0,*,*], aspgran, aspalt, /overplot, levels=[89.], c_charsize=charsize, c_linestyle=2, c_label=[0]
;iri_run, date, 225, param='alti', lati=radarsite.geolat, longi=radarsite.geolon, /ut, nel=nel
;oplot, nel*500., 100.+findgen(500)
;print, 'IRI 225', min(nel), max(nel)
;iri_run, date, 525, param='alti', lati=radarsite.geolat, longi=radarsite.geolon, /ut, nel=nel
;oplot, nel*500., 100.+findgen(500)
;print, 'IRI 525', min(nel), max(nel)
;iri_run, date, 825, param='alti', lati=radarsite.geolat, longi=radarsite.geolon, /ut, nel=nel
;oplot, nel*500., 100.+findgen(500)
;print, 'IRI 825', min(nel), max(nel)
; print, nel

xyouts, .5, .91, $
		radar+', beam '+strtrim(beam,2)+', '+STRMID(format_juldate(julmidnight),0,17)+textoidl('\pm')+'3:00 UT', $
		align=.5, /normal, charsize=charsize



;*******************************************
; Radar: SCATTER 
;*******************************************
hist = fltarr(n_elements(beams),71,nelev_steps+1)
histalt = fltarr(n_elements(beams), 71, nalt_steps+1)
radhist = histalt
rad_fit_read, date, radar, time=[0,1200], /filter, /ajground, /catfile, catpath='~/tmp/'

data_index = rad_fit_get_data_index()
if data_index eq -1 then $
	return

rad_fit_calculate_elevation, date=date, time=[0,1200], /overwrite, tdiff=-.324, interfer_pos=[0.,-58.9,-2.7], scan_boresite_offset=8.

; Find midnight
rad_calc_sunset, date, radar, 7, 70, $
	solnoon=solnoon
julmidnight = solnoon[20] - 0.5d

for ib=0,n_elements(beams)-1 do begin
    binds = where((*rad_fit_data[data_index]).beam eq beams[ib] and $
                (*rad_fit_data[data_index]).juls ge julmidnight-dtime/24.d and $
                (*rad_fit_data[data_index]).juls le julmidnight+dtime/24.d and $
                (*rad_fit_data[data_index]).tfreq ge 10e3 and $
                (*rad_fit_data[data_index]).tfreq le 12e3, ccinds)
    if ccinds le 0 then $
        continue

    elev = (*rad_fit_data[data_index]).elevation[binds,*]
    power = (*rad_fit_data[data_index]).power[binds,*]
    scat = (*rad_fit_data[data_index]).gscatter[binds,*]
    frang = ( (*rad_fit_data[data_index]).lagfr[binds,*]*0.15 )#(1. + fltarr(1,n_elements(elev[0,*])))
    rsep = ( (*rad_fit_data[data_index]).smsep[binds,*]*0.15 )#(1. + fltarr(1,n_elements(elev[0,*])))
    range = frang + ( ( fltarr( n_elements(elev[*,0]) ) + 1. )#indgen(1,n_elements(elev[0,*])) ) * rsep
    valti = sqrt( range^2 + Rav^2 + 2.*range*Rav*sin(elev*!dtor) ) - Rav
    alti = valti - valt2palt*range
    for ng=5,50 do begin
        if ng lt xrange[1] then begin
            for nel=0,nelev_steps-2 do begin
                elinds = where(elev[*,ng] ge elev_steps[nel] and elev[*,ng] lt elev_steps[nel+1] and $
                            scat[*,ng] eq 0b and power[*,ng] ne 10000. and power[*,ng] ge 6., ccel)
                if ccel gt 0. then begin
                    hist[ib,ng,nel] = hist[ib,ng,nel] + ccel
                endif
            endfor
            for nalt=0,nalt_steps-2 do begin
                altinds = where(alti[*,ng] ge alt_steps[nalt] and alti[*,ng] lt alt_steps[nalt+1] and $
                            scat[*,ng] eq 0b and power[*,ng] ne 10000. and power[*,ng] ge 6., ccalt)
                if ccalt gt 0. then begin
                    histalt[ib,ng,nalt] = histalt[ib,ng,nalt] + ccalt
                endif
            endfor
        endif
    endfor
endfor
radhist = histalt
hist = hist/max(hist[*,10:*,*])

; Plot virtual altitude
ib = 0
pos = [.1,.51,.49,.9]
ytickname = ''
ytitle = 'Virtual height [km]'
xtickname = replicate(' ',60)
xtitle = ''
plot, xrange, yrange, /nodata, xstyle=1, ystyle=1, position=pos, $
	xtickname=xtickname, ytickname=ytickname, xtitle=xtitle, ytitle=ytitle, $
	xticklen=1, xgridstyle=1, yticklen=1, ygridstyle=1, charsize=charsize
for ir=0,70 do begin
	for iel=0,nelev_steps-2 do begin
		if ir*45.+180. ge xrange[0] and (ir+1)*45.+180. le xrange[1] then begin
			col = bytscl(hist(ib,ir,iel), min=0, max=1, top=250) + 3b
			
			rancell = 180.+[ir,ir+1,ir+1,ir]*45.
			elevcell = elev_steps[iel]*[1,1,0,0] + elev_steps[iel+1]*[0,0,1,1]
			alticell = sqrt(rancell^2 + Rav^2 + 2.*rancell*Rav*sin(elevcell*!dtor)) - Rav
			if col gt 3b and max(alticell) le yrange[1] and min(alticell) ge yrange[0] then $
				polyfill, rancell, alticell, col=col
		endif
	endfor
endfor
; Plot physical altitude
pos = [.1,.1,.49,.5]
ytickname = ''
ytitle = 'Altitude [km]'
xtickname = ''
xtitle = 'Ground range [km]'
plot, xrange, yrange, /nodata, xstyle=1, ystyle=1, position=pos, $
	xtickname=xtickname, ytickname=ytickname, xtitle=xtitle, ytitle=ytitle, $
	xticklen=1, xgridstyle=1, yticklen=1, ygridstyle=1, charsize=charsize
for ir=0,70 do begin
	for iel=0,nelev_steps-2 do begin
		if ir*45.+180. ge xrange[0] then begin; and (ir+1)*45.+180. le xrange[1] then begin
			col = bytscl(hist(ib,ir,iel), min=0, max=1, top=250) + 3b
			
			rancell = 180.+[ir,ir+1,ir+1,ir]*45.
			elevcell = elev_steps[iel]*[1,1,0,0] + elev_steps[iel+1]*[0,0,1,1]
            valticell = sqrt(rancell^2 + Rav^2 + 2.*rancell*Rav*sin(elevcell*!dtor)) - Rav                
            grancell = Rav*asin( rancell*cos(elevcell*!dtor)/(valticell+Rav) )
			if col gt 3b and max(valticell) le yrange[1] and min(valticell) ge yrange[0] then $
				polyfill, grancell, valticell - valt2palt*rancell, col=col
		endif
	endfor
endfor
; print, size(asparanges), size(aspaelev)
xarr = asparanges # Replicate(1, N_Elements(aspaelev))
yarr = Replicate(1, N_Elements(asparanges)) # aspaelev
; print, size(xarr), size(yarr), size(aspect[beam,*,*])
aspalt = sqrt(xarr^2 + Rav^2 + 2.*xarr*Rav*sin(yarr*!dtor)) - Rav
aspgran = Rav*asin( xarr*cos(yarr*!dtor)/(aspalt+Rav) )
contour, aspect[beam,*,*], aspgran, aspalt, /overplot, levels=[89.], c_charsize=charsize, c_label=[0]
contour, aspect[0,*,*], aspgran, aspalt, /overplot, levels=[89.], c_charsize=charsize, c_linestyle=2, c_label=[0]
; Plot Millstone Hill densities (for Nov 17 2010)
;if date eq 20101118 then begin
;	load_usersym, /circle, /no_fill
;	mlh_read, data, mlh_alt, mlh_juls
;	sjul = julday(11, 18, 2010, 2, 25)
;	fjul = julday(11, 18, 2010, 8, 25)
;	inds = where(data.el1 eq 12.00 and data.az1 eq -90. and mlh_juls ge sjul and mlh_juls le fjul and mlh_alt ge 200. and mlh_alt le 500.)
;	altbins = mlh_alt[inds[UNIQ(mlh_alt[inds], SORT(mlh_alt[inds]))]]
;	mlh_nel = fltarr(n_elements(altbins))
;	mlh_nel_dev = fltarr(n_elements(altbins))
;	dnel = .05
;	nelbins = dnel + findgen(ceil(2./dnel))*dnel
;	for ialt=0,n_elements(altbins)-2 do begin
;		indalts = inds[where(mlh_alt[inds] ge altbins[ialt] and mlh_alt[inds] lt altbins[ialt+1])]
;		for iel=0,n_elements(nelbins)-2 do begin
;			nelalts = where(10.^(data.popl[indalts]-11.) ge nelbins[iel] and 10.^(data.popl[indalts]-11.) lt nelbins[iel+1], cc)
;			if cc gt 0 then begin
;				plots, nelbins[iel]*500., altbins[ialt], psym=8, symsize=cc/50., col=0
;			endif
;		endfor
;		mlh_nel[ialt] = mean( 10.^(data.popl[indalts]-11.) )
;		mlh_nel_dev[ialt] = meanabsdev( 10.^(data.popl[indalts]-11.) )
;	endfor
;endif

bpos = [.91,.1,.925,.5]
plot_colorbar, /vert, charthick=charthick, /continuous, $
	nlevels=4, scale=[0,1], position=bpos, charsize=charsize, $
	legend='Scatter distribution', /no_rotate, $
	level_format='(F4.2)', /keep_first_last_label


if keyword_set(ps) then $
	ps_close, /no_f

end
