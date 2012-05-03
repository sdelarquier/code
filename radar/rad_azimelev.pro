pro rad_azimelev, radar, date, ps=ps

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
bmsep = abs(radarsite.bmsep)

; Beams
beams = indgen(radarsite.maxbeam)

; Azimuth
azim = fltarr(radarsite.maxbeam)
for n=0,radarsite.maxbeam-1 do $
	azim[n] = radarsite.boresite - rt_get_azim(radar, beams[n], date)
stop
; Elevation
nelev = 101
delev = 60./nelev
elev = findgen(nelev)*delev

; Pointing direction
phi = fltarr(radarsite.maxbeam, nelev)
for n=0,radarsite.maxbeam-1 do $
	phi[n,*] = azim[n]/abs(azim[n])* acos( sqrt( (cos(azim[n]*!dtor)^2 - sin(elev*!dtor)^2) / cos(elev*!dtor)^2 ) ) * !radeg

; plot
if keyword_set(ps) then begin
	ps_open, '~/Desktop/rad_azimelev.ps'
endif


xtitle = 'Beam'
ytitle = 'Elevation angle'
xtickv = reverse(azim)
xticks = radarsite.maxbeam-1
; xtickname = string(beams, format='(I2)')
xtickname = ['0',' ','2',' ','4',' ','6',' ','8',' ','10',' ','12',' ','14',' ','16',' ','18',' ','20',' ','22',' ','24']
xrange = [min(azim)-abs(radarsite.bmsep), max(azim)+abs(radarsite.bmsep)]
yrange = [min(elev), max(elev)]
plot, xrange, yrange, /nodata, $
	xrange=xrange, xstyle=1, xtitle=xtitle, $
	yrange=yrange, ystyle=1, ytitle=ytitle, yticklen=1, ygridstyle=1, $
	xtickv=xtickv, xticks=xticks, xtickname=xtickname, position=[.08,.2,.58,.6]
oplot, (xrange[0] + (xrange[1]-xrange[0])/2.)*[1,1], yrange, linestyle=2
col = [50,220]
icol = 0
for n=0,radarsite.maxbeam-1 do begin
	oplot, phi[n,*], elev, color=col[icol]
	oplot, phi[n,0]*[1,1], yrange, linestyle=1
	icol = -icol + 1
endfor


xtitle = 'Beam'
ytitle = ''
ytickname = replicate(' ', 60)
xtickv = reverse(azim)
xticks = radarsite.maxbeam-1
; xtickname = string(beams, format='(I2)')
xtickname = ['0',' ','2',' ','4',' ','6',' ','8',' ','10',' ','12',' ','14',' ','16',' ','18',' ','20',' ','22',' ','24']
xrange = [min(azim)-abs(radarsite.bmsep), max(azim)+abs(radarsite.bmsep)]
yrange = [min(elev), max(elev)]
plot, xrange, yrange, /nodata, $
	xrange=xrange, xstyle=1, xtitle=xtitle, $
	yrange=yrange, ystyle=1, ytitle=ytitle, yticklen=1, ygridstyle=1, ytickname=ytickname, $
	xtickv=xtickv, xticks=xticks, xtickname=xtickname, position=[.59,.2,.98,.6]
oplot, (xrange[0] + (xrange[1]-xrange[0])/2.)*[1,1], yrange, linestyle=2
col = [50,220]
icol = 0
for n=0,radarsite.maxbeam-1 do begin
	bphi = phi[n,*]

	ind = where(abs(bphi) ge abs(bphi[0])+bmsep, cc)
	if cc gt 0 then $
		plots, bphi[0], elev[ind[0]], color=10, psym=2, symsize=.5

	ind = where(abs(bphi) ge abs(bphi[0])+2.*bmsep, cc)
	if cc gt 0 then $
		plots, bphi[0], elev[ind[0]], color=50, psym=2, symsize=.5

	ind = where(abs(bphi) ge abs(bphi[0])+3.*bmsep, cc)
	if cc gt 0 then $
		plots, bphi[0], elev[ind[0]], color=90, psym=2, symsize=.5

	ind = where(abs(bphi) ge abs(bphi[0])+4.*bmsep, cc)
	if cc gt 0 then $
		plots, bphi[0], elev[ind[0]], color=130, psym=2, symsize=.5

	ind = where(abs(bphi) ge abs(bphi[0])+5.*bmsep, cc)
	if cc gt 0 then $
		plots, bphi[0], elev[ind[0]], color=170, psym=2, symsize=.5

	ind = where(abs(bphi) ge abs(bphi[0])+6.*bmsep, cc)
	if cc gt 0 then $
		plots, bphi[0], elev[ind[0]], color=210, psym=2, symsize=.5

	ind = where(abs(bphi) ge abs(bphi[0])+7.*bmsep, cc)
	if cc gt 0 then $
		plots, bphi[0], elev[ind[0]], color=250, psym=2, symsize=.5

	oplot, phi[n,0]*[1,1], yrange, linestyle=1
	icol = -icol + 1
endfor
plot_colorbar, /horiz, position=[.59, .61, .98, .63], $
	/no_rotate, nlevels=7, colorsteps=7, charsize=1, $
	level_values=indgen(8), legend='Deviation [# beams]'

if keyword_set(ps) then $
	ps_close, /no_f

end