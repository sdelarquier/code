pro	rt_plot_nmf2_bias, time, date=date, beam=beam, ps=ps

common rt_data_blk

Rav = 6370.

if ~keyword_set(beam) then begin
	beam = 0
endif
binds = where(rt_data.beam[0,*] eq beam)
ib = binds[0]

; Retrieve raytracing parameters from structure
radar = rt_info.name
caldat, rt_data.juls[*,0], month, day, year, hours, minutes
tdate 	= year*10000L + month*100L + day

parse_time, time, hour, minute
if ~keyword_set(date) then $
	date = tdate[0]
timeind = where(tdate eq date and hours eq hour and minutes eq minute)
juls = julday(month[timeind], day[timeind], year[timeind], hours[timeind], minutes[timeind])

; Open postscript if desired
if keyword_set(ps) then begin
	ps_open, '~/Desktop/nmf2bias_'+radar+STRTRIM(beam,2)+'_'+STRTRIM(date,2)+'_'+ $
		STRTRIM(STRING(time,format='(I04)'),2)+rt_info.timez+'.ps', /no_init
	clear_page
endif

; Find NmF2 at max elevation position (which according to RT closely matches actual NmF2)
gaterange = [5,40]
maxelev = max(rt_data.elevation[timeind[0],ib,gaterange[0]:gaterange[1]], maxelevind)
groundNe = findgen(n_elements(rt_data.edens[timeind[0],ib,0,*]))*2500./n_elements(rt_data.edens[timeind[0],ib,0,*])
thtind = where(groundNe ge rt_data.grange[timeind[0],ib,gaterange[0]+maxelevind])
nmf2 = max(rt_data.edens[timeind[0],ib,*,thtind[0]], nmf2ind)

; The real NmF2 is only used whith no horizontal gradients
nmf2real = max(rt_data.edens[timeind[0],ib,*,0], nmf2realind)

; Find azimuth
az = rt_data.azim[timeind[0],ib]

; Set default altitude
hs = 300.

; Set range where dip and eclination values are defined
gind = where(groundNe ge rt_data.grange[timeind[0],ib,maxelevind])
I = rt_data.dip[timeind[0],ib,0,gind[0]]
D = rt_data.dip[timeind[0],ib,1,gind[0]]

; print, I, D, nmf2ind*10.+60.

; Electron density at max elevation		
fp2 = (rt_data.tfreq[timeind[0]]*1e6)^2*(1 - rt_data.nr[timeind[0],ib,gaterange[0]+maxelevind]^2)
nelmax = fp2/80.5
; nelmax = rt_data.edens[timeind[0],ib,(rt_data.altitude[timeind[0],ib,gaterange[0]+maxelevind]-60.)/10.,thtind[0]]


; Set plot area
xrange = [5., 40.]
yrange = [0., 1.]
xtitle = 'Slant range [x45km]'
; xtitle = 'Elevation bias'
title = STRMID(format_juldate(rt_data.juls[timeind]),0,17)
subtitle = rt_info.name+', Beam '+STRTRIM(beam,2)+', '+$
	STRTRIM(string(rt_data.tfreq[timeind],format='(F5.2)'),2)+' MHz'
plot, xrange, yrange, /nodata, xstyle=1, ystyle=1, $
	xtitle=xtitle, ytitle='Relative NmF!I2!N bias', $
	title=title+', '+subtitle

dN = fltarr(rt_info.ngates)
dNrel = fltarr(rt_info.ngates)
dNreal = fltarr(rt_info.ngates)
gates = lindgen(rt_info.ngates)
for ng=gaterange[0],gaterange[1] do begin

	if rt_data.elevation[timeind[0],ib,ng] gt 0. then begin

		nr = Rav/(Rav + hs)*cos(rt_data.elevation[timeind[0],ib,ng]*!dtor)/cos(atan(cos((D-az)*!dtor), tan(I*!dtor)))
		fp2 = (rt_data.tfreq[timeind[0]]*1e6)^2*(1 - nr^2)
		N = fp2/80.5

		dN[ng] = abs(nmf2 - N)/nmf2
		dNrel[ng] = abs(nelmax - N)/nelmax
		dNreal[ng] = abs(nmf2real - N)/nmf2
; 		print, dN[ng], dNrel[ng], dNreal[ng], rt_data.elevation[timeind[0],ib,ng]

	endif
	
endfor

plotinds = where(rt_data.elevation[timeind[0],ib,*] gt 0.)
oplot, gates[plotinds], dN[plotinds], psym=1, symsize=.5
oplot, gates[plotinds], dNreal[plotinds], psym=7, symsize=.5
oplot, gates[plotinds], dNrel[plotinds], psym=4, symsize=.5
; abs(maxelev - rt_data.elevation[timeind[0],ib,plotinds])


end