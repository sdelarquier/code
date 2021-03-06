pro rt_plot_nrelev, time, beam=beam, xrange=xrange, yrange=yrange, ps=ps

common rt_data_blk

Rav = 6370.

if keyword_set(ps) then $
	ps_open, '~/Desktop/nrelev_'+rt_info.name+'.ps'

; Set beam number
if ~keyword_set(beam) then begin
	nbeams = n_elements(rt_data.beam[0,*])
	beam = rt_data.beam[0,round(nbeams/2.)-1]
	beam = 0
endif
binds = where(rt_data.beam[0,*] eq beam)
ib = binds[0]

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

; Set plot limits
charsize = 1.8
if ~keyword_set(xrange) then $
	xrange = [rt_info.elev_beg, rt_info.elev_end]
if ~keyword_set(yrange) then $
	yrange = [0.9,1.]
title = STRMID(format_juldate(rt_data.juls[timeind]),0,17)
subtitle = 'Radar: '+rt_info.name+', Beam '+STRTRIM(beam,2)+', Freq. '+$
	STRTRIM(string(rt_data.tfreq[timeind],format='(F5.2)'),2)+' MHz'
plot, xrange, yrange, xstyle=1, ystyle=1, /nodata, $
	xtitle='Elevation', ytitle='Refractive index', $
	charsize=charsize*.8
xyouts, .5, .98, title+'!C'+subtitle, charsize=charsize*.8, align=.5, /normal


elev = reform(rt_data.elevation[timeind[0],ib,0:30])
gates = lindgen(rt_info.ngates)
nr_iri = fltarr(rt_info.ngates)
nr_corr = fltarr(rt_info.ngates)
nr_corr_old = fltarr(rt_info.ngates)
for ng=2,30 do begin
	if rt_data.gscatter[timeind[0],ib,ng] eq 2b then begin
		nr_iri[ng] = rt_data.nr[timeind[0],ib,ng]

		hr = rt_data.altitude[timeind[0],ib,ng]
		az = rt_data.azim[timeind[0],ib]

		gind = round(rt_data.grange[timeind[0],ib,ng]*n_elements(rt_data.dip[timeind[0],ib,0,*])/2500.)
		I = rt_data.dip[timeind[0],ib,0,gind]
		D = rt_data.dip[timeind[0],ib,1,gind]

		nr_corr[ng] = Rav/(Rav + hr)*cos(elev[ng]*!dtor)*sqrt(1. + cos((D-az)*!dtor)^2/tan(I*!dtor)^2)


		nr_corr_old[ng] = Rav/(Rav + hr)*cos(elev[ng]*!dtor)/sin(I*!dtor)

	endif
endfor

load_usersym, /circle
loadct, 3
inds = where(rt_data.gscatter[timeind[0],ib,*] eq 2b)
oplot, elev[inds], nr_iri[inds], thick=2, psym=8, symsize=.8, col=100; circles
oplot, elev[inds], nr_corr[inds], thick=2, psym=7, symsize=.8; x
oplot, elev[inds], nr_corr_old[inds], thick=2, psym=1, symsize=.8; +

; Legend
oplot, xrange, [1.,1.], linestyle=1
plots, 0.97*(xrange[1]-xrange[0])+xrange[0], 0.95*(yrange[1]-yrange[0])+yrange[0], psym=8, col=100, symsize=.8, /data
xyouts, 0.96*(xrange[1]-xrange[0])+xrange[0], 0.94*(yrange[1]-yrange[0])+yrange[0], 'IRI', charsize=charsize*.8, align=1.
plots, 0.97*(xrange[1]-xrange[0])+xrange[0], 0.9*(yrange[1]-yrange[0])+yrange[0], psym=7, symsize=.8, /data
xyouts, 0.96*(xrange[1]-xrange[0])+xrange[0], 0.89*(yrange[1]-yrange[0])+yrange[0], 'Formula (incl. declination)', charsize=charsize*.8, align=1.
plots, 0.97*(xrange[1]-xrange[0])+xrange[0], 0.85*(yrange[1]-yrange[0])+yrange[0], psym=1, symsize=.8, /data
xyouts, 0.96*(xrange[1]-xrange[0])+xrange[0], 0.84*(yrange[1]-yrange[0])+yrange[0], 'Formula (dip only)', charsize=charsize*.8, align=1.


if keyword_set(ps) then $
	ps_close, /no_fil

end