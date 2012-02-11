pro rt_plot_nrelev, time, beam=beam, xrange=xrange, yrange=yrange

common rt_data_blk

Rav = 6370.

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
if ~keyword_set(xrange) then $
	xrange = [rt_info.elev_beg, rt_info.elev_end]
if ~keyword_set(yrange) then $
	yrange = [0.8,1.]
title = STRMID(format_juldate(rt_data.juls[timeind]),0,17)
subtitle = 'Radar: '+rt_info.name+', Beam '+STRTRIM(beam,2)+', Freq. '+$
	STRTRIM(string(rt_data.tfreq[timeind],format='(F5.2)'),2)+' MHz'
plot, xrange, yrange, xstyle=1, ystyle=1, /nodata, $
	xtitle='Elevation', ytitle='Refractive index', $
	title=title+'!C'+subtitle, charsize=!P.charsize*.8


elev = reform(rt_data.elevation[timeind[0],ib,0:30])
gates = lindgen(rt_info.ngates)
nr_iri = fltarr(rt_info.ngates)
nr_corr = fltarr(rt_info.ngates)
nr_corr_old = fltarr(rt_info.ngates)
for ng=2,30 do begin
	if rt_data.ionoscatter[timeind[0],ib,ng] eq 1b then begin
		nr_iri[ng] = rt_data.nr[timeind[0],ib,ng]

		hr = rt_data.altitude[timeind[0],ib,ng]
		az = rt_data.azim[timeind[0],ib]

		gind = round(rt_data.grange[timeind[0],ib,ng]*n_elements(rt_data.dip[timeind[0],ib,0,*])/2500.)
		I = rt_data.dip[timeind[0],ib,0,gind]
		D = rt_data.dip[timeind[0],ib,1,gind]

		nr_corr[ng] = Rav/(Rav + hr)*cos(elev[ng]*!dtor)/cos(atan(cos((D-az)*!dtor), tan(I*!dtor)))


		nr_corr_old[ng] = Rav/(Rav + hr)*cos(elev[ng]*!dtor)/sin(I*!dtor)

	endif
endfor

inds = where(rt_data.ionoscatter[timeind[0],ib,*] eq 1b)
oplot, elev[inds], nr_iri[inds], linestyle=0, thick=2, psym=4, symsize=.5; solid line
oplot, elev[inds], nr_corr[inds], linestyle=1, thick=2, psym=7, symsize=.5; dotted line
oplot, elev[inds], nr_corr_old[inds], linestyle=2, thick=2, psym=1, symsize=.5; dashed line

end