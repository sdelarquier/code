pro rad_fit_plot_rti_lines, date=date, time=time, beam=beam, step=step, ps=ps

common rad_data_blk

rad_fit_read, [20101117, 20101118], 'bks', time=[2230,1230], /filter
time = [2230,1230]

data_index = rad_fit_get_data_index()

if ~keyword_set(date) then begin
	date = [0l, 0l]
	caldat, (*rad_fit_info[data_index]).sjul, month, day, year
	date[0] = year*10000L + month*100L + day
	caldat, (*rad_fit_info[data_index]).fjul, month, day, year
	date[1] = year*10000L + month*100L + day
endif

if ~keyword_set(time) then begin
	time = [0l, 0l]
	caldat, (*rad_fit_info[data_index]).sjul, month, day, year, hour, minute
	time[0] = hour*100L + minute
	caldat, (*rad_fit_info[data_index]).fjul, month, day, year, hour, minute
	time[1] = hour*100L + minute
endif

if ~keyword_set(beam) then $
	beam = 7

if ~keyword_set(step) then $
	step = 10.

; Select data
parse_date, date[0], syy, smm, sdd
parse_date, date[1], fyy, fmm, fdd
parse_time, time, shr, smn, fhr, fmn
sjul = julday(smm, sdd, syy, shr, smn)
fjul = julday(fmm, fdd, fyy, fhr, fmn)
binds = where((*rad_fit_data[data_index]).beam eq beam and $
				(*rad_fit_data[data_index]).juls ge sjul and $
				(*rad_fit_data[data_index]).juls le fjul and $
				(*rad_fit_data[data_index]).channel eq 0, ccb)

; time resolution in minutes
dmn = round( ((*rad_fit_data[data_index]).juls[binds[1]] - (*rad_fit_data[data_index]).juls[binds[0]])*1440.d )

if keyword_set(ps) then begin
	if size(ps, /type) eq 7 then $
		ps_open, '~/Desktop/'+ps+'.ps' $
	else $
		ps_open, '~/Desktop/rad_fit_plot_rti_lines.ps'
endif

xmaps = 1
ymaps = 3
set_format, /sardines, /portrait
clear_page

; Display date(s)
xyouts, .5, .9, date2str(date), /normal, align=.5, charsize=get_charsize(xmaps,ymaps)

; Velocity tsr
velmax = 100.
velscale = 50.
position = define_panel(xmaps,ymaps,0,0,/bar)
_xticks = get_xticks(sjul, fjul, xminor=_xminor)
plot, [0.,0.], /nodata, xrange=[sjul,fjul], yrange=[-velscale,velscale], xstyle=1, ystyle=1, position=position, $
	xtickname=replicate(' ', 60), xticks=_xticks, xminor=_xminor, xthick=2., ythick=2., $
	xtitle='', ytitle='Velocity [m/s]', charsize=get_charsize(xmaps,ymaps)

velm = fltarr(n_elements((*rad_fit_data[data_index]).juls[binds]))
veldev = fltarr(n_elements((*rad_fit_data[data_index]).juls[binds]))
for nv=0,n_elements((*rad_fit_data[data_index]).juls[binds])-1 do begin
	velo = (*rad_fit_data[data_index]).velocity[binds[nv],0:30]
	velinds = where(velo lt velmax and velo gt -velmax, ccvel)
	if ccvel gt 0 then begin
		veldev[nv] = meanabsdev(velo[velinds], /nan)
; 		veldev[nv,0] = median(velo[velinds[0:round(ccvel/2.)]])
; 		veldev[nv,1] = median(velo[velinds[round(ccvel/2.):ccvel-1]])
		velm[nv] = median(velo[velinds])
	endif
	if ~(nv*dmn mod step) then $
		oplot, sjul + [nv*dmn, nv*dmn]/1440.d, [-velscale, velscale], linestyle=2, thick=1.
; 	plots, (*rad_fit_data[data_index]).juls[binds[nv]]*[1.,1.], veldev[nv,*]
endfor
oplot, [sjul, fjul], [0., 0.], linestyle=2, thick=1.
plots, (*rad_fit_data[data_index]).juls[binds], velm
errplot, (*rad_fit_data[data_index]).juls[binds], velm-veldev, velm+veldev, color=1

; velocity vertical line plot
position = define_panel(xmaps,ymaps,0,1,/bar)
_xticks = get_xticks(sjul, fjul, xminor=_xminor)
plot, [0.,0.], /nodata, xrange=[sjul,fjul], yrange=[0.,30.], xstyle=1, ystyle=1, position=position, $
	xtickname=replicate(' ', 60), xticks=_xticks, xminor=_xminor, xthick=2., ythick=2., $
	xtitle='', ytitle='Gate', charsize=get_charsize(xmaps,ymaps)

ydata = findgen(31)
; 1 m/s is velfact minutes
velfact = 2.
col = 0
for nv=0,n_elements((*rad_fit_data[data_index]).juls[binds])-1,round(step/dmn) do begin
	velo = (*rad_fit_data[data_index]).velocity[binds[nv],0:30]
	velinds = where(velo lt velmax and velo gt -velmax, ccvel)
	if ccvel gt 0 then begin
		offset = sjul - median(velo[velinds])/1440.d/velfact + nv*dmn/1440.d
		for ng=0,n_elements(velo)-2 do begin
			if velo[ng] lt velmax and velo[ng+1] lt velmax and velo[ng] gt -velmax and velo[ng+1] gt -velmax then $
				oplot, velo[ng:ng+1]/1440.d/velfact + offset, ydata[ng:ng+1], color=col
		endfor
	endif
	oplot, sjul + [nv*dmn, nv*dmn]/1440.d, [0., 30.], linestyle=2, thick=1., color=col
	col = -col + 1
endfor

rad_fit_plot_rti_panel, xmaps, ymaps, 0, 2, $
	date=date,time=time,beam=beam,scale=[-velscale, velscale], $
	param='velocity', xtitle='Time', /bar, /last, yrange=[0., 30.]
plot_colorbar, xmaps, ymaps, 0, 2, scale=[-velscale, velscale], param='velocity', /bar

if keyword_set(ps) then $
	ps_close

end