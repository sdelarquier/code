pro	rad_fit_plot_velocity_corr, date, radar, time=time, beam=beam

common rt_data_blk
common rad_data_blk

if ~keyword_set(time) then time = [0,2400]

; Set-up output space
set_format, /landscape, /sardines, /tokyo
ps_open, '~/Desktop/'+radar+'.'+strtrim(date,2)+'.'+strtrim(time[0],2)+'.'+strtrim(time[1],2)+'corrvel.ps'
xyouts, .1, .86, 'Velocity correction for '+radar+', beam '+strtrim(beam,2), $
	charsize=charsize, charthick=charthick, /normal
parse_date, date, yy, mm, dd
ajul = julday(mm, dd, yy)
xyouts, .9, .86, /normal, $
	STRMID(format_juldate(ajul),0,11), $
	align=1., charsize=charsize, charthick=charthick

; Read radar data and pass it through median filter
rad_fit_read, date, radar, time=time, /filter;, /ajground, /catfile, catpath='~/tmp/', /force
data_index = rad_fit_get_data_index()
if data_index eq -1 then $
	return

; ***************************************************
; -IRI correction
; ***************************************************
; Calculate corrected velocities with RT method
rad_fit_calc_velocity, method='iri'

; Correction
rt_data.nr = (1./rt_data.nr-1.)*100.

; Plot correction
scale = [0.,10.]
set_colorsteps, 250
rt_plot_rti_panel, 2,2,0,0, param='nr', /bar, scale=scale, time=time, /first, /last, beam=beam, /grid

; Plot velocity
; rad_fit_plot_rti_panel, 2,1,0,1, param='velocity', /bar, scale=[-50.,50.], time=time, /first, /last, beam=beam, /grid, coords='rang', yrange=[0,3000.]

; Plot elevation
; rt_plot_rti_panel, 2,2,0,1, param='elevation', /bar, scale=[0,30], time=time, /first, /last, beam=beam, /grid



; ***************************************************
; -IRI correction
; ***************************************************
; Calculate corrected velocities with RT method
rad_fit_calc_velocity, method='elevation'

; Plot correction
set_colorsteps, 250
rad_fit_plot_rti_panel, 2,2,1,0, param='width', /bar, scale=scale, time=time, /last, beam=beam, /grid, coords='rang', yrange=[0.,3000.]
plot_colorbar, 2,2,1,0, scale=scale, legend='Correction [%]', /no_rotate, $
	level_format='(F4.1)', /keep_first_last_label, nlevels=4, /continuous

; Plot elevation
; rad_fit_plot_rti_panel, 2,2,1,1, param='elevation', /bar, scale=[0,30], time=time, /last, beam=beam, /grid, coords='rang', yrange=[0.,3000.]
; plot_colorbar, 2,2,1,1, scale=[0,30], legend='Elevation [deg]', /no_rotate, $
; 	level_format='(F4.1)', /keep_first_last_label, nlevels=4, /continuous


ps_close, /no_f

end