pro	rt_comp, date, radar, time=time

common rt_data_blk

rt_run, date, radar, time=time, /ionos, bin='group'
; rad_fit_read, date, radar, time=time
; rad_fit_calculate_elevation, date=date, time=time, /overwrite

ps_open, '~/Desktop/rt_comp.ps'

; set_format, /landscape, /sardines
clear_page

xmaps = 1
ymaps = 1

parse_date, date, year, month, day
sdate = strtrim(day,2)+'/'+strtrim(month,2)+'/'+strtrim(year,2)
xyouts, .5, .9, sdate, align=.5, charsize=get_charsize(xmaps,ymaps), /normal

; rad_fit_plot_rti_panel, xmaps, ymaps, 0, 0, $
; 	date=date,time=time, beam=7, $
; 	param='power', xtitle='', /bar, scale=scale
; plot_colorbar, xmaps, ymaps, 0, 0, param='power', /bar

; rad_fit_plot_rti_panel, xmaps, ymaps, 0, 1, $
; 	date=date,time=time, beam=7, $
; 	param='elevation', xtitle='', /bar, scale=scale
; plot_colorbar, xmaps, ymaps, 0, 1, param='elevation', /bar


; clear_page

rt_data.power = rt_data.power/max(rt_data.power)
rt_data.ionospower = rt_data.ionospower/max(rt_data.ionospower)
scale = [0., 1.]
set_colorsteps, 250
rt_plot_rti_panel, xmaps, ymaps, 0, 0, $
	date=date,time=time, $
	param='power', xtitle='', /bar, scale=scale, /ionos
plot_colorbar, xmaps, ymaps, 0, 0, param='power', /bar, scale=scale, level_format='(f4.2)'
set_colorsteps, 8

; scale = [10., 30.]
; rt_plot_rti_panel, xmaps, ymaps, 0, 1, $
; 	date=date,time=time, $
; 	param='elev', xtitle='', /bar, scale=scale, /ionos
; plot_colorbar, xmaps, ymaps, 0, 1, param='elevation', /bar, scale=scale

ps_close

end