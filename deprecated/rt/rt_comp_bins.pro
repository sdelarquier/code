pro rt_comp_bins, ps=ps

common rt_data_blk

date = 20101117
time = [2100, 1300]

xmaps = 1
ymaps = 3
scale = [10., 40.]
param = 'elev'
cbparam='elevation'

rad_calc_ground_range, date=date, radar='bks', Rgnd=Rgnd

if keyword_set(ps) then $
	ps_open, '~/Desktop/rt_comp_bins.ps'

set_format, /portrait, /sardines
clear_page

xyouts, .5, .9, date2str(date), /normal, align=.5, charsize=get_charsize(xmaps,ymaps)

; rt_run, date, 'bks', time=time, /ionos, bin='phase'
; rt_plot_rti_panel,xmaps, ymaps, 0, 0, ytitle='True range', $
; 	date=[date,date+1],time=time, /grid, $
; 	param=param, xtitle='', /bar, scale=scale, /ionos;, /range
; plot_colorbar, xmaps, ymaps, 0, 0, param=cbparam, /bar, scale=scale

rt_run, date, 'bks', time=time, /ionos, bin='group'
rt_plot_rti_panel,xmaps, ymaps, 0, 0, ytitle='Group range', $
	date=[date,date+1],time=time, /grid, $
	param=param, xtitle='', /bar, scale=scale, /ionos;, /range
plot_colorbar, xmaps, ymaps, 0, 0, param=cbparam, /bar, scale=scale


rt_info.fov_loc_center = (Rgnd[0:rt_info.ngates]-180.)/45.

rt_plot_rti_panel,xmaps, ymaps, 0, 2, ytitle='Ground range (RBPOS)', $
	date=[date,date+1],time=time, /grid, $
	param=param, xtitle='', /bar, scale=scale, /ionos, /last
plot_colorbar, xmaps, ymaps, 0, 2, param=cbparam, /bar, scale=scale


rt_run, date, 'bks', time=time, /ionos, bin='ground'
rt_plot_rti_panel,xmaps, ymaps, 0, 1, ytitle='Ground range (RT)', $
	date=[date,date+1],time=time, /grid, $
	param=param, xtitle='', /bar, scale=scale, /ionos
plot_colorbar, xmaps, ymaps, 0, 2, param=cbparam, /bar, scale=scale


if keyword_set(ps) then $
	ps_close

end