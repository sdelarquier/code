pro	rt_plot_map

common rt_data_blk

; Date and time
caldat, rt_data.juls[*,0], mm, dd, yy, hr, mn
date = yy*10000L + mm*100L + dd
time = hr*100L + mn
ut = hr + mn/60.

; Read corresponding radar data
rad_fit_read, [date[0],date[n_elements(rt_data.juls[*,0])-1]], rt_info.name, time=[time[0],time[n_elements(rt_data.juls[*,0])-1]], /filter
stop
; Axis limits
xrange = [-50.,50.]
yrange = [-50.,50.]

; Loop through time
for nt=0,n_elements(rt_data.juls[*,0])-1 do begin
	; open postscript
	ps_open, '~/Desktop/rt_map/rt_plot_map_'+STRTRIM(string(nt+1,format='(I02)'),2)+'.ps'

	; Plot map pamel
	clear_page
	map_plot_panel,1,1,0,0, date=date, time=time, coords='geog', xrange=xrange, yrange=yrange, $
			rotate=ut[nt]*360./24., /isotropic, /no_fill
	
	; Overlay radar fovs
	overlay_radar, date=date, time=time, coords='geog', rotate=ut[nt]*360./24., names=rt_info.name
	overlay_fov, date=date, time=time, coords='geog', rotate=ut[nt]*360./24., names=rt_info.name, /no_fill

	; Overlay scatter limits predicted by ray-tracing
	

	; Close ps
	ps_close

	; Convert to png
	spawn, 'ps2png.sh ~/Desktop/rt_map/rt_plot_map_'+STRTRIM(string(nt+1,format='(I02)'),2)+'.ps'
	spawn, 'mv rt_plot_map_'+STRTRIM(string(nt+1,format='(I02)'),2)+'.png ~/Desktop/rt_map/rt_plot_map_'+STRTRIM(string(nt+1,format='(I02)'),2)+'.png'

endfor

; Make animated gif
spawn, 'convert -delay 100  ~/Desktop/rt_map/rt_plot_map_??.png ~/Desktop/rt_map/rt_plot_map.gif'

end