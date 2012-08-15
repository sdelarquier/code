pro	plot_hmax

common rt_data_blk

caldat, rt_data.juls, month, day, year, hours, minutes
date 	= year*10000L + month*100L + day
time = hours*100L + minutes
nt = n_elements(time)

hmax = fltarr(nt)
nmax = fltarr(nt)
for i=0,nt-1 do begin
	rt_get_hmax, time[i], hmax=hm, nmax=nm

	hmax[i] = hm
	nmax[i] = nm
endfor

clear_page
yrange = [150., 350.]
; caldat, rt_data.juls[0], month, day, year, hour, minute
; print, hour, minute
xrange = [rt_data.juls[0], rt_data.juls[nt-2]]
position = define_panel(1,1,0,0, /bar)
charsize = get_charsize(1,1)
plot, rt_data.juls, hmax, min_val=1., $
	xtickformat='label_date', thick=2, xtickv=rt_data.juls[0:*:6], $
	yrange=yrange, xrange=xrange, xticks=3, xminor=6, $
	xstyle=9, ystyle=9, position=position, $
	charsize=charsize, xtitle='Time (UT)', ytitle='Altitude (km)'
plot, rt_data.juls, nmax, min_val=1., /ylog, col=200, $
	xtickformat='label_date', thick=2, $
	yrange=[1e10,1e12], xrange=xrange, $
	xstyle=5, ystyle=5, position=position, $
	charsize=charsize, xtitle='Time (UT)', ytitle='Altitude (km)'
axis, yaxis=1, /ylog, yrange=[1e10, 1e12], $
	charsize=charsize, ytitle='Electron density [m!E3!N]'
axis, xaxis=1, xtickname=replicate(' ',60), xrange=xrange, xstyle=1, $
	xtickv=rt_data.juls[0:*:6], xticks=3, xminor=6
	

end