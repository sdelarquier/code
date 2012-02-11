pro map_plot_pos, lats, lons, ut=ut

date = 20110720
time = 400
parse_time, time, hour, minute
if ~keyword_set(ut) then $
	ut = hour + minute/60.
rotate = ut*360./24.
lim = 31.

map_plot, date=date, time=time, coords='geog', /no_coast, xrange=[-lim,lim], yrange=[-lim,lim], $
	rotate=(hour + minute/60.)*360./24.

tmp = calc_stereo_coords(lats, lons)
xx = tmp[0,*]
yy = tmp[1,*]
x1 = cos(rotate*!dtor)*xx - sin(rotate*!dtor)*yy
y1 = sin(rotate*!dtor)*xx + cos(rotate*!dtor)*yy
xx = x1
yy = y1

oplot, xx, yy

end
