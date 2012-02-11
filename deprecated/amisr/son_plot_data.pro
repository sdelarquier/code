pro	son_plot_data

ps_open, '~/Desktop/son_data.ps'

son_pos = [67., 309.]
date = 20110720
time = 445
parse_time, time, hour, minute
ut0 = hour + minute/60.
rotate = ut0*360./24.
lim = 31.
;scale = [1e10, 3e11]*1e-11
scale = [0, 1000.]

son_read, lat=lats, lon=lons, nel=nel, ut=ut, alt=alt, vo=vo

siz = size(lats)
nt = siz[1]
nalt = siz[2]

;  Plot data
rotate = ut*360./24.

xx = 0.*lats
yy = xx
for it=0,nt-1 do begin
	tmp = calc_stereo_coords(lats[it,*], lons[it,*])
	rotate = ut[it,0]*360./24.
	x1 = cos(rotate*!dtor)*tmp[0:29] - sin(rotate*!dtor)*tmp[30:59]
	y1 = sin(rotate*!dtor)*tmp[0:29] + cos(rotate*!dtor)*tmp[30:59]
	xx[it,*] = x1
	yy[it,*] = y1
endfor


; Plot Data
map_plot, date=date, coords='geog', xrange=[-lim,lim], yrange=[-lim,lim], $
	rotate=ut0*360./24.
	
for it=nt-3,0,-1 do begin
	for ialt=0,nalt-2 do begin
		if vo[it, ialt] ne 9999.99 and alt[it,ialt] lt 450. and ut[it,ialt] ge 4. and alt[it,ialt] gt 70. then begin
			; col = bytscl(10^(nel[it,ialt])*1e-11, max=scale[1], min=scale[0], top=250) + 2
			col = bytscl(vo[it,ialt], max=scale[1], min=scale[0], top=250) + 2
		
			polyfill, [xx[it,ialt], xx[it+1,ialt], xx[it+1,ialt+1], xx[it,ialt+1]], $
					[yy[it,ialt], yy[it+1,ialt], yy[it+1,ialt+1], yy[it,ialt+1]], $
					col=col
		endif
	endfor
endfor

; Plot F-region limits
blat = fltarr(nt-1,2)
blon = fltarr(nt-1,2)
for it=0,nt-2 do begin
	for ialt=0,nalt-2 do begin
		if alt[it,ialt] ge 190. and alt[it,ialt] lt 220. then begin
			blat[it,0] = lats[it,ialt]
			blon[it,0] = lons[it,ialt]
		endif
		if alt[it,ialt] ge 390. and alt[it,ialt] lt 420. then begin
			blat[it,1] = lats[it,ialt]
			blon[it,1] = lons[it,ialt]
		endif
	endfor
endfor


tmp = calc_stereo_coords(blat[*,0], blon[*,0])
rotate = ut[*,0]*360./24.
xx = tmp[0,*]
yy = tmp[1,*]
x1 = cos(rotate*!dtor)*xx - sin(rotate*!dtor)*yy
y1 = sin(rotate*!dtor)*xx + cos(rotate*!dtor)*yy
xx = x1
yy = y1

oplot, xx, yy, thick=2


tmp = calc_stereo_coords(blat[*,1], blon[*,1])
rotate = ut[*,0]*360./24.
xx = tmp[0,*]
yy = tmp[1,*]
x1 = cos(rotate*!dtor)*xx - sin(rotate*!dtor)*yy
y1 = sin(rotate*!dtor)*xx + cos(rotate*!dtor)*yy
xx = x1
yy = y1

oplot, xx, yy, thick=2


; Plot EISCAT points
elat = [71.93, 67.14, 72.7, 72.7]
elon = [16.25, 21.79, 15.18, 15.18]
eut = [4.09, 4.31, 4.47, 4.87]
tmp = calc_stereo_coords(elat, elon)
rotate = eut*360./24.
x1 = cos(rotate*!dtor)*tmp[0,*] - sin(rotate*!dtor)*tmp[1,*]
y1 = sin(rotate*!dtor)*tmp[0,*] + cos(rotate*!dtor)*tmp[1,*]

oplot, x1, y1, psym=1, symsize=1.



; Plot EISCAT scans

eslat = [75.08, 63.8]
eslon = [11.04, 23.5]
esut = [4.03, 4.35]
tmp = calc_stereo_coords(eslat, eslon)
rotate = esut*360./24.
x1 = cos(rotate*!dtor)*tmp[0,*] - sin(rotate*!dtor)*tmp[1,*]
y1 = sin(rotate*!dtor)*tmp[0,*] + cos(rotate*!dtor)*tmp[1,*]

oplot, x1, y1, thick=2


eslat = [74.16, 63.8]
eslon = [11.04, 23.5]
esut = [4.41, 4.75]
tmp = calc_stereo_coords(eslat, eslon)
rotate = esut*360./24.
x1 = cos(rotate*!dtor)*tmp[0,*] - sin(rotate*!dtor)*tmp[1,*]
y1 = sin(rotate*!dtor)*tmp[0,*] + cos(rotate*!dtor)*tmp[1,*]

oplot, x1, y1, thick=2


eslat = [74.11, 63.8]
eslon = [11.04, 23.5]
esut = [4.81, 5.16]
tmp = calc_stereo_coords(eslat, eslon)
rotate = esut*360./24.
x1 = cos(rotate*!dtor)*tmp[0,*] - sin(rotate*!dtor)*tmp[1,*]
y1 = sin(rotate*!dtor)*tmp[0,*] + cos(rotate*!dtor)*tmp[1,*]

oplot, x1, y1, thick=2

ps_close

end