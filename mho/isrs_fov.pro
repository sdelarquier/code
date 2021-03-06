pro	isrs_fov, dist, elev, radar=radar


; Open postscript
set_format, /landscape
ps_open, '~/Desktop/isrs_fov.ps'

; Plot radar fov
radar = ['bks', 'wal', 'fhe', 'fhw', 'cve', 'cvw']
rad_plot_fov, radar, coords='magn', panel=[1,0], yrange=[-50,0], xrange=[-40,35]
; rad_plot_fov, radar, coords='magn', panel=[1,0], yrange=[-45,-20], xrange=[-25,20]

; Plot MH location
mhpos = [42.6,288.5]
magpos = cnvcoord(mhpos[0], mhpos[1], 0.)
lat = magpos[0]
lon = magpos[1]
proj = calc_stereo_coords(lat, lon)
load_usersym, /circle
plots, proj[0], proj[1], psym=8, symsize=0.8
xyouts, proj[0]*1.1, proj[1]*0.97, 'MH', /data, charthick=20., color=255, align=.5
xyouts, proj[0]*1.1, proj[1]*0.97, 'MH', /data, charthick=4., align=.5

; Plot Sondie location
sdpos = [66.36,309.05]
magpos = cnvcoord(sdpos[0], sdpos[1], 0.)
lat = magpos[0]
lon = magpos[1]
proj = calc_stereo_coords(lat, lon)
load_usersym, /circle
plots, proj[0], proj[1], psym=8, symsize=0.8
xyouts, proj[0]*1.1, proj[1]*0.97, 'MH', /data, charthick=20., color=255, align=.5
xyouts, proj[0]*1.1, proj[1]*0.97, 'MH', /data, charthick=4., align=.5

; Plot MH fov
nazim = 100
azims = -180. + findgen(nazim+1)*360./nazim
mhfov = findgen(nazim+1,2)
alti = 0.
Rav = 6370.
for iaz=0,nazim do begin
	is = calc_pos(mhpos[0], mhpos[1], alti, azims[iaz], dist, elev, latiout, longiout)
	magpos = cnvcoord(latiout, longiout, 0.)
	lat = magpos[0]
	lon = magpos[1]
	proj = calc_stereo_coords(lat, lon)
	mhfov[iaz,*] = proj
endfor
plots, mhfov[*,0], mhfov[*,1], thick=8

; Plot Sondie fov
nazim = 100
azims = -180. + findgen(nazim+1)*360./nazim
sdfov = findgen(nazim+1,2)
alti = 0.
Rav = 6370.
for iaz=0,nazim do begin
    is = calc_pos(sdpos[0], sdpos[1], alti, azims[iaz], dist, elev, latiout, longiout)
    magpos = cnvcoord(latiout, longiout, 0.)
    lat = magpos[0]
    lon = magpos[1]
    proj = calc_stereo_coords(lat, lon)
    sdfov[iaz,*] = proj
endfor
plots, sdfov[*,0], sdfov[*,1], thick=8

; Close postscript
ps_close, /no_f

end