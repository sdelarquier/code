pro rad_plot_msi_beams, ps=ps

common radarinfo

; Set date and time
ajul = systime(/julian,/utc)
caldat, ajul, mm, dd, year
yrsec = (ajul-julday(1,1,year,0,0,0))*86400.d
tval = TimeYMDHMSToEpoch(year, mm, dd, 0, 0, 0)
ngates = 70
coords = 'magn'

; Set plot area
load_usersym, /circle
loadct, 4
set_format, /landscape
if keyword_set(ps) then $
	ps_open, '~/Desktop/msi_beams.ps'
clear_page
position = define_panel(1,1,0,0, /bar)
map_plot_panel, 1, 1, 0, 0, date=year*10000L+mm*100L+dd, coords=coords, /bar, /iso, /no_fill, yrange=[-50., 10.], xrange=[-43.,37.]

; Set radar list
radar = ['wal','bks','fhe','fhw','cve','cvw']
acolor = [220,50,100,100,140,140]
nradars = n_elements(radar)
overlay_radar, jul=ajul, names=radar, coords=coords, /annotate
overlay_fov, jul=ajul, names=radar, coords=coords, /no_fill, nranges=ngates

; open file
openw, unit, '~/Desktop/sd_msi_coverage.dat', /get_lun
openw, unit2, '~/Desktop/sd_msi_pos.dat', /get_lun

; Plot and write to file
for ir=0,nradars-1 do begin
	color = acolor[ir]
	radID = where(network.code[0,*] eq radar[ir])
	for s=0,31 do begin
		if (network[radID].site[s].tval eq -1) then break
		if (network[radID].site[s].tval ge tval) then break
	endfor
	radarsite = network[radID].site[s]
	nbeams = network[radID].site[s].maxbeam
	rad_define_beams, network[radID].ID, nbeams, ngates, year, yrsec, coords=coords, $
			/normal, fov_loc_center=fov_loc_center

	for ib=0,nbeams-1 do begin
		for ig=0,ngates-1 do begin
			proj = calc_stereo_coords(fov_loc_center[0,ib,ig],fov_loc_center[1,ib,ig])
			
			plots, proj[0], proj[1], psym=8, color=color, symsize=.1
		endfor
		printf, unit, radar[ir], ib, 'lats', reform(fov_loc_center[0,ib,*]), format='(A3,I3,A5,'+strtrim(ngates+1,2)+'f9.3)'
		printf, unit, radar[ir], ib, 'lons', reform(fov_loc_center[1,ib,*]), format='(A3,I3,A5,'+strtrim(ngates+1,2)+'f9.3)'
	endfor

	printf, unit2, radar[ir], radarsite.geolat, radarsite.geolon, radarsite.boresite, radarsite.bmsep, radarsite.maxbeam, format='(A3,4F7.2,I4)'
endfor

free_lun, unit, unit2

if keyword_set(ps) then begin
	ps_close, /no_filename
; 	spawn, 'ps2png.sh ~/Desktop/msi_beams.ps'
; 	spawn, 'mv msi_beams.png ~/Desktop/msi_beams.png'
endif

end