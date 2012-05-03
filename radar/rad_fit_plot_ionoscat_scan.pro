;- Plots a field of view representation of power for a given radar and day and given time
;
; Last update: Feb, 23, 2012
pro rad_fit_plot_ionoscat_scan, date, time, radar, ps=ps, xrange=xrange, yrange=yrange, showbeam=showbeam

common radarinfo
common rad_data_blk

if keyword_set(ps) then $
	ps_open, '~/Desktop/ionoscat_scan_'+radar+'_'+strtrim(date,2)+'.ps'
set_format, /landscape
clear_page

; Parse date
parse_date, date, yy, mm, dd

; Find range-gate locations
yrsec = (julday(mm,dd,yy) - julday(1,1,yy))*86400.d
radID = where(network.code[0,*] eq radar)
tval = TimeYMDHMSToEpoch(yy, mm, dd, 0, 0, 0)
for s=0,31 do begin
	if (network[radID].site[s].tval eq -1) then break
	if (network[radID].site[s].tval ge tval) then break
endfor
radarsite = network[radID].site[s]
nbeams = network[radID].site[s].maxbeam
ngates = 65
rad_define_beams, network[radID].id, nbeams, ngates, yy, yrsec, coords='magn', $
		/normal, fov_loc_full=fov_loc_full

; Calculate stereographic projection
for ib=0,nbeams do begin
	for ig=0,ngates do begin
		for p=0,3 do begin
			lat = fov_loc_full[0,p,ib,ig]
			lon = fov_loc_full[1,p,ib,ig]
			tmp = calc_stereo_coords(lat, lon)
			fov_loc_full[0,p,ib,ig] = tmp[0]
			fov_loc_full[1,p,ib,ig] = tmp[1]
		endfor
	endfor
endfor

; Set plot limits
txrange = [min(fov_loc_full[0,*,*,*],xmin)-5, max(fov_loc_full[0,*,*,*],xmax)+5]
tyrange = [min(fov_loc_full[1,*,*,*],ymin)-5, max(fov_loc_full[1,*,*,*],ymax)+5]
if ~keyword_set(xrange) then $
	xrange = txrange
if ~keyword_set(yrange) then $
	yrange = tyrange

; Adjust plot limits so that they cover the same extent
ext = abs(abs(xrange[1]-xrange[0]) - abs(yrange[1]-yrange[0]))
if abs(xrange[1]-xrange[0]) gt abs(yrange[1]-yrange[0]) then begin
	yrange[1] = yrange[1] + ext/2.
	yrange[0] = yrange[0] - ext/2.
endif else if abs(xrange[1]-xrange[0]) lt abs(yrange[1]-yrange[0]) then begin
	xrange[1] = xrange[1] + ext/2.
	xrange[0] = xrange[0] - ext/2.
endif

if keyword_set(rt) and keyword_set(vel) then $
	xmaps = 3 $
else if keyword_set(rt) or keyword_set(vel) then $
	xmaps = 2 $
else $
	xmaps = 1
; Initilalize position
xmap = 0


;*******************************************
; Radar: SCATTER 
;*******************************************
parse_date, date, yy, mm, dd
parse_time, time, hr, mn
jultime = julday(mm, dd, yy, hr, mn)
map_plot_panel, xmaps, 1, 0, 0, date=date, coords='magn', /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1

rad_fit_read, date, radar, time=[0,1200], /filter, /ajground, /catfile, catpath='~/tmp/'
data_index = rad_fit_get_data_index()
if data_index eq -1 then $
	return

; get color preferences
scale = [0,30]
foreground  = get_foreground()
color_steps = get_colorsteps()
ncolors     = get_ncolors()
bottom      = get_bottom()

; Set color bar and levels
cin = FIX(FINDGEN(color_steps)/(color_steps-1.)*(ncolors-1))+bottom
lvl = scale[0]+FINDGEN(color_steps)*(scale[1]-scale[0])/color_steps

; Plot beam-range grid
for ib=0,nbeams-1 do begin
	binds = where((*rad_fit_data[data_index]).beam eq ib and $
				(*rad_fit_data[data_index]).juls ge jultime and $
				(*rad_fit_data[data_index]).tfreq ge 10e3 and $
				(*rad_fit_data[data_index]).tfreq le 12e3, ccinds)
	if ccinds gt 0 then begin
		power = (*rad_fit_data[data_index]).power[binds,*]
		scat = (*rad_fit_data[data_index]).gscatter[binds,*]
	endif else begin
		binds = 0
		power = fltarr(1,ngates+1) + 10000.
	endelse
	for ig=0,ngates do begin
		xx = fov_loc_full[0,*,ib,ig]
		yy = fov_loc_full[1,*,ib,ig]
		if power[0,ig] ne 10000. and scat[0,ig] eq 0b then begin
			color_ind = (MAX(where(lvl le ((power[0,ig] > scale[0]) < scale[1]))) > 0)
			col = cin[color_ind]

			polyfill, xx, yy, col=col
		endif
; 		plots, [xx, xx[0]], [yy, yy[0]], thick=1
		if (ib eq nbeams-1) then $
			plots, xx[1:2], yy[1:2];, thick=2, col=200
		if ~(ib mod 4) then $
			plots, [xx[0],xx[3]], [yy[0],yy[3]];, thick=2, col=200
		if ~(ig mod 5) then $
			plots, xx[0:1], yy[0:1];, thick=2, col=200
		if (ig eq ngates-1) then $
			plots, xx[2:3], yy[2:3];, thick=2, col=200
		if (ib eq 20) then begin
			plots, xx[1:2], yy[1:2], thick=2
			plots, [xx[0],xx[3]], [yy[0],yy[3]], thick=2
		endif
		if n_elements(showbeam) eq 1 then begin
			if (ib eq showbeam) then begin
				plots, xx[1:2], yy[1:2], thick=2
				plots, [xx[0],xx[3]], [yy[0],yy[3]], thick=2
			endif
		endif
	endfor
endfor

; Plot map
map_plot_panel, xmaps, 1, 0, 0, date=date, coords='magn', /iso, /no_fill, yrange=yrange, xrange=xrange, coast_linecolor=1
overlay_radar, name=radar, /anno, coords='magn', charsize=.5

plot_colorbar, xmaps, 1, 0, 0, charthick=charthick, $
	nlevels=8, scale=scale, charsize=charsize, $
	legend='Power [dB]', /no_rotate, $
	level_format='(I4)', /keep_first_last_label, /vert

xyouts, .5, .88, $
		radar+', '+STRMID(format_juldate(jultime),0,17), $
		align=.5, /normal, charsize=charsizeze




if keyword_set(ps) then $
	ps_close, /no_f

end
