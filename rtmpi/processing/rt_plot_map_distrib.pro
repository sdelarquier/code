pro	rt_plot_map_distrib, date

common rt_data_blk

radtemp = 'wal'
geopos = [29.7,-82.3]	; florida
geopos = [32.1,-81.1]	; georgia

ps_open, '~/Desktop/rt_distrib.ps'

saz = rt_get_azim(radtemp, 0, date)
faz = rt_get_azim(radtemp, 23, date)
rt_run, date, 'custom', /no_rays, geopos=geopos, azim=[saz+8.,faz+8.,3.24], outdir='/tmp/';, /force
;rt_run, date, radtemp, /no_rays
calculate_sunset, date, rt_info.glat, rt_info.glon, solnoon=tsolnoon
parse_date, date, year, month, day
parse_time, tsolnoon, hour, minutes
solnoon = julday(month,day,year,hour,minutes)
julmidnight = solnoon - 0.5d

; Correct power
pinds = where(rt_data.power eq 10000. and rt_data.gscatter ne 2b, ccpinds)
if ccpinds gt 0 then $
	rt_data.power[pinds] = 0.

indsmidnight = where(rt_data.juls ge julmidnight-3.5d/24.d and $
					rt_data.juls le julmidnight+3.5d/24.d, nnighttimes)
hist = fltarr(n_elements(rt_data.beam[0,*]),n_elements(rt_data.power[0,0,*]))
grange = fltarr(n_elements(rt_data.beam[0,*]),n_elements(rt_data.power[0,0,*]))
cc = 0L
if nnighttimes gt 0 then begin
	for it=0,nnighttimes-1 do begin
		hist = hist + min(rt_data.power[indsmidnight[it],*,*]) / rt_data.power[indsmidnight[it],*,*]
		ioinds = where(rt_data.gscatter[indsmidnight[it],*,*] eq 2b, ccio)
		tgrange = rt_data.grange[indsmidnight[it],*,*]
		grange[ioinds] = (grange[ioinds]*cc + tgrange[ioinds])/(cc + 1)
		cc = cc + ccio
	endfor
endif
rt_data.power[0,*,*] = hist/max(hist,/nan)
rt_data.grange[0,*,*] = grange

rt_plot_map, 0, param='power'

ps_close, /no_filename

end
