pro	rt_plot_nmf2_param, time, date=date, beam=beam, ps=ps

common rt_data_blk

Rav = 6370.

if ~keyword_set(beam) then begin
	beam = 0
endif
binds = where(rt_data.beam[0,*] eq beam)
ib = binds[0]

; Retrieve raytracing parameters from structure
radar = rt_info.name
caldat, rt_data.juls[*,0], month, day, year, hours, minutes
tdate 	= year*10000L + month*100L + day

parse_time, time, hour, minute
if ~keyword_set(date) then $
	date = tdate[0]
timeind = where(tdate eq date and hours eq hour and minutes eq minute)
juls = julday(month[timeind], day[timeind], year[timeind], hours[timeind], minutes[timeind])

; Open postscript if desired
if keyword_set(ps) then begin
	ps_open, '~/Desktop/nmf2bias_'+radar+STRTRIM(beam,2)+'_'+STRTRIM(date,2)+'_'+ $
		STRTRIM(STRING(time,format='(I04)'),2)+rt_info.timez+'.ps', /no_init
	clear_page
endif

; Find NmF2 at max elevation position (which according to RT closely matches actual NmF2)
gaterange = [5,40]
maxelev = max(rt_data.elevation[timeind[0],ib,gaterange[0]:gaterange[1]], maxelevind)
groundNe = findgen(n_elements(rt_data.edens[timeind[0],ib,0,*]))*2500./n_elements(rt_data.edens[timeind[0],ib,0,*])
thtind = where(groundNe ge rt_data.grange[timeind[0],ib,gaterange[0]+maxelevind])
nmf2 = max(rt_data.edens[timeind[0],ib,*,thtind[0]], nmf2ind)

; The real NmF2 is only used whith no horizontal gradients
nmf2real = max(rt_data.edens[timeind[0],ib,*,0], nmf2realind)

; Find azimuth
az = rt_data.azim[timeind[0],ib]

; Set default altitude
hs = rt_data.altitude[timeind[0],ib,maxvelind]

; Set range where dip and eclination values are defined
gind = where(groundNe ge rt_data.grange[timeind[0],ib,maxelevind])
I = rt_data.dip[timeind[0],ib,0,gind[0]]
D = rt_data.dip[timeind[0],ib,1,gind[0]]



end