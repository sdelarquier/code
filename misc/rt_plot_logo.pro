pro rt_plot_logo, time

common rt_data_blk


Rav = 6370.

rthick = 1
beam = rt_data.beam[0,0]
ib = 0

; Retrieve raytracing parameters from structure
radar = rt_info.name
beam	= beam
caldat, rt_data.juls[*,0], month, day, year, hours, minutes
tdate 	= year*10000L + month*100L + day

parse_time, time, hour, minute
date = tdate[0]
timeind = where(tdate eq date and hours eq hour and minutes eq minute)
juls = julday(month[timeind], day[timeind], year[timeind], hours[timeind], minutes[timeind])

; Test for data availability
rt_read_rays, date, radar, time, radpos=radpos, thtpos=thtpos, grppth=grppth, raysteps=raysteps, code=code
if ~code then $
	return

; If data is present, limit it to selected beam
radpos = reform(radpos[ib,*,*])
thtpos = reform(thtpos[ib,*,*])
grppth = reform(grppth[ib,*,*])
raysteps = reform(raysteps[ib,*])
nrays = n_elements(radpos[*,0])

; Adjust parameters for current device
charsize = 1.
position = [.08,.2,.88,.8]
bpos = [.895,.15,.91,.55]	

; Generate plotting area
maxr = 2000.
maxh = 500.
tht0 = maxr/Rav/2.
xmin = -(Rav + maxh) * sin (tht0)
xmax = (Rav + maxh) * sin (tht0)
xran = [xmin, xmax*1.01]
ymin = Rav * cos (tht0)
ymax = Rav + maxh
yran = [ymin, ymax*1.01]

for ir=0,nrays-1,5 do begin
	filename = 'rlog_'+STRTRIM(string(ir,format='(I04)'),2)
	ps_open, filename+'.ps'

	plot, xran, yran, /nodata, xstyle=5, ystyle=5, /iso, position=position


	;*****************************************************************************
	; Axis and formating
	;*****************************************************************************
	;- plot graph limits
	; Top and bottom axis
	thetas = -tht0 + findgen(101)*(2.*tht0)/100.
	oplot, Rav*sin(thetas), Rav*cos(thetas), thick=2


	;*****************************************************************************
	;- plot rays
	;*****************************************************************************
	loadct, 0
	nrays = n_elements(radpos[*,0])
	hmax = 0.
	cc = 0
	for nr=0,ir,5 do begin
		nrsteps = raysteps[nr]
		for ns=1,nrsteps-1 do begin
			xx = [radpos[nr,ns-1]*1e-3*sin(-tht0 + thtpos[nr,ns-1]), $
						radpos[nr,ns]*1e-3*sin(-tht0 + thtpos[nr,ns])]
			yy = [radpos[nr,ns-1]*1e-3*cos(-tht0 + thtpos[nr,ns-1]), $
						radpos[nr,ns]*1e-3*cos(-tht0 + thtpos[nr,ns])]
			if thtpos[nr,ns] lt maxr/Rav then $
				oplot, xx, yy, thick=rthick, color=100
		endfor
	endfor

	ps_close, /no_filename
	spawn, 'ps2png.sh ~/'+filename+'.ps'
	spawn, 'rm -f ~/'+filename+'.ps'

endfor

for ir=nrays-1,0,-5 do begin
	filename = 'rlog_'+STRTRIM(string(2*nrays-ir,format='(I04)'),2)
	ps_open, filename+'.ps'

	plot, xran, yran, /nodata, xstyle=5, ystyle=5, /iso, position=position


	;*****************************************************************************
	; Axis and formating
	;*****************************************************************************
	;- plot graph limits
	; Top and bottom axis
	thetas = -tht0 + findgen(101)*(2.*tht0)/100.
	oplot, Rav*sin(thetas), Rav*cos(thetas), thick=2


	;*****************************************************************************
	;- plot rays
	;*****************************************************************************
	loadct, 0
	nrays = n_elements(radpos[*,0])
	hmax = 0.
	cc = 0
	for nr=0,ir,5 do begin
		nrsteps = raysteps[nr]
		for ns=1,nrsteps-1 do begin
			xx = [radpos[nr,ns-1]*1e-3*sin(-tht0 + thtpos[nr,ns-1]), $
						radpos[nr,ns]*1e-3*sin(-tht0 + thtpos[nr,ns])]
			yy = [radpos[nr,ns-1]*1e-3*cos(-tht0 + thtpos[nr,ns-1]), $
						radpos[nr,ns]*1e-3*cos(-tht0 + thtpos[nr,ns])]
			if thtpos[nr,ns] lt maxr/Rav then $
				oplot, xx, yy, thick=rthick, color=100
		endfor
	endfor

	ps_close, /no_filename
	spawn, 'ps2png.sh ~/'+filename+'.ps'
	spawn, 'rm -f ~/'+filename+'.ps'

endfor

spawn, 'convert -delay 10 rlog_????.png ~/Desktop/rtwait.gif'
; spawn, 'rm -f rlog_????.png'

end