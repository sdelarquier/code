PRO rt_plot_hist, radar=radar, date=date, time=time

common rt_data_blk
common radarinfo

; Retrieve date and time information
if ~keyword_set(radar) then $
	radar = rt_info.name
if ~keyword_set(date) then begin
	caldat, rt_data.juls, month, day, year, hours, minutes
	date 	= year*10000L + month*100L + day
endif
if ~keyword_set(time) then begin
	caldat, rt_data.juls, month, day, year, hours, minutes
	time = hours*100L + minutes
endif else $
	time = time[0] + findgen(time[1]-time[0]+1)
tsteps = n_elements(time)

ngates = 60
range_gate = [0, 180L + 45L*FINDGEN(ngates)]

if MEAN(rt_data.hist eq 0.) then begin
	; cycle through beams
	hist = dindgen(16,ngates)*0D
	power = dindgen(tsteps,ngates)*0D
	ionospower = dindgen(tsteps,ngates)*0D
	for b=0,15 do begin
		; cycle through time steps
		for t=0,tsteps-1 do begin
			; Test for data availability
			dirname='/tmp/rt/ray_'+radar+STRTRIM(b,2)+'_'+STRTRIM(date[t],2)+'_'+ $
					STRTRIM(STRING(time[t],format='(I04)'),2)+rt_info.timez
			print, dirname
			dirTest = FILE_TEST(dirname+'/raytrace.ps', /READ)
			if (dirTest ne 1) then begin
				print, 'No data found for beam '+STRTRIM(b,2)
			endif
			
			; Read ground scatter
			OPENR, unit, dirname+'/ranges.dat', /get_lun
			rt_read_ranges, unit, numran, gndran, grpran, ranelv, ranalt
			FREE_LUN, unit
			power[t,*] = 10D*ALOG10( histc(grpran,range_gate) )
			inds = where(power[t,*] lt 0.,cc)
			if cc gt 0 then $
				power[t,inds] = 0.
		
			; Read potential ionospheric scatter
			rt_get_normrays, normpath, normelev, dirname=dirname
			ionospower[t,*] = 10D*ALOG10( histc(normpath,range_gate) )
			inds = where(ionospower[t,*] lt 0.,cc)
			if cc gt 0 then $
				ionospower[t,inds] = 0.
			
			; Adds them up
			data = power[t,*] + ionospower[t,*]

			; Make the histogram
			inds = where(data gt 0., cinds)
			if cinds gt 0 then $
				hist[b,inds] += data[inds]
		; end cycle through time step
		endfor
	; end cycle through beams
	endfor
	
	; Save result in the common structure (just in case)
	rt_data.hist = hist
endif else $
	hist = rt_data.hist

hist = hist/max(hist[*,5:*])

ps_open, 'rt_hist.ps'
clear_page
map_plot_panel, yrange=[-51,-11], /no_fill, coords='magn', xrange=[-31,11], date=date[0], time=time[0]
rad_overlay_scan, hist, network[where(network.code[0] eq radar)].ID, scale=[0L,1L], coords='magn', date=date[0], time=time[0]
overlay_fov, name=radar, /grid, /no_fill, fov_linecolor=get_black(), fov_linethick=1., nranges=ngates, date=date[0], time=time[0]
overlay_coast, coords='magn', /no_fill, coast_linethick=1, coast_linecolor=get_black()
plot_colorbar, scale=[0L,1L], param='power', legend='Counts'
overlay_isr, 'msh', azim=-85.+findgen(5)*5., date=date[0], time=time[0]
ps_close

END