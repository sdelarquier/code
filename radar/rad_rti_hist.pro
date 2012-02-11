pro	rad_rti_hist, date, radar, count=count

common rad_data_blk
common radarinfo


time0 = [0700,0700]
; date=[20101101,20101103]
freq_band = [10.,12.]
sfjul, date, time0, sjul, fjul, no_d=nd
hist = dblarr(150, 75)
juls = sjul + dindgen(150)*30.d/(60.d*24.d)
; print, juls, format='(D)'
datagap_flag = 0L
for i=0L, nd-1L do begin
    sfjul, adate, atime, sjul+double(i), /jul_to
    rad_fit_set_data_index, -1
    rad_fit_read, adate, radar, time=time0
    if ~PTR_VALID(rad_fit_data[0]) then $
	continue
    sfjul, adate, time0, isjul, ifjul
    inds = where( (*rad_fit_data[0]).beam eq 7 AND $
		(*rad_fit_data[0]).juls ge isjul-10.d/1440.d AND $
		(*rad_fit_data[0]).juls le ifjul+10.d/1440.d AND $
		(*rad_fit_data[0]).channel eq 0. AND $
		(*rad_fit_data[0]).tfreq*0.001 ge freq_band[0] AND $
		(*rad_fit_data[0]).tfreq*0.001 le freq_band[1], cinds)
    if cinds gt 0 then begin
	for nh=0,n_elements(juls)-1 do begin
	    jinds = where((*rad_fit_data[0]).juls[inds]-juls[nh]-double(i) lt 0.0208333 and $
			(*rad_fit_data[0]).juls[inds]-juls[nh]-double(i) ge 0., cj)
	    if cj gt 0 then begin
		datagap_flag = 0L
; 		caldat, juls[nh]+i, month, day, year, hour, minute
; 		print, month, day, year, hour, minute
		for ng=0,59 do begin
		    thist = (*rad_fit_data[0]).power[inds[jinds],ng]
		    cinds = where(thist ne 10000. and thist ge 6. and (*rad_fit_data[0]).gscatter[inds[jinds],ng] eq 1, gc)
		    if gc gt 0 then begin
; 			hist[nh,ng] += TOTAL(10^(thist[cinds]/10.))
			if ~keyword_set(count) then $
			    hist[nh,ng] += TOTAL(thist[cinds]/10.) $
			else $
			    hist[nh,ng] += gc
; 			hist[nh,ng] += MEAN(thist[cinds])*float(gc)/float(n_elements(inds[jinds]))
		    endif
		endfor
	    endif else $
		datagap_flag += 1
	    ; If there is a data gap of more than 5 hours, then we skip the whole day
	    if datagap_flag gt 10 then begin
		; But first we have to erase what has already been calculated
		for nhh=0,nh-1 do begin
		    jinds = where((*rad_fit_data[0]).juls[inds]-juls[nhh]-double(i) lt 0.0208333 and $
				(*rad_fit_data[0]).juls[inds]-juls[nhh]-double(i) ge 0., cj)
		    if cj gt 0 then begin
			for ng=0,59 do begin
			    thist = (*rad_fit_data[0]).power[inds[jinds],ng]
			    cinds = where(thist ne 10000. and thist ge 6. and (*rad_fit_data[0]).gscatter[inds[jinds],ng] eq 1, gc)
			    if gc gt 0 then begin
				if keyword_set(count) then $
				    hist[nh,ng] -= TOTAL(thist[cinds]) $
				else $
				    hist[nh,ng] -= gc
			    endif
			endfor
		    endif
		; End of erase
		endfor
		break
	    ; go to next day
	    endif
	; End of time loop
	endfor
    endif
endfor

if ~keyword_set(count) then $
    hist = hist/max(hist) $
else $
    hist = hist/max(hist)
;   hist = hist/nd

; openw, unit, 'rti_hist', /get_lun
; writeu, unit, hist, juls
; free_lun, unit
; 
; openr, unit, 'rti_hist', /get_lun
; readu, unit, hist, juls
; free_lun, unit

; ps_open, '~/Desktop/rad_rti_hist.ps'

clear_page
set_bottom, 14
set_ncolors, 240
scale=[0.,1.]
; scale=[0.,30.]
sfjul, date[0], time0, sjul, fjul
; set up coordinate system for plot
position = define_panel(1, 1, 0, 0, /bar, /with_info, no_title=no_title)
plot, [0,0], /nodata, xstyle=5, ystyle=5, $
	yrange=[0,60], xrange=[sjul,fjul], position=position

gate = findgen(60)
for nt=0,n_elements(juls)-2 do begin
    start_time = juls[nt]
    end_time = juls[nt+1] 
    for r=0,n_elements(gate)-2 do begin
	; only plot points with real data in it
	IF hist[nt,r] le 0. THEN $
		continue

	col = get_color_index(hist[nt,r], param='power', scale=scale)
	
	; finally plot the point
	POLYFILL,[start_time,start_time,end_time,end_time], $
		[gate[r],gate[r+1],gate[r+1],gate[r]], $
		COL=col,NOCLIP=0
    endfor
endfor

; Plot sunrise/sunset and solar noon
nid = where(network.code[0] eq radar)
stid = network[nid].ID
nbeams = network[nid].SITE[where(network[nid].SITE[*].TVAL eq -1)].MAXBEAM
yrsec = day_no(date[0]+15L)*24.*3600.
rad_define_beams, stid, nbeams, 60, date[0]/10000L, yrsec, coords='geog', fov_loc_center=fov_loc
rise = dblarr(n_elements(gate))
sset = dblarr(n_elements(gate))
noon = dblarr(n_elements(gate))
for r=0,n_elements(gate)-1 do begin
    calculate_sunset, date[0], fov_loc[0,11,r], fov_loc[1,11,r], $
	risetime=risetime, settime=settime, solnoon=solnoon
	
    parse_date, date[0], year, month, day
    parse_time, risetime, hour, minutes
    rise[r] = julday(month,day,year,hour,minutes)
    
    parse_time, settime, hour, minutes
    shift_d = 0
    if settime lt 1200 then $
	shift_d = 1
    sset[r] = julday(month,day+shift_d,year,hour,minutes)
    
    parse_time, solnoon, hour, minutes
    noon[r] = julday(month,day,year,hour,minutes)
;     print, risetime, solnoon, settime, fov_loc[0,11,r], fov_loc[1,11,r]
endfor
oplot, rise, gate, thick=2, linestyle=2
oplot, sset, gate, thick=2, linestyle=2
oplot, noon, gate, thick=2, linestyle=2


_xtitle = 'Time UT'
_xtickformat = 'label_date'
charsize = get_charsize(1, 1)
xticks = get_xticks(sjul, fjul, xminor=_xminor)
_ytickformat = ''
_ytickname = ''
yrange = [0.,75.]
xrange=[sjul,fjul]
plot, [0,0], /nodata, position=position, $
	charthick=charthick, charsize=charsize, $
	yrange=yrange, xrange=xrange, $
	xstyle=1, ystyle=1, xtitle=_xtitle, ytitle='Range', $
	xticks=xticks, xminor=_xminor, yticks=yticks, yminor=yminor, $
	xtickformat=_xtickformat, ytickformat=_ytickformat, $
	xtickname=_xtickname, ytickname=_ytickname, $
	color=get_foreground(), title=title
plot_colorbar, 1, 1, 0, 0, scale=scale, level_format='(f3.1)', legend='count', /with_info, /no_rotate


sfjul, date, time0, sjul, fjul, no_d=nd
title = 'Average monthly received power'
subtitle = 'Radar: '+STRTRIM(radar,2)+', beam 10'
sdate = format_juldate(sjul)
right_title = sdate+'!C!5 to !C!5'+format_juldate(fjul)
plot_title, title, subtitle, top_right_title=right_title

; ps_close

end
