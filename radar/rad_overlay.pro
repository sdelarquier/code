pro	rad_overlay, date, radars, time=time

if ~keyword_set(time) then $
	time=1200


time = time + 30
timearr = [time-100L,time+200L]
parse_date, date, yyyy, mm, dd
parse_time, time, hr, mn
scan_jul = julday(mm,dd,yyyy, hr, mn)

; WINDOW, 0, xsize=800, ysize=800
clear_page
map_plot_panel, date=date, time=time, coords='mlt', /no_fill, xrange=[-15,20],yrange=[20,45], $
	coast_linecolor=1

for r=0, n_elements(radars)-1 do begin
	rad_fit_read, date, radars[r], time=timearr
	rad_fit_overlay_scan, jul=scan_jul, coords='mlt', param='power', scale=[0,30]
	overlay_fov, names=radars[r], jul=scan_jul, coords='mlt', /no_fill, nranges=70
endfor
rad_fit_plot_title, STRTRIM(time,2)+'UT', ' ', /no_date


END