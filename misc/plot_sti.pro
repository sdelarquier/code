; procedure to plot a season time color graph
; 
; DATA: 2D array with 1st dim as day-of-year data points
; and 2nd dim as time-of-day data points, all linearly spaced
;
; SX: day-of-year vector. Must be same size as DATA 1st dim.
;
; TY: time-of-day vector. Must be the same size as DATA 2nd dim.
;
; SCALE: upper and lower limit of the data for color scale
;
; DAYS: 2 elements array with begining and end day in day-of-year.
; Default is [0,365] or whatever is in the 'Sx'
;
; TIMES: 2 elements array wih begining and end time of day.
; Default is [0.,24.] or whatever is in the 'Ty' variable
;
pro	plot_sti, data, scale, sx=sx, ty=ty, $
	days=days, times=times, $
	charsize=charsize, title=title, $
	position=position, legend=legend

if ~keyword_set(sx) then begin
    if ~ keyword_set(days) then $
	days = [0., 365.]
    sx = days[0] + indgen(days[1]-days[0])
endif
nd = n_elements(sx)
if ~keyword_set(ty) then begin
    if ~ keyword_set(times) then $
	times = [0., 24.]
    ty = times[0] + indgen(times[1]-times[0])
endif
nt = n_elements(ty)

; Check that the data is consistent with axis limits
sizata = size(data)
if (nt ne sizata[2]) or (nd ne sizata[1]) then begin
    print, 'number of elements mismatch'
    return
endif

; generate months axis
juldates = TIMEGEN(nd+1,units='days',start=julday(1,1,2010)+days[0])
xrange = [juldates[0], juldates[nd-1]]

; generate time axis
jultimes = TIMEGEN(nt+1,units='hours',start=julday(1,1,2010,times[0]))
yrange = [jultimes[0], jultimes[nt-1]]

; position axis
if ~keyword_set(position) then begin
    xmaps = 1
    ymaps = 1
    xmap = 0
    ymap = 0
    position = define_panel(xmaps, ymaps, xmap, ymap, /bar, with_info=with_info)
    if ~keyword_set(charsize) then $
	charsize = get_charsize(xmaps, ymaps)
endif
bpos = define_cb_position(position, /vertical, gap=0.01, width=0.015)    
plot, [0,0], /nodata, xstyle=5, ystyle=5, $
    yrange=yrange, xrange=xrange, position=position
    
; start plotting
for id=0,nd-1 do begin
    sday = juldates[id]
    bday = juldates[id+1]
    for it=0,nt-1 do begin
	shr = jultimes[it]
	bhr = jultimes[it+1]
	
	col = get_color_index(data[id,it], scale=scale, colorsteps=255)
	
	; finally plot the point
	POLYFILL,[sday,bday,bday,sday],[shr,shr,bhr,bhr], $
		COL=col,NOCLIP=0
    endfor
endfor

; "over"plot axis
dum = label_date(date_format='%M')
plot, [0,0], /nodata, position=position, $
	charthick=2., charsize=charsize, $
	xrange=xrange, xtickformat='label_date', $
	xstyle=1, ystyle=4, xtickunit='Months',  $
	xtitle='Month'
dum = label_date(date_format='%H:%I')
plot, [0,0], /nodata, position=position, $
	charthick=2., charsize=charsize, $
	yrange=yrange, ytickformat='label_date', $
	xstyle=4, ystyle=1, ytickunit='Hours', $
	ytitle='Hour LT',  title=title

; plot color bar
plot_colorbar, /vert, charthick=2, /bar, /continuous, $
	scale=scale, position=bpos, charsize=charsize, $
	legend=legend, nlevels=4

end