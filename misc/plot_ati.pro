; procedure to plot a altitude time color graph
; 
; DATA: 2D array with 2nd dim as altitude data points
; and 1st dim as time-of-day data points, all linearly spaced
;
; AY: altitude vector. Must be same size as DATA 1st dim.
;
; TX: time-of-day vector. Must be the same size as DATA 2nd dim.
;
; SCALE: upper and lower limit of the data for color scale
;
; ALTS: 2 elements array with begining and end altitudes in km.
; Default is [100,250] or whatever is in the 'AY'
;
; TIMES: 2 elements array wih begining and end time of day.
; Default is [0.,24.] or whatever is in the 'TX' variable
;
pro	plot_ati, data, scale, tx=tx, ay=ay, $
	alts=alts, times=times, $
	charsize=charsize, title=title, $
	position=position, legend=legend


sizata = size(data)
nt = sizata[1]
na = sizata[2]
if ~keyword_set(ay) then begin
    if ~ keyword_set(alts) then $
	alts = [100., 250.]
    ay = alts[0] + indgen(na+1)*(alts[1]-alts[0])/na
endif
if ~keyword_set(tx) then begin
    if ~ keyword_set(times) then $
	times = [0., 24.]
    tx = times[0] + indgen(nt+1)
endif

; generate months axis
yrange = alts
print, yrange

; generate time axis
jultimes = TIMEGEN(nt+1,units='hours',start=julday(1,1,2010,times[0]))
xrange = [jultimes[0], jultimes[nt-1]]

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
bpos = define_cb_position(position, /vertical, gap=0.01, width=0.01)    
plot, [0,0], /nodata, xstyle=5, ystyle=5, $
    yrange=yrange, xrange=xrange, position=position
    
; start plotting
for ia=0,na-1 do begin
    salt = ay[ia]
    balt = ay[ia+1]
    for it=0,nt-1 do begin
	shr = jultimes[it]
	bhr = jultimes[it+1]
	
	col = get_color_index(data[it,ia], scale=scale, colorsteps=255)
	
	; finally plot the point
	POLYFILL,[shr,bhr,bhr,shr],[salt,salt,balt,balt], $
		COL=col,NOCLIP=0
    endfor
endfor

; "over"plot axis
plot, [0,0], /nodata, position=position, $
	charthick=2., charsize=charsize, $
	xrange=xrange, xtickformat='label_date', $
	xstyle=1, ystyle=1, xtickunit='Hours', $
	xtitle='Hour LT',  title=title, $
	ytitle='Alt. [km]', yrange=yrange

; plot color bar
plot_colorbar, /vert, charthick=2, /bar, /continuous, $
	scale=scale, position=bpos, charsize=charsize, $
	legend=legend, nlevels=4

end