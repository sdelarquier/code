PRO	milh_plot_ati, param=param

clear_page
milh_read, nlines, juls, alt, az, el, range, lat, lon, ti, te, nel, vo, dti, dtr, dnel, dvo

; azimuth to consider
azm = -90.

if ~keyword_set(param) then $
	param = 'vo'
nparams = n_elements(param)

sjul = JULDAY(11, 17, 2010, 21, 00)
fjul = JULDAY(11, 18, 2010, 13, 00)
xrange = [sjul, fjul]
xticks = get_xticks(sjul, fjul, xminor=_xminor)
yrange = [0.,500.]
_xtickformat = 'label_date'
_xtitle = 'Time (UT)'
_xtickname = ''
_ytitle = 'Altitude [km]'
_ytickformat = ''
_ytickname = ''
xstyle = 1
ystyle = 1

; Set number of panels
xmaps = 1
ymaps = nparams
nsum = 2
charsize = get_charsize(1, 2)
if nparams gt 3 then $
  set_format, /sardines, /portrait $
else $
  set_format, /sardines, /landscape

; Data
; xdata = juls[UNIQ(juls,sort(juls))]
xdata = timegen(500,start=sjul,final=fjul,minutes=indgen(6)*10)
nev = n_elements(xdata)
; ydata = alt[UNIQ(alt[where(alt le yrange[1])],sort(alt[where(alt le yrange[1])]))]
ydata = findgen(50.)*20.
nalts = n_elements(ydata)
zdata = fltarr(n_elements(xdata),n_elements(ydata))
print, nev, nalts
; CALDAT, xdata, month, day, year, hour, minute
; print, hour*100L+minute
for np=0,nparams-1 do begin
	CASE param[np] OF
		'vo': 	begin
			  ytitle = textoidl('Velocity [m/s]')
			  scale = [-100.,100.]
			  paramcol = 'velocity'
			end
		'ti': 	begin
			  ytitle = textoidl('T_i [K]')
			  scale = [600.,1000.]
			  paramcol = 'power'
			end
		'te': 	begin
			  ytitle = textoidl('T_e [K]')
			  scale = [500.,3000.]
			  paramcol = 'power'
			end
		'nel': begin
			  ytitle = textoidl('N_{el} [log(m^{3})]')
			  scale = [9.,12.]
			  paramcol = 'power'
			end
	ENDCASE
	xmap = 0
	ymap = np
	
	if ymap eq nparams-1 then $
		last = 1L $
	else $
		last = 0L

	s = execute('paramdata = '+param[np])
	
	position = define_panel(xmaps, ymaps, xmap, ymap, /bar, with_info=with_info)
	charsize = get_charsize(xmaps, ymaps)
	; set up coordinate system for plot
	plot, [0,0], /nodata, xstyle=5, ystyle=5, $
		yrange=yrange, xrange=xrange, position=position
	
	for nt=0,nev-2 do begin
		start_time = xdata[nt]
; 		caldat, start_time, month, day, year, hour, mn
; 		print, day, month, year, hour, mn
		end_time = xdata[nt+1]
		for na=0,nalts-2 do begin
			inds = where(juls ge xdata[nt] and $
				juls lt xdata[nt+1] and $
				alt ge ydata[na] and $
				alt lt ydata[na+1] and $
				az ge azm-1. and az le azm+1. and $
				nel gt 0., cc)
			if cc gt 0 then begin
			    zdata[nt,na] = mean(paramdata[inds])
			    col = get_color_index(zdata[nt,na], scale=scale, colorsteps=255, param=paramcol)
			    
			    ; finally plot the point
			    POLYFILL,[start_time,start_time,end_time,end_time], $
					    [ydata[na],ydata[na+1],ydata[na+1],ydata[na]], $
					    COL=col,NOCLIP=0
			endif
		endfor
	endfor

	fmt = get_format(sardines=sd, tokyo=ty)
	if sd and last eq 0L then begin
		_xtitle = ' '
		_xtickformat = ''
		_xtickname = replicate(' ', 60)
	endif else begin
		_xtickformat = 'label_date'
		_xtitle = 'Time (UT)'
		_xtickname = ''
	endelse

	
	bpos = define_cb_position(position, /vertical, gap=0.01, width=0.01)   
	; plot color bar
	plot_colorbar, /vert, charthick=2, /bar, /continuous, $
		scale=scale, position=bpos, charsize=charsize, $
		legend=ytitle, nlevels=3, param=paramcol, level_format='(F5.1)'

	; "over"plot axis
	plot, [0,0], /nodata, position=position, $
		charthick=charthick, charsize=charsize, $
		yrange=yrange, xrange=xrange, $
		xstyle=1, ystyle=ystyle, xtitle=_xtitle, ytitle=_ytitle, $
		xticks=xticks, xminor=_xminor, yticks=yticks, yminor=yminor, $
		xtickformat=_xtickformat, ytickformat=_ytickformat, $
		xtickname=_xtickname, ytickname=_ytickname, $
		color=get_foreground(), title=title
endfor

title = 'MILSTONE HILL'
subtitle = 'Parameter plot'
sdate = format_juldate(sjul)
right_title = sdate+'!C!5 to !C!5'+format_juldate(fjul)
plot_title, title, subtitle, top_right_title=right_title

END