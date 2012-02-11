pro	milh_plot_dl

openr, unit, '~/Documents/IDL/milh/NHee_idl', /get_lun
nlines = 0L
readu, unit, nlines
juls = dblarr(nlines)
azm = fltarr(nlines)
elm = fltarr(nlines)
alt = fltarr(nlines)
range = fltarr(nlines)
nel = dblarr(nlines)
nelmodd = dblarr(nlines)
vo = dblarr(nlines)
readu, unit, juls, azm, alt, alt, range, nel, nelmodd, vo
free_lun, unit

CALDAT, juls[0],month, day, year, hour
sjul = julday(month, day, year, hour)
CALDAT, juls[n_elements(juls)-1], month, day, year, hour
fjul = julday(month, day, year, hour+2L)

inds = where(nel ne 9999.99)
Ma = max(nel[inds])
mi = min(nel[inds])


; Reorganize the data
xrange = [sjul, fjul]
xdata = sjul + dindgen( round( (fjul - sjul)/(10.d/1440.d) ) )*(10.d/1440.d)
yrange = [100.,400.]
ydata = yrange[0] + findgen(round( (yrange[1] - yrange[0])/8. ))*8.
edens = dblarr(n_elements(xdata),n_elements(ydata))
for nx=0,n_elements(xdata)-2 do begin
	for ny=0,n_elements(ydata)-2 do begin
		inds = where(juls ge xdata[nx] and $
					juls lt xdata[nx+1] and $
					alt ge ydata[ny] and $
					alt lt ydata[ny+1] and $
					nel ne 9999.99, cc)
		if cc gt 0. then $
			edens[nx,ny] = 10.^(mean(nel[inds], /double, /nan))
	endfor
endfor

edens = smooth(edens, 5, /nan)

set_format, /sardines;, /portrait
clear_page


;- ********************************************************************
;- ********************************************************************
; rad_fit_read, [20100813,20100814], 'bks', time=[1300,500]
; rad_fit_plot_rti_panel, 1, 2, 0, 0, param='power', /bar, time=[1300,500], date=[20100813,20100814], yrange=[0.,70.], /sun, beam=10
; plot_colorbar, 1, 2, 0, 0, param='power'
rt_run, 20100813, 'bks', time=[1300,500], beam=10
position = [.08,.2,.88,.6]
bpos = [.895,.2,.91,.6]
rt_plot_rti_panel, 1,2,0,0, param='power', /bar, time=[1300,500], /sun, scale=[0.,20.], charsize=1., position=position
plot_colorbar, 1, 2, 0, 0, param='power', scale=[0.,20.], charsize=1., position=bpos


;- ********************************************************************
;- ********************************************************************
; position = define_panel(1, 2, 0, 1, /bar, with_info=with_info, no_title=no_title)
; plot, [0,0], /nodata, xstyle=5, ystyle=5, $
; 	yrange=yrange, xrange=xrange, position=position
; ; juldata = juls[UNIQ(juls,sort(juls))]
; ; caldat, xdata, month, day, year, hour, mint
; ; print, [transpose(hour),transpose(mint)]
; ; altdata = alt[UNIQ(alt,sort(alt))]
; hmf2 = fltarr(n_elements(xdata))
; nmf2 = fltarr(n_elements(xdata))
; loadct, 0, file='/tmp/colors2.tbl'
; for nx=0,n_elements(xdata)-3 do begin
;     for ny=0,n_elements(ydata)-2 do begin
; 		if edens[nx,ny] gt 0. then begin
; 			tmax = edens[nx,ny]
; 			if tmax gt nmf2[nx] then begin
; 				nmf2[nx] = tmax
; 				hmf2[nx] = ydata[ny]
; 			endif
; 			
; 			col = bytscl(edens[nx,ny]*1e-11, min=0.1, max=5., top=252) + 2
; 
; 			POLYFILL,[xdata[nx],xdata[nx],xdata[nx+2],xdata[nx+2]], $
; 				[ydata[ny],ydata[ny+1],ydata[ny+1],ydata[ny]], $
; 				COL=col,NOCLIP=0
; 		endif
;     endfor
; endfor
; month = 8
; day = 13
; year = 2010
; ; At MH
; calculate_sunset, 20100813, 42.6, 288.5, $
; 	risetime=risetime, settime=settime, solnoon=solnoon
; parse_time, settime, hour, minutes
; shift_d = 0
; if settime lt 1200 then $
; 	shift_d = 1
; sset = julday(month,day+shift_d,year,hour,minutes)
; parse_time, solnoon, hour, minutes
; shift_d = 0
; if solnoon lt 1200 then $
; 	shift_d = 1
; snoon = julday(month,day+shift_d,year,hour,minutes)
; oplot, [snoon, snoon], [0., 800.], linestyle=2, thick=3
; oplot, [sset, sset], [0., 800.], linestyle=2, thick=3
; parse_time, solnoon, hour, minutes
; ;- At conjugate point
; ; calculate_sunset, 20100813, -66.6, 291.8, $
; ; 	risetime=risetime, settime=settime, solnoon=solnoon
; ; parse_time, settime, hour, minutes
; ; shift_d = 0
; ; if settime lt 1200 then $
; ; 	shift_d = 1
; ; sset = julday(month,day+shift_d,year,hour,minutes)
; ; oplot, [sset, sset], [0., 800.], linestyle=2, thick=3
; ; parse_time, solnoon, hour, minutes
; 
; oplot, xdata, smooth(hmf2,5), thick=2, min_val=200., max_val=400.
; 
; plot_colorbar,1,2,0,1, legend='Electron Density [10!E11!Nm!E-3!N]', charsize=get_charsize(1, 2), $
; 		scale=[0.1,5.], /continuous, param='power', level_format='(f4.2)', nlevels=4, /keep_first_last_label
; 
; _xtickformat = 'label_date'
; _ytickformat = ''
; _xtickname = '';replicate(' ',60)
; xstyle = 1
; ystyle = 1
; charsize = get_charsize(1, 2)
; xticks = 8
; _xtitle = ''
; _ytitle = 'Altitude (km)'
; plot, [0,0], /nodata, position=position, $
; 	charthick=charthick, charsize=charsize, $
; 	yrange=yrange, xrange=xrange, $
; 	xstyle=xstyle, ystyle=ystyle, xtitle=_xtitle, ytitle=_ytitle, $
; 	xticks=xticks, xminor=_xminor, yticks=yticks, yminor=yminor, $
; 	xtickformat=_xtickformat, ytickformat=_ytickformat, $
; 	xtickname=_xtickname, ytickname=_ytickname, $
; 	color=get_foreground(), title=title



;- ********************************************************************
;- ********************************************************************
; loadct, 1, file='/tmp/colors2.tbl'
; position = define_panel(1, 2, 0, 1, /bar, with_info=with_info, no_title=no_title)
; ; set up coordinate system for plot
; plot, [0,0], /nodata, xstyle=5, ystyle=5, $
; 	yrange=yrange, xrange=xrange, position=position
; zdata = dblarr(n_elements(xdata),n_elements(ydata))
; hmf2 = fltarr(n_elements(xdata))
; nmf2 = fltarr(n_elements(xdata))
; 
; nelnoon = dblarr(n_elements(ydata))
; noonmin = min(abs(snoon-xdata)*1440.d, nind)
; nelnoon = edens[nind,*]
; 
; scale = [.3,1.7]
; 
; for nx=0,n_elements(xdata)-3 do begin
;     for ny=0,n_elements(ydata)-2 do begin
; 	if edens[nx,ny] gt 0 then begin
; 	    zdata[nx,ny] = edens[nx,ny]/nelnoon[ny]
; 	    
; 	    col = bytscl(zdata[nx,ny], min=scale[0], max=scale[1], top=252) + 2
; 		POLYFILL,[xdata[nx],xdata[nx],xdata[nx+2],xdata[nx+2]], $
; 			[ydata[ny],ydata[ny+1],ydata[ny+1],ydata[ny]], $
; 			COL=col,NOCLIP=0
; 	endif
;     endfor
; endfor
; month = 8
; day = 13
; year = 2010
; ; At MH
; calculate_sunset, 20100813, 42.6, 288.5, $
; 	risetime=risetime, settime=settime, solnoon=solnoon
; parse_time, settime, hour, minutes
; shift_d = 0
; if settime lt 1200 then $
; 	shift_d = 1
; sset = julday(month,day+shift_d,year,hour,minutes)
; parse_time, solnoon, hour, minutes
; shift_d = 0
; if solnoon lt 1200 then $
; 	shift_d = 1
; snoon = julday(month,day+shift_d,year,hour,minutes)
; oplot, [snoon, snoon], [0., 800.], linestyle=2, thick=3
; oplot, [sset, sset], [0., 800.], linestyle=2, thick=3
; parse_time, solnoon, hour, minutes
; 
; plot_colorbar,1,2,0,1, legend='Electron density ratio', charsize=get_charsize(1, 2), $
; 		scale=scale, /continuous, param='power', level_format='(f4.2)', nlevels=4, /keep_first_last_label
; 
; _xtickformat = 'label_date'
; _ytickformat = ''
; _xtickname = ''
; xstyle = 1
; ystyle = 1
; charsize = get_charsize(1, 2)
; xticks = 8
; _xtitle = 'Time (UT)'
; _ytitle = 'Altitude (km)'
; plot, [0,0], /nodata, position=position, $
; 	charthick=charthick, charsize=charsize, $
; 	yrange=yrange, xrange=xrange, $
; 	xstyle=xstyle, ystyle=ystyle, xtitle=_xtitle, ytitle=_ytitle, $
; 	xticks=xticks, xminor=_xminor, yticks=yticks, yminor=yminor, $
; 	xtickformat=_xtickformat, ytickformat=_ytickformat, $
; 	xtickname=_xtickname, ytickname=_ytickname, $
; 	color=get_foreground(), title=title


;- ********************************************************************
;- ********************************************************************
; top_pos = [position[0],position[3]*1.05, $
; 	position[2],position[3]*1.4]
; plot, xdata[0:*:2], nmf2[0:*:2]*1e-11, thick=2, $
; 	position=top_pos, xrange=xrange, xstyle=xstyle, $
; 	charthick=charthick, charsize=charsize, $
; 	yrange=yrange, ystyle=ystyle, ytitle=_ytitle, $
; 	xticks=xticks, xminor=_xminor, yticks=yticks, yminor=yminor, $
; 	xtickformat='', ytickformat=_ytickformat, $
; 	xtickname=replicate(' ',60), ytickname=_ytickname, $
; 	color=get_foreground(), title=title

end
