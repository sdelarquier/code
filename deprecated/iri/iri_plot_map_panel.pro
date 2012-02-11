pro	iri_plot_map_panel, xmaps, ymaps, xmap, ymap, date, time, res=res, $
		alt=alt, scale=scale, charsize=charsize

parse_date, date, year, month, day
parse_time, time, hour, minutes
hour = hour + minutes/60.

if ~keyword_set(alt) then $
    alt = 300.
if ~keyword_set(res) then $
    res = 1.

lonbeg = 0.
lonend = 360.
lonstp = res
xlat = -90
nlats = FIX(180./res)
nlons = FIX(360./res)
nel = dblarr(nlats+1,nlons+1)
tnel = dblarr(500)

if ~keyword_set(scale) then begin
    nelmin = 1e10
    nelmax = 1.1e12
endif else begin
    nelmin = scale[0]
    nelmax = scale[1]
endelse
print, nelmin, nelmax

lon = -180. + findgen(nlons)*res
lat = -90. + findgen(nlats)*res

; xmaps = 1
; ymaps = 1
; xmap = 0
; ymap = 0
yrange = [-90.,90.]
xrange = [-180.,180.]
position = define_panel(xmaps, ymaps, xmap, ymap, /bar, with_info=with_info)
bpos = define_cb_position(position, /vertical, gap=0.01, width=0.02)
; charsize = get_charsize(xmaps, ymaps)
; charsize=.5
title = 'IRI - 300 km - '+STRTRIM(month,2)+'/'+STRTRIM(day,2)+'/'+STRTRIM(year,2)+' - '+STRTRIM(hour,2)+'LT'
; clear_page
; plot, [0,0], /nodata, xstyle=5, ystyle=5, $
;     yrange=yrange, xrange=xrange, position=position
MAP_SET, /cylindrical, title=title, /noerase, charsize=charsize, $
	/continents, /hires, /grid, position=position, $
	limit=[-90.,-180.,90.,180.]
	


for nlat=1,nlats-1 do begin
    spawn, 'rm inp_file'
    
    ; 'jmag(=0/1,geog/geom),lati/deg,long/deg'
    spawn, 'echo 0,'+STRTRIM(xlat,2)+',0 >> inp_file'
    ; 'year(yyyy),mmdd(or -ddd),iut(=0/1,LT/UT),hour'
    rdate = STRMID(date,4,4)+','+STRMID(date,8,2)+STRMID(date,10,2)+',0,'+STRTRIM(hour,2)
    spawn, 'echo '+rdate+' >> inp_file'
    ; 'height/km'
    ; '(enter  0 for list of peak heights and densities
    spawn, 'echo '+STRTRIM(alt,2)+' >> inp_file'
    ; 'upper height [km] for TEC integration (0 for no TEC)'
    spawn, 'echo 600 >> inp_file'
    ; 'variable? (1/2/../8 for height/lat/long/year/month/day/day of year/hour)
    spawn, 'echo 3 >> inp_file'
    ; 'begin, end, and stepsize for the selected variable'
    spawn, 'echo '+STRTRIM(lonbeg,2)+','+STRTRIM(lonend,2)+','+STRTRIM(lonstp,2)+' >> inp_file'
    ; 'Options: t(rue) or f(alse)'
    ; 'Standard: t,t,t,t,f,f,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,f,t,t,t,t,t,f,f'
    ; 'Enter 0 to use standard or 1 to enter your own'
    spawn, 'echo 0 >> inp_file'
    spawn, '/davit/lib/vt/rt/IRI/iri < inp_file'
    
    
    openr, unit, 'iri_nel.dat', /get_lun
    readf, unit, yyyy, mmdd, hr, format='(2I4,F5.2)'
    readf, unit, tnel, format='(500E19.11)'
    free_lun, unit
    
    ; shift so that longitude go -180E, 180E
    nel[nlat,0:nlons/2L] = (tnel[nlons/2L:nlons])
    nel[nlat,nlons/2L:nlons] = (tnel[0:nlons/2L])
    
    slat = lat[nlat]-res/2.
    blat = lat[nlat]+res/2.
    for nlon=1,nlons-1 do begin
	slon = lon[nlon]-res/2.
	blon = lon[nlon]+res/2.
	
	col = bytscl(nel[nlat,nlon], min=nelmin, max=nelmax, top=252) + 1
	
	; finally plot the point
	POLYFILL,[slon,slon,blon,blon], [slat,blat,blat,slat], $
		COL=col,NOCLIP=0
    endfor
    xlat = xlat + res
endfor
MAP_CONTINENTS, /coast, /NOERASE
MAP_GRID, charsize=charsize, color=254., $
	letdel=30., londel=60., /label
; "over"plot axis
; plot, [0,0], /nodata, position=position, $
; 	charthick=charthick, charsize=charsize, $
; 	yrange=yrange, xrange=xrange, $
; 	xstyle=1, ystyle=ystyle, $
; 	color=get_foreground(), title=title
	

plot_colorbar, /vert, charthick=2, /bar, /continuous, /no_rotate, $
	scale=[nelmin,nelmax], position=bpos, charsize=charsize, $
	legend='Electron Density [m!E-3!N]', $
	level_format='(E5.0)', nlevels=1

; openw, unit, 'iri_nel_map.dat', /get_lun
; writeu, unit, date, time, alt, nel
; free_lun, unit



end