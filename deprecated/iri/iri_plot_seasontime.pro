pro	iri_plot_seasontime, year, res=res, alt=alt

if ~keyword_set(alt) then $
    alt = 300.
    
    
time0 = [0,2400]
idate = year*10000L + [0101,1231]
sfjul, idate, time0, sjul, fjul, no_d=ndays

hrbeg = 0.
hrend = 24.
hrstp = 0.5
nhrs = FIX(abs(hrbeg-hrend)/hrstp)
nel = dblarr(ndays,nhrs+1)
tnel = dblarr(500)
day = sjul + dindgen(ndays)*res
; hrs = findgen(nhrs)*hrstp
hrs = sjul + dindgen(nhrs)*hrstp/24.d

nelmin = 1e10
nelmax = 6e11
scale = [nelmin,nelmax]

xmaps = 1
ymaps = 1
xmap = 0
ymap = 0
xrange = [sjul,fjul]
yrange = [hrs[0],hrs[nhrs-1]]
position = define_panel(xmaps, ymaps, xmap, ymap, /bar, with_info=with_info)
bpos = define_cb_position(position, /vertical, gap=0.01, width=0.02)
charsize = get_charsize(xmaps, ymaps)
; charsize=.5
title = 'IRI - 300 km - '+STRTRIM(year,2)
clear_page
plot, [0,0], /nodata, xstyle=5, ystyle=5, $
    yrange=yrange, xrange=xrange, position=position

for nd=0,ndays-1 do begin
    date = format_juldate(day[nd],/short_date)
    spawn, 'rm inp_file'
    
    ; 'jmag(=0/1,geog/geom),lati/deg,long/deg'
    spawn, 'echo 0,40,288 >> inp_file'
    ; 'year(yyyy),mmdd(or -ddd),iut(=0/1,LT/UT),hour'
    rdate = STRMID(date,0,4)+','+STRMID(date,4,2)+STRMID(date,6,2)+',0,0'
    print, rdate
    spawn, 'echo '+rdate+' >> inp_file'
    ; 'height/km'
    ; '(enter  0 for list of peak heights and densities
    spawn, 'echo '+STRTRIM(alt,2)+' >> inp_file'
    ; 'upper height [km] for TEC integration (0 for no TEC)'
    spawn, 'echo 600 >> inp_file'
    ; 'variable? (1/2/../8 for height/lat/long/year/month/day/day of year/hour)
    spawn, 'echo 8 >> inp_file'
    ; 'begin, end, and stepsize for the selected variable'
    spawn, 'echo '+STRTRIM(hrbeg,2)+','+STRTRIM(hrend,2)+','+STRTRIM(hrstp,2)+' >> inp_file'
    ; 'Options: t(rue) or f(alse)'
    ; 'Standard: t,t,t,t,f,f,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,f,t,t,t,t,t,f,f'
    ; 'Enter 0 to use standard or 1 to enter your own'
    spawn, 'echo 0 >> inp_file'
    spawn, '/davit/lib/vt/rt/IRI/iri < inp_file'
    
    
    openr, unit, 'iri_nel.dat', /get_lun
    readf, unit, yyyy, mmdd, hr, format='(2I4,F5.2)'
    readf, unit, tnel, format='(500E19.11)'
    free_lun, unit
    
    nel[nd,*] = tnel[0:nhrs]
    
    sday = day[nd]
    bday = day[nd]+res
    for nhr=0,nhrs-1 do begin
	shr = hrs[nhr]
	bhr = hrs[nhr]+hrstp
	
	col = bytscl(nel[nd,nhr], min=nelmin, max=nelmax, top=252) + 1
	
	; finally plot the point
	POLYFILL,[sday,bday,bday,sday],[shr,shr,bhr,bhr], $
		COL=col,NOCLIP=0
    endfor
endfor
plot_colorbar, /vert, charthick=2, /bar, /continuous, /no_rotate, $
	scale=[nelmin,nelmax], position=bpos, charsize=charsize, $
	legend='Electron Density [m!E-3!N]', $
	level_format='(E5.0)', nlevels=1
; "over"plot axis
dum = label_date(date_format='%M')
plot, [0,0], /nodata, position=position, $
	charthick=charthick, charsize=charsize, $
	xrange=xrange, yticks=' ', $
	xtickformat='label_date', $
	xstyle=1, ystyle=4, xtickunit='Months',  $
	xtitle='Month', $
	color=get_foreground(), title=title
dum = label_date(date_format='%H:%I')
plot, [0,0], /nodata, position=position, $
	charthick=charthick, charsize=charsize, $
	yrange=yrange, xticks=' ', $
	ytickformat='label_date',xtickformat='label_date', $
	xstyle=4, ystyle=1, ytickunit='Hours', $
	ytitle='Hour LT', $
	color=get_foreground(), title=title

end