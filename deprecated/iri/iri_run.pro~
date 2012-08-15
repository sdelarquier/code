pro iri_run, date, time, ps=ps, alti=alti

base = '~/Documents/Research/NRL/'

parse_date, date, year, month, day
parse_time, time, hour, minutes

dhour = hour + minutes/60.
if ~keyword_set(alti) then $
	alti = 300.
; Longitude
lonbeg = -180.
lonend = 180.
lonstp = 2.
nlons = fix((lonend - lonbeg)/lonstp)
; Latitude
latbeg = -70.
latend = 70.
latstp = 2.
nlats = fix((latend-latbeg)/latstp)

hmf2 = fltarr(nlats+1, nlons+1)
nmf2 = fltarr(nlats+1, nlons+1)
nel = fltarr(nlats+1, nlons+1)
lats = latbeg + findgen(nlats+1)*latstp
lons = lonbeg + findgen(nlons+1)*lonstp

; lati,longi,alti,iyyyy,mmdd,iut,dhour,ivar,vbeg,vend,vstp
for ilat=0,nlats do begin
	spawn, 'rm inp_file'
	; lati,longi,alti
	rpos = STRTRIM(lats[ilat],2)+',0.0,'+STRTRIM(alti,2)
	; iyyyy,mmdd,iut,dhour
	rdate = STRTRIM(year,2)+','+STRTRIM(month*100L+day,2)+',0,'+STRTRIM(dhour,2)
	; 'begin, end, and stepsize for the selected variable'
	rvar = '3,'+STRTRIM(lonbeg,2)+','+STRTRIM(lonend,2)+','+STRTRIM(lonstp,2)
	spawn, 'echo '+rpos+','+rdate+','+rvar+' >> inp_file'
	spawn, '/davit/lib/vt/fort/iri/iri < inp_file'

	iri_read, nmf2=tnmf2, hmf2=thmf2, nel=tnel

	hmf2[ilat,*] = thmf2
	nmf2[ilat,*] = tnmf2
	nel[ilat,*] = tnel
endfor

if keyword_set(ps) then $
	ps_open, base+'IRI_'+STRTRIM(date,2)+'_'+STRTRIM(time,2)+'.ps', /no_init

ymaps = 2

legend = 'hmF!I2!N [km]'
scale = [fix(min(hmf2)/10L)*10.,fix(max(hmf2)/10L)*10.]
set_format, /portrait, /sardines, /tokyo
clear_page
position = define_panel(1, ymaps, 0, 0, /bar, /no_title)
bpos = define_cb_position(position, /vertical, gap=0.01, width=0.02)
charsize = get_charsize(1,ymaps)
title = 'IRI - '+STRTRIM(month,2)+'/'+STRTRIM(day,2)+'/'+STRTRIM(year,2)+', ' $
	+STRTRIM(string(hour,format='(I02)'),2)+':'+STRTRIM(string(minutes,format='(I02)'),2)+'LT'
MAP_SET, /cylindrical, title=title, /noerase, charsize=charsize, $
	/continents, /hires, /grid, position=position, $
	limit=[-90.,-180.,90.,180.]

for ilat=0,nlats do begin
    slat = lats[ilat] - latstp/2.
    blat = lats[ilat] + latstp/2.
	for ilon=0,nlons do begin
		slon = lons[ilon] - lonstp/2.
		blon = lons[ilon] + lonstp/2.
		
		col = bytscl(hmf2[ilat,ilon], min=scale[0], max=scale[1], top=252) + 2
		
		; finally plot the point
		POLYFILL,[slon,slon,blon,blon], [slat,blat,blat,slat], $
			COL=col,NOCLIP=0
	endfor
endfor

MAP_CONTINENTS, /coast, /NOERASE
MAP_GRID, charsize=charsize, color=255., $
	letdel=30., londel=60., /label
plot_colorbar, /vert, charthick=2, /bar, /continuous, /no_rotate, $
	scale=scale, position=bpos, charsize=charsize, $
	legend=legend, level_format='(F5.1)', nlevels=5


openw, unit, base+'hmf2_'+STRTRIM(date,2)+'_'+STRTRIM(time,2)+'.dat', /get_lun
ncols = 10L
for ilon=0,nlons/10L do begin
	if (ilon+1)*ncols gt nlons then $
		ncols = fix(nlons - ilon*ncols + 1)
	formatLon = '(A10,'+STRTRIM(ncols,2)+'F10.2)'
	formatNL = '(F10.2,'+STRTRIM(ncols,2)+'F10.2)'
	printf, unit, ' ', lons[ilon*10L:(ilon*10L+ncols-1)], format=formatLon
	for ilat=0,nlats do begin
		printf, unit, lats[ilat], hmf2[ilat,ilon*10L:(ilon*10L+ncols-1)], format=formatNL
	endfor
	printf, unit, ' '
endfor
free_lun, unit




legend = 'NmF!I2!N [m!E-3!N]'
scale = [floor(min(alog10(nmf2))*10L)/10.,ceil(max(alog10(nmf2))*10L)/10.]
position = define_panel(1, ymaps, 0, 1, /bar, /no_title)
bpos = define_cb_position(position, /vertical, gap=0.01, width=0.02)
MAP_SET, /cylindrical, /noerase, charsize=charsize, $
	/continents, /hires, /grid, position=position, $
	limit=[-90.,-180.,90.,180.]

for ilat=0,nlats do begin
    slat = lats[ilat] - latstp/2.
    blat = lats[ilat] + latstp/2.
	for ilon=0,nlons do begin
		slon = lons[ilon] - lonstp/2.
		blon = lons[ilon] + lonstp/2.
		
		col = bytscl(alog10(nmf2[ilat,ilon]), min=scale[0], max=scale[1], top=252) + 2
		
		; finally plot the point
		POLYFILL,[slon,slon,blon,blon], [slat,blat,blat,slat], $
			COL=col,NOCLIP=0
	endfor
endfor

MAP_CONTINENTS, /coast, /NOERASE
MAP_GRID, charsize=charsize, color=255., $
	letdel=30., londel=60., /label
plot_colorbar, /vert, charthick=2, /bar, /continuous, /no_rotate, $
	scale=scale, position=bpos, charsize=charsize, $
	legend=legend, level_format='(F5.2)', nlevels=4

; legend = 'Nel [m!E-3!N]'
; scale = [floor(min(alog10(nel))*10L)/10.,ceil(max(alog10(nel))*10L)/10.]
; print, max(nel)
; position = define_panel(1, ymaps, 0, 2, /bar, /no_title)
; bpos = define_cb_position(position, /vertical, gap=0.01, width=0.02)
; MAP_SET, /cylindrical, /noerase, charsize=charsize, $
; 	/continents, /hires, /grid, position=position, $
; 	limit=[-90.,-180.,90.,180.]
; 
; for ilat=0,nlats do begin
;     slat = lats[ilat] - latstp/2.
;     blat = lats[ilat] + latstp/2.
; 	for ilon=0,nlons do begin
; 		slon = lons[ilon] - lonstp/2.
; 		blon = lons[ilon] + lonstp/2.
; 		
; 		col = bytscl(alog10(nel[ilat,ilon]), min=scale[0], max=scale[1], top=252) + 2
; 		
; 		; finally plot the point
; 		POLYFILL,[slon,slon,blon,blon], [slat,blat,blat,slat], $
; 			COL=col,NOCLIP=0
; 	endfor
; endfor
; 
; MAP_CONTINENTS, /coast, /NOERASE
; MAP_GRID, charsize=charsize, color=255., $
; 	letdel=30., londel=60., /label
; plot_colorbar, /vert, charthick=2, /bar, /continuous, /no_rotate, $
; 	scale=scale, position=bpos, charsize=charsize, $
; 	legend=legend, level_format='(F5.2)', nlevels=4

if keyword_set(ps) then $
	ps_close, /no_init

openw, unit, base+'nmf2_'+STRTRIM(date,2)+'_'+STRTRIM(time,2)+'.dat', /get_lun
ncols = 10L
for ilon=0,nlons/10L do begin
	if (ilon+1)*ncols gt nlons then $
		ncols = fix(nlons - ilon*ncols + 1)
	formatLon = '(A10,'+STRTRIM(ncols,2)+'F10.2)'
	formatNL = '(F10.2,'+STRTRIM(ncols,2)+'E10.2)'
	printf, unit, ' ', lons[ilon*10L:(ilon*10L+ncols-1)], format=formatLon
	for ilat=0,nlats do begin
		printf, unit, lats[ilat], nmf2[ilat,ilon*10L:(ilon*10L+ncols-1)], format=formatNL
	endfor
	printf, unit, ' '
endfor
free_lun, unit


end