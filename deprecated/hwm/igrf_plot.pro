pro	igrf_plot, date, pdip=pdip, pdec=pdec, pcomb=pcomb

cd, '~/Documents/Research/HWM/HWM07'

parse_date, date, year, month, day
dayno = day_no(date)
ryear = year + dayno/365.

res = 2.
nlats = 0L
nlons = 0L
nlats = 180L/res
nlons = 360L/res
lat = -90. + findgen(nlats+1)*res
lon = -180. + findgen(nlons+1)*res

dip = fltarr(nlons+1,nlats+1)
dec = fltarr(nlons+1,nlats+1)

clear_page
xmaps = 1
ymaps = 1
xmap = 0
ymap = 0
charsize = get_charsize(xmaps, ymaps)
; charsize=.5
yrange = [-90.,90.]
xrange = [-180.,180.]
position = define_panel(xmaps, ymaps, xmap, ymap, /bar, with_info=with_info)
bpos = define_cb_position(position, /vertical, gap=0.01, width=0.02)
title = 'IGRF'
MAP_SET, /cylindrical, title=title, /noerase, charsize=charsize, $
	/continents, /hires, /grid, position=position, $
	limit=[-90.,-180.,90.,180.]

; Execute hwm code
input = STRTRIM(ryear,2)
print, input
spawn, 'rm inp_file'
spawn, 'echo '+input+' >> inp_file'
spawn, '/home/sebastien/Documents/Research/HWM/HWM07/igrfOUT < inp_file'


openr, unit, 'igrfOUT.dat', /get_lun
readf, unit, dip, format='(181F8.3)'
readf, unit, dec, format='(181F8.3)'
free_lun, unit

dip = shift(dip,90,0)
dec = shift(dec,90,0)

comb = dip*0.
pinds = where(dip ge 0.)
comb[pinds] = (cos(dec[pinds]*!PI/180.) - sin(dec[pinds]*!PI/180.))* $
		cos(dip[pinds]*!PI/180.)*sin(dip[pinds]*!PI/180.)
ninds = where(dip le 0.)
comb[ninds] = (cos(dec[ninds]*!PI/180.) + sin(dec[ninds]*!PI/180.))* $
		cos(-dip[ninds]*!PI/180.)*sin(-dip[ninds]*!PI/180.)
		
if keyword_set(pcomb) then begin
    legend = 'Wind efficiency'
    scale = [-.6,.6]
    for nlat=1,nlats-1 do begin
		slat = lat[nlat]-res/2.
		blat = lat[nlat]+res/2.
		for nlon=1,nlons-1 do begin
			slong = lon[nlon]-res/2.
			blong = lon[nlon]+res/2.
			
	; 	    col = bytscl(dec[nlon,nlat], min=scale[0], max=scale[1], top=252) + 1
			col = get_color_index(comb[nlon,nlat], /rotate,scale=scale,colorsteps=250.)
			
			; finally plot the point
			POLYFILL,[slong,slong,blong,blong], [slat,blat,blat,slat], $
				COL=col,NOCLIP=0
		endfor
    endfor
endif

if keyword_set(pdec) then begin
    scale = [-30.,30.]
    legend = 'Magnetic field declination [degrees]'
    for nlat=1,nlats-1 do begin
		slat = lat[nlat]-res/2.
		blat = lat[nlat]+res/2.
		for nlon=1,nlons-1 do begin
			slong = lon[nlon]-res/2.
			blong = lon[nlon]+res/2.
			
	; 	    col = bytscl(dec[nlon,nlat], min=scale[0], max=scale[1], top=252) + 1
			col = get_color_index(dec[nlon,nlat], /rotate,scale=scale,colorsteps=250.)
			
			; finally plot the point
			POLYFILL,[slong,slong,blong,blong], [slat,blat,blat,slat], $
				COL=col,NOCLIP=0
		endfor
    endfor
endif

if keyword_set(pdip) then begin
    dip_label = -90.+findgen(19)*10.
;     contour, dip, lon, lat, /xstyle, /ystyle, $
; 	    /overplot, /color, /fill, /closed, $
; 	    levels=dip_label
    contour, dip, lon, lat, $
	    c_charsize=.5, $
	    /overplot, c_labels=dip_label, $
	    levels=dip_label, c_thick=(dip_label eq 0.)*2.
endif

MAP_CONTINENTS, /coast, /NOERASE
MAP_GRID, charsize=charsize, color=254., $
	letdel=30., londel=60., /label

plot_colorbar, /vert, charthick=2, /bar, /continuous, $
	scale=scale, position=bpos, charsize=charsize, $
	legend=legend, $
	level_format='(f4.1)', nlevels=6

; openw, unit, 'igrf_idl.dat', /get_lun
; writeu, unit, nlats, nlons
; writeu, unit, comb, dip, dec
; free_lun, unit

end