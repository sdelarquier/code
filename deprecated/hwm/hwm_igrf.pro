pro	hwm_igrf, xmaps, ymaps, xmap, ymap, date, time, alt=alt, res=res


if ~keyword_set(alt) then $
    alt = 150.
if ~keyword_set(res) then $
    res = 2.

; ***************************************************************************************************************
; WINDS
; ***************************************************************************************************************
sfjul, date, [0,0], sjul, fjul, no_hours=nhrs
juls = sjul + dindgen(nhrs)/24.d

parse_date, date, year, month, day
iyd = year*1000L + day_no(date)

parse_time, time, hour, minutes
hour = hour + minutes/60.
sec = hour*3600.

slon = 0.
flon = 360.
lonstp = res

nlats = 180./res
nlons = 360./res
lat = -90. + findgen(nlats+1)*res
lon = -180. + findgen(nlons+1)*res

wm = fltarr(nlons+1,nlats+1)
wz = fltarr(nlons+1,nlats+1)

; clear_page
; xmaps = 1
; ymaps = 1
; xmap = 0
; ymap = 0
; charsize = get_charsize(xmaps, ymaps)
charsize=.5
yrange = [-90.,90.]
xrange = [-180.,180.]

; Execute hwm code
glat = 90.
for nlat=1,nlats-1 do begin
    input = STRTRIM(iyd,2)+','+STRTRIM(sec,2)+','+STRTRIM(alt,2)+','+STRTRIM(glat,2)+','+STRTRIM(0.,2)+','+ $
	STRTRIM(slon,2)+','+STRTRIM(flon,2)+','+STRTRIM(lonstp,2)+',2'
    print, input
    spawn, 'rm inp_file'
    spawn, 'echo '+input+' >> inp_file'
    spawn, '/home/sebastien/Documents/Research/HWM/HWM07/hwmOUT < inp_file'


    twm = fltarr(500)
    twz = fltarr(500)
    openr, unit, 'hwm.dat', /get_lun
    readf, unit, twm, format='(500F8.3)'
    readf, unit, twz, format='(500F8.3)'
    free_lun, unit

    wm[*,nlat] = twm[0:nlons]
    wz[*,nlat] = twz[0:nlons]
    
    glat -= res
endfor
wm = shift(wm,nlons/2,0)
wz = shift(wz,nlons/2,0)
; ***************************************************************************************************************
; ***************************************************************************************************************

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

; clear_page
; xmaps = 1
; ymaps = 1
; xmap = 0
; ymap = 0
; charsize = get_charsize(xmaps, ymaps)
; charsize=.5
yrange = [-90.,90.]
xrange = [-180.,180.]
position = define_panel(xmaps, ymaps, xmap, ymap, /bar, with_info=with_info)
bpos = define_cb_position(position, /vertical, gap=0.01, width=0.01)
title = 'IGRF + HWM07!C'+STRMID(STRTRIM(alt,2),0,3)+'km - '+format_date(date,/human)+' - '+STRMID(STRTRIM(hour,2),0,2)+'LT'
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

dip_label = -90.+findgen(19)*10.
;     contour, dip, lon, lat, /xstyle, /ystyle, $
; 	    /overplot, /color, /fill, /closed, $
; 	    levels=dip_label
contour, dip, lon, lat, $
	c_charsize=.5, $
	/overplot, c_labels=dip_label, $
	levels=dip_label, c_thick=(dip_label eq 0.)*2.

wstp = 6
velovect,wz[0:*:wstp,0:*:wstp],wm[0:*:wstp,0:*:wstp],lon[0:*:wstp],lat[0:*:wstp], /overplot
print, lon[0:*:wstp]
print, lat[0:*:wstp]
print, wz[0:*:wstp,0:*:wstp]

MAP_CONTINENTS, /coast, /NOERASE, color=255
MAP_GRID, charsize=charsize, color=254., $
	letdel=30., londel=60., /label

plot_colorbar, /vert, charthick=2, /bar, /continuous, $
	scale=scale, position=bpos, charsize=charsize, $
	legend=legend, nlevels=6


end