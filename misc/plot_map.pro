; procedure to plot data on a map
; 
; DATA: 2D array with 1st dim as longitudes data points
; and 2nd dim as latitudes data points, all linearly spaced
;
; LATS: latitudes vector. Must be same size as DATA 2nd dim.
;
; LONS: longitudes vector. Must be the same size as DATA 1st dim.
;
; SCALE: upper and lower limit of the data for color scale
;
pro	plot_map_data, data, scale, lats=lats, lons=lons, $
	charsize=charsize, title=title, $
	position=position, legend=legend

sizata = size(data)
nlons = sizeata[1]
nlats = sizeata[2]
if ~keyword_set(lons) then $
    lons = -180. + findgen(nlons+1)*360./nlons
if ~keyword_set(lats) then $
    lats = -90. + findgen(nlats+1)*180./nlats

; XYranges
xrange = [lons[0], lons[nlons]]
yrange = [lats[0], lats[nlats]]
reslons = 360./nlons
reslats = 180./nlats

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


MAP_SET, /cylindrical, title=title, /noerase, charsize=charsize, $
	/continents, /hires, /grid, position=position, $
	limit=[-90.,-180.,90.,180.]
for nlat=1,nla-1 do begin
    slat = lat[nlat]-reslats/2.
    blat = lat[nlat]+reslats/2.
    for nlon=1,nlo-1 do begin
	slong = lon[nlon]-reslons/2.
	blong = lon[nlon]+reslons/2.
	
; 	    col = bytscl(dec[nlon,nlat], min=scale[0], max=scale[1], top=252) + 1
	col = get_color_index(data[nlon,nlat], /rotate,scale=scale,colorsteps=255.)
	
	; finally plot the point
	POLYFILL,[slong,slong,blong,blong], [slat,blat,blat,slat], $
		COL=col,NOCLIP=0
    endfor
endfor
MAP_CONTINENTS, /coast, /NOERASE
MAP_GRID, charsize=charsize, color=254., $
	letdel=30., londel=60., /label
plot_colorbar, /vert, charthick=2, /bar, /continuous, $
	scale=scale, position=bpos, charsize=charsize, $
	legend=legend, nlevels=4


end