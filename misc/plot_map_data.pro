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
nlons = sizata[1]
nlats = sizata[2]
if ~keyword_set(lons) then $
    lons = -180. + findgen(nlons)*360./nlons
if ~keyword_set(lats) then $
    lats = -90. + findgen(nlats)*180./nlats

; XYranges
limit = [lats[0],lons[0], lats[nlats-1],lons[nlons-1]]
reslons = 360./(nlons-1)
reslats = 180./(nlats-1)
print, limit

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


MAP_SET,0,lons[nlons/2],0, /cylindrical, title=title, /noerase, charsize=charsize, $
	position=position, $
	limit=limit

for nlat=1,nlats-2 do begin
    slat = lats[nlat]-reslats/2.
    blat = lats[nlat]+reslats/2.
    for nlon=1,nlons-2 do begin
	slong = lons[nlon]-reslons/2.
	blong = lons[nlon]+reslons/2.
; 	    col = bytscl(dec[nlon,nlat], min=scale[0], max=scale[1], top=252) + 1
	col = get_color_index(data[nlon,nlat], param='power',scale=scale,colorsteps=255.)
	
	; finally plot the point
	POLYFILL,[slong,slong,blong,blong], [slat,blat,blat,slat], $
		COL=col,NOCLIP=0
    endfor
endfor
MAP_CONTINENTS, /coast, /NOERASE, color=255
MAP_GRID, charsize=charsize, color=255., $
	letdel=30., londel=60., /label


end