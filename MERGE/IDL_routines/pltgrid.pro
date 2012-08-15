PRO sdgrdplt, flnm, coord, cntrs

common pair_data, pair_info

	if (KEYWORD_SET(flnm)) then file=flnm else file='ftn08'

!P.FONT = 0
new_vel_col
get_pair_info

;READING THE GRID DATA FILE
;--------------------------
get_lun,u
openr, u, file
recl1    =  0L
recl2    =  0L
stat     =  intarr(2)
height 	 =  0.
latg     =  fltarr(256)
long 	 =  fltarr(256)
latm	 =  fltarr(256)
lonm	 =  fltarr(256)
readu, u, recl1,stat,height,recl2
readu, u, recl1,latg,long,latm,lonm,recl2
st1 = stat(0)
st2 = stat(1)

if (KEYWORD_SET(coord)) then begin
  lat = latm
  lon = lonm
endif else begin
   lat = latg
   lon = long
endelse


;SETTING THE PLOT LIMITS
;-----------------------
pair = 0
if (st1 eq  7) and (st2 eq  6) then pair = 1
if (st1 eq  5) and (st2 eq  3) then pair = 2
if (st1 eq  1) and (st2 eq  8) then pair = 3
if (st1 eq  9) and (st2 eq 10) then pair = 4
if (st1 eq  2) and (st2 eq  8) then pair = 5
if (st1 eq  2) and (st2 eq  1) then pair = 6
if (st1 eq  4) and (st2 eq 11) then pair = 7
if (st1 eq  4) and (st2 eq 12) then pair = 8
if (st1 eq 11) and (st2 eq 12) then pair = 9

if (pair eq 0) then begin		;Not a nominal pair
	print,' Not a Nominal Pair'
	goto, endloop
endif

radars = pair_info(pair).name
misc = fltarr(3)
if (KEYWORD_SET(coord)) then lims = pair_info(pair).maglim $
else lims = pair_info(pair).geolim & misc = pair_info(pair).misc

ymin = lims(0)
ylim = lims(1)
xmin = lims(2)
xlim = lims(3)
lambx = misc(0)
lamby = misc(1)

resp = ''
print, ' '
print, 'The extreme limits of the map are (latmin,latmax,lonmin,lonmax):'
print, ymin, ylim, xmin, xlim
read, 'Do you want to change them (y/n) ?... ', resp
if (resp eq 'y') or (resp eq 'Y') then $
read, 'Enter latmin, latmax, lonmin, lonmax... ',ymin, ylim, xmin, xlim


;GETTING THE L-SHELL CONTOURS
;----------------------------
if (not KEYWORD_SET(coord)) then begin
  lshlat=fltarr(4,360)
  lshlon=fltarr(4,360)
  get_lun,u1
  openr,u1,getenv('SD_TABLES')+'/lsh_geog.dat'
  readf,u1,lshlon
  readf,u1,lshlat
  close,u1
  free_lun,u1
endif

ncol = (!d.n_colors < 256) -1 ;Colors we can use

;SETTING THE MAP
;---------------
	if !d.name eq 'PS' then begin
		tcol = 1
	endif else tcol = 10 ;ncol-1

	dlat = 5.
	dlon = 15.

	pollat = (ymin+ylim)/2.

	pollon = (xmin+xlim)/2.

	map_set,pollat,pollon,limit=[ymin,xmin,ylim,xlim],/GRID,$
		glinethick=0.5,/stereo,color=tcol,$
		latdel=dlat,londel=dlon,/label,latlab=xmin+.25*dlon,$
		lonlab=llab

	if (KEYWORD_SET(cntrs)) then $
	map_continents,color=tcol

	S = findgen(16)*(!PI*2/16.)
	usersym,cos(S),sin(S),/FILL


	delt_lat = abs(ylim-ymin)

	delt_lon = abs(xlim-xmin)


;PLOT THE GRID POINTS
;--------------------
	for i = 0,255 do begin
	  if (lon(i) gt xmin) and (lon(i) lt xlim) and (lat(i) gt ymin) $
	      	  	       and (lat(i) lt ylim) then begin
	     plots,lon(i),lat(i),psym=8,symsize=0.4,color=tcol
	  endif
	endfor


;PLOTS THE L-SHELL CONTOURS
;--------------------------
	if(not KEYWORD_SET(coord)) then begin
	for i=0,358 do begin
	   for j=0,3 do begin
	      if lshlat(j,i) gt ymin then if lshlat(j,i) lt ylim then $
		if lshlon(j,i) gt xmin then if lshlon(j,i) lt xlim then begin

		plots,[lshlon(j,i),lshlon(j,i+1)]$
			,[lshlat(j,i),lshlat(j,i+1)],color=tcol

	      endif	
	   endfor
	endfor

	!p.font=-1

	lambda= '!7K=70!9%'
 	xyouts,lambx,lamby,lambda,color=tcol,/data
	endif

;WRITES COMMENTS ON MAPS
;-----------------------
	!p.font=0

	thetitle='SUPERDARN GRID'

	xyouts, 0.02, 0.965, thetitle, /normal, color=tcol
	xyouts, 0.98, 0.965, radars, /normal, alignment=1.0,color=tcol


  	EMPTY
endloop:
free_lun, u

END ; sdgrdplt

;*****************************************************************************************

PRO PLTGRID

file = ''
flnm = ''
coord = ''
cntrs = ''

print, ' '
print, 'Enter GRID data file to be plotted'
print, 'It should have been created first by the grid.exe'
print, 'program in the /misc directory. The present program'
print, 'reads the file from the misc/Grid directory'
print, ' '
read, 'Enter File... ', file
flnm = (getenv('SD_MISC') + '/Grid/' + file)

print, ' '
print, 'Do you want the plot to be in geomagnetic coordinates ?'
read, 'The default is in geographic coordinates (y/n)... ', coord
if (coord eq 'n') or (coord eq 'N') then coord = ''

if(KEYWORD_SET(coord)) then goto, endcnt
  print, ' '
  read, 'Do you want the continent contours (y/n) ?... ', cntrs
  if (cntrs eq 'n') or (cntrs eq 'N') then cntrs = ''
endcnt:

sdgrdplt, flnm, coord, cntrs

end

