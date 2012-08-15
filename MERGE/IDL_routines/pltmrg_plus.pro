
;=======================================================================

PRO READ_MERGE, mrg_unit, nvects,yr,mo,dy,shr,smin,ssec,ehr,emin,esec, $
	  		   niter, map_qlty, height_r, data


recl1    =  0L
recl2    =  0L
sdate    =  intarr(2,6)
edate    =  intarr(2,6)
mhz	 =  intarr(2)
khz	 =  intarr(2)
lagfr    =  intarr(2)
smsep    =  intarr(2)
map_qlty =  0L
ngood    =  0
niter    =  0L
height_r =  0.


  readu, mrg_unit, recl1,sdate,edate,mhz,khz,lagfr,smsep,map_qlty,ngood, $
		   niter,height_r,recl2
  nvects = ngood

  yr=     sdate(0,0)
  mo=     sdate(0,1)
  dy=     sdate(0,2)

  start_time = cnv_mdhms_sec(sdate)
  stop_time  = cnv_mdhms_sec(edate)

  time_min = min(start_time)
  time_max = max(stop_time)

  if (start_time(0) eq time_min) then begin
     shr  = sdate(0,3)
     smin = sdate(0,4)
     ssec = sdate(0,5)
  endif else begin
     shr  = sdate(1,3)
     smin = sdate(1,4)
     ssec = sdate(1,5)
  endelse

  if (stop_time(0) eq time_max) then begin
     ehr  = edate(0,3)
     emin = edate(0,4)
     esec = edate(0,5)
  endif else begin
     ehr  = edate(1,3)
     emin = edate(1,4)
     esec = edate(1,5)
  endelse


  if (ngood eq 0) then goto, jump
	
  ib      =  0L
  jb      =  0L
  ncnt    =  intarr(16,16)
  lat_g   =  fltarr(16,16)
  lon_g   =  fltarr(16,16)
  vx      =  fltarr(16,16)
  vy      =  fltarr(16,16)
  vz      =  fltarr(16,16)

  g1      =  0

  for n = 0,ngood-1 do begin

    readu, mrg_unit, recl1,ib,jb,a1,a2,b1,b2,c1,c2,c3,h1,h2,d1,d2,d3, $
                                              e1,e2,e3,f1,f2,f3,g1,recl2


    ncnt(ib,jb)  =   1
    lat_g(ib,jb) =  a1
    lon_g(ib,jb) =  a2
    vx(ib,jb)    =  d1
    vy(ib,jb)    =  d2
    vz(ib,jb)    =  d3

  endfor

  cnt    =  ncnt
  lat    =  lat_g
  lon    =  lon_g  


  data= FLTARR( 4, ngood)
  igood= 0

  for ib = 0,15 do begin
    for jb = 0,15 do begin

      if (cnt(ib,jb) gt 0) then begin

        vvx        =  vx(ib,jb)
        vvy        =  vy(ib,jb)
	vvz 	   =  vz(ib,jb)
        lon_f       =  lon(ib,jb)
        lat_f       =  lat(ib,jb)

	Vmag = sqrt(vvx^2+vvy^2+vvz^2)
	azim = atan(vvy,-vvx)*!radeg
	
        data( 0, igood)= lat_f
        data( 1, igood)= lon_f
        data( 2, igood)= Vmag
        data( 3, igood)= azim
        igood= igood + 1

      endif

    endfor
  endfor

;endwhile

jump:

return
END ; read_merge


;=======================================================================
;
	PRO PRINT_OPTIONS, up, file, vers, infi, infr
;
; Print the MERGE processing options
;
;****************************************************************************

f_low1 	= infi(14)
f_low2 	= infi(15)
f_high1 = infi(16)
f_high2 = infi(17)
idur 	= infi(18)
ialt 	= infi(19)
igs 	= infi(20)
iereg	= infi(21)
ifltr	= infi(22)
inter	= infi(23)
idiv	= infi(24)
iext	= infi(25)
ires	= infi(26)
istnd	= infi(27)
tperiod    = infr(0)
height     = infr(1)
frang_min  = infr(3)
pwr_min    = infr(4)
vel_max    = infr(5)
velerr_max = infr(6)
vel_min    = infr(7)
tstep	   = infr(8)

printf, up, ' '
printf, up, ' '
printf, up, 'FICHIER: ', file
printf, up, '******** '
printf, up, ' '

printf, up, vers
printf, up, '-------------------'

printf, up, 'Processing Parameters:'

if (idur eq 1) then begin
   pstring = '    Map for Each Scan'
endif
if (idur eq 2) then begin
   pstring = '    Averaged Map, integ. time = ' $
             + strcompress(string(fix(tperiod)),/remove_all) + ' s'
endif
printf, up, pstring
if (idur eq 2) then begin
   pstring = '    Time Step = ' $
             + strcompress(string(fix(tstep)),/remove_all) + ' s'
endif
printf, up, pstring

if (ialt eq 1) then begin
   pstring = '    Constant Virtual Height of ' $
             + strcompress(string(fix(height)),/remove_all) + ' km'
endif
if (ialt eq 2) then begin
   pstring = '    Model for Virtual Height'
endif
if (ialt eq 3) then begin
   pstring = '    Virtual Height from elev. angle'
endif
printf, up, pstring

pstring =  '    ifltr  = '+ strcompress(string(fix(ifltr)),/remove_all)
printf, up, pstring

pstring =  '    inter = '+ strcompress(string(fix(inter)),/remove_all)
printf, up, pstring

pstring =  '    idiv  = '+ strcompress(string(fix(idiv)),/remove_all)
printf, up, pstring

pstring =  '    iext  = '+ strcompress(string(fix(iext)),/remove_all)
printf, up, pstring

pstring =  '    ires  = '+ strcompress(string(fix(ires)),/remove_all)
printf, up, pstring

printf, up, ' '

pstring =  'Data Selection Parameters:'
printf, up, pstring

pstring = '    Frequency Range for Radar 1: ' $
	  + strcompress(string(fix(f_low1 )),/remove_all) + '-' $
	  + strcompress(string(fix(f_high1)),/remove_all) + ' MHz'
printf, up, pstring

pstring = '    Frequency Range for Radar 2: ' $
	  + strcompress(string(fix(f_low2 )),/remove_all) + '-' $
	  + strcompress(string(fix(f_high2)),/remove_all) + ' Mhz'
printf, up, pstring

pstring =  '    Minimum Power = ' $
	   + strcompress(string(fix(pwr_min)),/remove_all) + ' db'
printf, up, pstring

pstring =  '    Minimum Vlos = ' $
	   + strcompress(string(fix(vel_min)),/remove_all) + ' m/s'
printf, up, pstring

pstring =  '    Maximum Vlos = ' $
	   + strcompress(string(fix(vel_max)),/remove_all) + ' m/s'
printf, up, pstring

pstring =  '    Max Error on Vlos = ' $
	   + strcompress(string(fix(velerr_max)),/remove_all) + ' m/s'
printf, up, pstring

pstring =  '    igs   = ' + strcompress(string(fix(igs)),/remove_all)
printf, up, pstring

pstring =  '    iereg = ' + strcompress(string(fix(iereg)),/remove_all)
printf, up, pstring
if (iereg eq 2) then begin
   pstring = '    Minimum range for F region = ' $
             + strcompress(string(fix(frang_min)),/remove_all) + ' km'
   printf, up, pstring
endif

return
end

;*******************************************************************************
;
; This program automatically plots the velocity maps produced by MERGE in the
; standard mode (it seeks the files in the directory where standard results are).
;
; A file named 'idl.ps' is produced, and sent to the printerdefined by
; the environment variable IDL_BWPRINT.
; There are 8 maps on a page, plotted in the magnetic coordinate system.
; The maps for which the quality index is less than 1000 (less than 20 vectors)
; are not plotted.
; The mapping is valid for any kind of projection (flow lines are plotted).
;
;**********************************************************************************

PRO pltmrg_plus, file=flnm

common pair_data, pair_info
get_pair_info

spawn, 'date', proc_day
proc_day = 'Processed on: ' + proc_day

f_path = getenv('SD_MRGOPEN_PATH') + '/' 

if (KEYWORD_SET(flnm)) then file=flnm $
else begin
   print, ' '
   print, 'Existing .mrg files: '
   cmnd = 'ls -1 '+f_path+'*.mrg'
   spawn, cmnd
   file = ' '
   print, ' '
   read,'Which file do you want to print (YYMMDDss) ?... ',file
endelse
file = f_path + file + '.mrg'

scale=1000
lthik = 1.5
!P.FONT = 0

;spawn, 'rm idl.ps'

set_plot, 'PS'
device, font_size=8
device, /inches,xsize=6.,ysize=10.,xoffset=1.25,yoffset=0.75


;READING THE GENERAL HEADER OF THE MERGE DATA FILE
;-------------------------------------------------
get_lun,u
openr, u, file

recl1    =  0L
recl2    =  0L
version  =  '01234567890123456789'
infi     =  intarr(32)
infr     =  fltarr(10)

readu, u, recl1,version,infi,infr,recl2

vers = 'MERGE: version ' + string(strmid(version,11,3))
print, vers

st1 = infi(0)
st2 = infi(1)
istnd = infi(27)


;DEFINING THE PAIR NUMBER AND THE CORRESPONDING RADAR NAMES
;----------------------------------------------------------
pair = 0
if (st1 eq  7) and (st2 eq  6) then pair = 1
if (st1 eq  5) and (st2 eq  3) then pair = 2
if (st1 eq  1) and (st2 eq  8) then pair = 3
if (st1 eq  9) and (st2 eq 10) then pair = 4
if (st1 eq  2) and (st2 eq  8) then pair = 5

if (pair eq 0) then begin		;Not a nominal pair
	print,' Not a Nominal Pair'
	goto, endloop
endif

radars = pair_info(pair).name
thetitle = 'SUPERDARN VELOCITY MAP'

;SETTING THE PLOT LIMITS, GRID AND COLORS
;----------------------------------------

lims = pair_info(pair).maglim
ymin = lims(0)
ylim = lims(1)
xmin = lims(2)
xlim = lims(3)

tcol = 1

dlat = abs(ylim-ymin)/5.
dlon = abs(xlim-xmin)/5.

pollat = (ymin+ylim)/2.
pollon = (xmin+xlim)/2.

S = findgen(16)*(!PI*2/16.)
usersym,cos(S),sin(S),/FILL

Re    =  6362.
dist_ref = 120.
dt = dist_ref/scale


;READING THE DATA
;----------------
!p.multi(0) = 0
!p.multi(1) = 2
!p.multi(2) = 4
nb_to_draw = 8
page = 0

while( NOT EOF(u) ) do begin

	nvects = 0
	map_qlty = 0

	read_merge,u, nvects, yr, mo, dy, shr, smin, ssec, ehr, emin, esec, $
	    niter, map_qlty, height_r, data

;	if (nvects eq 0) or (map_qlty lt 1000) then goto, loop
	if (nvects le 5) then goto, loop
	!p.multi(0) = nb_to_draw

	if (nb_to_draw eq 8) then begin
	   page = page + 1
	   page_nb = strcompress(string(fix(page)),/remove_all)
	   print, 'Plotting page ', page_nb
	endif

	l = (nb_to_draw/2) * 2
	if (l eq nb_to_draw) then colonne = 1 $
	else colonne = 2

	l = nb_to_draw + 1
	m = l/2
	ligne = m


	lats = fltarr(nvects)
	lons = fltarr(nvects)
	Vmag   = fltarr(nvects)
	azim   = fltarr(nvects)

	lats = data(0,0:nvects-1)
	lons = data(1,0:nvects-1)
	Vmag   = data(2,0:nvects-1)
	azim   = data(3,0:nvects-1)


; Calculating the positions (late,lone) of the the points, initially at
; (lats,lons), and moving with the velocity Vmag during the time dt
;----------------------------------------------------------------------	
	cside = (90. - lats)*!dtor
	bside = Vmag*dt/Re	
	angs  = azim*!dtor
	
	arg = cos(bside)*cos(cside) + sin(bside)*sin(cside)*cos(angs)

	q = where( arg lt -1, count )
 		if count ne 0 then arg(q) = -1.0
	q = where( arg gt  1, count )
		if count ne 0 then arg(q) = 1.0

	aside = acos(arg)
	late = 90. - aside*!radeg

	arg = (cos(bside)-cos(aside)*cos(cside))/(sin(aside)*sin(cside))

	q = where( arg lt -1,count )
 		if count ne 0 then arg(q) = -1.0
	q = where( arg gt  1 ,count )
		if count ne 0 then arg(q) = 1.0

	bang = acos(arg)*!radeg

	q = where (angs lt 0,count)
		if count ne 0 then bang(q) = -1.*bang(q)
	lone = lons + bang

	outlats = fltarr(nvects)
	outlons = fltarr(nvects)
	outlate = fltarr(nvects)
	outlone = fltarr(nvects)
	for i = 0,nvects-1 do begin
	     inlat = lats(i)
	     inlon = lons(i)
	     outpos = cnvcoord(inlat,inlon,height_r)
	     outlats(i) = outpos(0)
	     outlons(i) = outpos(1)
	     inlat = late(i)
	     inlon = lone(i)
	     outpos = cnvcoord(inlat,inlon,height_r)
	     outlate(i) = outpos(0)
	     outlone(i) = outpos(1)
	endfor
	lats = outlats
	lons = outlons
	late = outlate
	lone = outlone


;SETTING THE MAP
;---------------

	if (nb_to_draw eq 8) then $
	   map_set,pollat,pollon,limit=[ymin,xmin,ylim,xlim],/GRID,$
		glinethick=0.5,/stereo,color=tcol,$
		latdel=dlat,londel=dlon,/label,latlab=xmin+.25*dlon,$
		lonlab=ymin+dlat $
	else $
	   map_set,pollat,pollon,limit=[ymin,xmin,ylim,xlim],/GRID,$
		glinethick=0.5,/stereo,color=tcol,/noerase,$
		latdel=dlat,londel=dlon,/label,latlab=xmin+.25*dlon,$
		lonlab=ymin+dlat


;Calculating the final position of the arrows whose lengths are proportional to
;Vmag, and which pass through the points (late,lone) calculated before
;------------------------------------------------------------------------------
	tlat = fltarr(nvects)
	tlon = fltarr(nvects)
	for i=0,nvects-1 do begin
	   nx = convert_coord(lons(i),lats(i),/data,/to_device)
	   x0 = nx(0)
	   y0 = nx(1)
	   nx = convert_coord(lone(i),late(i),/data,/to_device)
	   x1 = nx(0)
	   y1 = nx(1)

; If P0 (x0,y0) are the paper coordinates of the origin of the arrow,
;    P1 (x1,y1) are the paper coordinates of the point calculated above,
;    P2 (x2,y2) are the paper coordinates of the end of the arrow,
; we solve for (x2,y2) the two equations:
;	(1)	sqrt[(x2-x0)^2+(y2-y0)^2] = Vmag*0.04*!d.x_size/scale
;	(2)	(x2-x0)/(x1-x0) = (y2-y0)/(y1-y0)
; where (1) expresses that the length of the arrow is proportional to the scaled velocity
; and (2) expresses that P0-P1 and P0-P2 are parallel

	   size = Vmag(i)*0.04*!d.x_size/scale
	   rap = (y1-y0)/(x1-x0)
	   sqdifx = size^2/(1+rap^2)
	   sqdify = rap^2*sqdifx
	   difx = sqrt(sqdifx)
	   dify = sqrt(sqdify)
	   if ((x1-x0) lt 0.) then difx = -1.*difx
	   if ((y1-y0) lt 0.) then dify = -1.*dify
	   x2 = difx + x0
	   y2 = dify + y0

	   nx = convert_coord(x2,y2,/device,/to_data)
	   tlon(i) = nx(0)
	   tlat(i) = nx(1)
	endfor

;PLOT THE VELOCITY ARROWS
;------------------------
	for i = 0,nvects-1 do begin
	  if (lons(i) gt xmin) and (lons(i) lt xlim) and (lats(i) gt ymin) $
	      	  	       and (lats(i) lt ylim) then begin
	     plots,lons(i),lats(i),psym=8,symsize=0.4,color=tcol
	     plots,[lons(i),tlon(i)],[lats(i),tlat(i)], $
		   color=tcol,thick=lthik
	  endif
	endfor


;PLOT THE SCALE (ARROW AND COLOR BAR)
;------------------------------------
	x0 = 0.84
	y0 = 0.87
	x = ( x0 + (colonne-1) ) * 0.5
	y = ( y0 + (ligne-1) ) * 0.25
	plots,x,y,psym=8,symsize=0.4,color=tcol,/normal

	x0  = 0.84
	xx0 = x0 + 0.08
	y0  = 0.87
	x = ( x0 + (colonne-1) ) * 0.5	
	xx = ( xx0 + (colonne-1) ) * 0.5
	y = ( y0 + (ligne-1) ) * 0.25
	plots,[x,xx],[y,y],color=tcol,thick=lthik,/normal

	x0 = 0.82
	y0 = 0.83
	x = ( x0 + (colonne-1) ) * 0.5
	y = ( y0 + (ligne-1) ) * 0.25
	scstring = strtrim(string(scale),2)+'m/s'
	xyouts,x,y,scstring,color=tcol,/normal


;WRITES COMMENTS ON MAPS
;-----------------------

	!p.font = 0
	if (nb_to_draw eq 8) then begin	
	   xyouts, 0.01, 1.0, thetitle, /normal, color=tcol,charsize=1.75
  	   xyouts, 0.99, 1.0, radars, /normal, alignment=1.0,color=tcol, $
			charsize=1.75
	   if (istnd eq 1) then options = 'non-standard options' $
	   else options = 'standard options'
	   xyouts, 0.02,-0.01,options,/normal,color=tcol,charsize=0.8
	   xyouts,0.98,-0.01,proc_day,/normal,color=tcol,charsize=0.8,alignment=1.0
	endif

	name_month = month_name(mo)
	date = strcompress(string(fix(dy)),/remove_all)+name_month+ $
	       strcompress(string(fix(yr)),/remove_all)

	if (ssec ge 10.) then begin
	   sstring = string(fix(ssec))
	endif else begin
	   sstring = '0'+string(fix(ssec))
	endelse

	if (smin ge 10.) then begin
	   mstring = string(fix(smin))
	endif else begin
	   mstring = '0'+string(fix(smin))
	endelse


	sttim = strcompress(string(fix(shr))+':'+mstring $
		+':'+sstring,/remove_all)

	if (esec ge 10.) then begin
	   sstring = string(fix(esec))
	endif else begin
	   sstring = '0'+string(fix(esec))
	endelse

	if (emin ge 10.) then begin
	   mstring = string(fix(emin))
	endif else begin
	   mstring = '0'+string(fix(emin))
	endelse


	endtim = strcompress(string(fix(ehr))+':'+mstring $
		+':'+sstring,/remove_all)

	times=sttim+' - '+endtim+' UT'

	x0 = 0.09
	y0 = 0.89
	x = ( x0 + (colonne-1) ) * 0.5
	y = ( y0 + (ligne-1) ) * 0.25
	xyouts,x,y,date,/normal,color=tcol

	x0 = 0.03
	y0 = 0.84
	x = ( x0 + (colonne-1) ) * 0.5
	y = ( y0 + (ligne-1) ) * 0.25
	xyouts,x,y,times,/normal,color=tcol

	b1 = cnv_mdhms_sec(yr,mo,dy,shr,smin,ssec)
	b2 = cnv_mdhms_sec(yr,mo,dy,ehr,emin,esec)
	b = (b1 + b2) / 2

	yy = ymin + 1.
	for i=1,3 do begin
	      xx = xmin + (2*i-1) * dlon
	      mlt = MLT(yr, b, xx)
	      mlthr = fix(mlt)
	      mltmin= fix((mlt-mlthr)*60.)
	      if (mltmin ge 10.) then mstring = string(fix(mltmin)) $
	      else mstring = '0'+string(fix(mltmin))
	      smlt = strcompress(string(mlthr)+':'+mstring,/remove_all)
	      xyouts, xx,yy,smlt,alignment=0.5,charsize=0.9,color=tcol
	endfor
	EMPTY

nb_to_draw = nb_to_draw - 1
if (nb_to_draw eq 0) then nb_to_draw = 8

loop:

endwhile

endloop:

output_ps
;init_default
!p.multi = [0,0,0,0,0]
close,u
free_lun,u

; Keeps the options in file options.prt
;--------------------------------------
get_lun, up
openw, up, 'options.prt'
printf, up, proc_day
print_options, up,file,vers,infi,infr
close, up
free_lun, up

answer = ''
if (istnd eq 1) then begin
	print, ' '
	print, 'Options are not Standard...'
	print, 'Do you want to print them ? '
	read, answer
	if (answer eq 'Y' or answer eq 'y') then begin
		que = getenv('IDL_BWPRINT')
		cmnd = 'lp -d'+que+' options.prt'
		spawn, cmnd
	endif		
endif


END ; pltmrg_plus

