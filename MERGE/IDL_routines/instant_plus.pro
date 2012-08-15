
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

  nvects= ngood

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

	vvx 	    =  vx(ib,jb)
	vvy	    =  vy(ib,jb)
	vvz	    =  vz(ib,jb)
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
	PRO NEW_VEL_COL
;
; Load color bar vel
;
;****************************************************************************

	common colors, r_orig, g_orig, b_orig, RED,GREEN,BLUE
	ncol = (!d.n_colors < 256) -1 ;Colors we can use

	RED = INTARR(ncol)
	GREEN=INTARR(ncol)
	BLUE =INTARR(ncol)
	get_lun,colorfile
	OPENR,colorfile,getenv('SD_TABLES')+'/new_vel_col.dat'
	ar=intarr(4,11)
	READF,colorfile,ar
	free_lun,colorfile
	red(1) = 0
	blue(1) = 0
	green(1) = 0
	ind = 1
	if (!d.name eq 'PS') then ind = 2
	FOR I=0,10 DO BEGIN
	  RED(I+ind)=ar(1,i)
	  GREEN(I+ind)=ar(2,i)
	  BLUE(I+ind)=ar(3,i)
	ENDFOR
	red(0) = 0
	blue(0) = 0

	green(0) = 0
	red(ncol-1) = 255
	blue(ncol-1) = 255
	green(ncol-1) = 255
	red(ncol-2) = 0
	blue(ncol-2) = 75
	green(ncol-2) = 205

	TVLCT,RED,GREEN,BLUE
	END



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

;=============================================================================

pro INSTANT_PLUS, date=dte, colors=colors

spawn, 'date', proc_day
proc_day = 'Processed on: ' + proc_day

;Fetch the .mrg files
;--------------------

f_path = getenv('SD_MRGOPEN_PATH') + '/'

if (KEYWORD_SET(dte)) then date=dte $
else begin
   print, ' '
   print, 'Existing .mrg files: '
   cmnd = 'ls -1 '+f_path+'*.mrg'
   spawn, cmnd
   print, ' '
   date=0L
   read,'Which date do you want (YYMMDD) ?... ',date
endelse
str_date = strcompress(string(date),/remove_all)
cmnd = 'ls -1 ' + f_path + str_date + '*.mrg'
spawn, cmnd, file_ar
n = n_elements(file_ar)
print, ' '
print, 'Number of Pairs =', n

if (n le 1) then goto, stopcode

;Initialisations
;---------------

recl1 = 0L
recl2 = 0L
version = '01234567890123456789'
vers = strarr(n)
infi = intarr(32)
infr = fltarr(10)
pair_infi = intarr(n,32)
pair_infr = fltarr(n,10)
istnd = intarr(n)
mrg_unit = intarr(n)

;Reads the General Headers of the .mrg files
;-------------------------------------------

for i=0,n-1 do begin
   get_lun, u 
   mrg_unit(i) = u
   openr, mrg_unit(i), file_ar(i)
   readu, mrg_unit(i), recl1,version,infi,infr,recl2
   vers(i) = 'MERGE: version ' + string(strmid(version,11,3))
   pair_infi(i,*) = infi
   pair_infr(i,*) = infr
endfor

;Keeps the MERGE processing options in options.prt
;-------------------------------------------------
get_lun, up
openw,up,'options.prt'
printf, up, proc_day
for k=0,n-1 do begin
	print, file_ar(k)
	istnd(k) = pair_infi(k,27)
	print_options, up,file_ar(k),vers(k),pair_infi(k,*),pair_infr(k,*)
endfor
close, up
free_lun, up

;Checks the Important Processing Parameters
;------------------------------------------

int_time = fltarr(n)
int_time = pair_infr(0,0)
a = where((pair_infr(*,0)-int_time),count)
if(count ne 0) then begin
	print, ' '
   	print, 'Integration time is not the same for all the .mrg files'
  	print, 'Program will stop'
   	goto, stopcode
endif
int_time = int_time(0)
print, ' '
print, 'Integration Time =', int_time

stp_time = fltarr(n)
stp_time = pair_infr(0,8)
a = where((pair_infr(*,8)-stp_time),count)
if(count ne 0) then begin
	print, ' '
   	print, 'Step time is not the same for all the .mrg files'
  	print, 'Program will stop'
   	goto, stopcode
endif
stp_time = stp_time(0)
print, 'Step Time =', stp_time

time = pair_infi(*,2:7)
tstart = cnv_mdhms_sec(time)
tstart_min = min(tstart)
delta_t = tstart - tstart_min
diff = delta_t/stp_time
idif = fix(diff) * 10
diff = fix(diff*10)
a = where((diff-idif),count)
if(count ne 0) then begin
	print, ' '
	print, 'The .mrg files do not begin at compatible times.'
	print, 'Rerun MERGE with the same start time, or with start'
	print, '   times compatible with the integration time.'
	print, 'Program will stop'
	goto, stopcode
endif


;Positions the .mrg files to the first common scan
;-------------------------------------------------

units = -mrg_unit
pos = lonarr(n)
start_time = lonarr(n)
nb_tstp = lonarr(n)
for i=0,n-1 do begin
   point_lun, units(i), p
   pos(i) = p
   read_merge, mrg_unit(i),nvects,yr,mo,dy,shr,smin,ssec,ehr,emin,esec, $
			   niter,map_qlty,height_r,data
   start_time(i) = cnv_mdhms_sec(yr,mo,dy,shr,smin,ssec)
   nb_tstp(i) = fix((start_time(i)-tstart_min)/stp_time)
endfor
nbt = max(nb_tstp)
stpt = fix(stp_time)
start_time_max = tstart_min + nbt*stpt

for i=0,n-1 do begin
   while (start_time(i) lt start_time_max) do begin
	point_lun, units(i),p
	pos(i) = p
	read_merge, mrg_unit(i),nvects,yr,mo,dy,shr,smin,ssec,ehr,emin,esec, $
				niter,map_qlty,height_r,data
	start_time(i) = cnv_mdhms_sec(yr,mo,dy,shr,smin,ssec)
   endwhile
endfor

for i=0,n-1 do point_lun, mrg_unit(i), pos(i)

;Setting the plot
;----------------
init_default

scale = 1500
lthik = 1.0
!p.font = 0
set_plot, 'PS'
if KEYWORD_SET(colors) then begin
	new_vel_col
	spawn, '/usr/bin/rm -f idl.psc'
	device, /color, font_size=8, filename='idl.psc'
	device, /inches,xsize=6.5,ysize=10.,xoffset=1.0,yoffset =0.75
	ncol = (!d.n_colors < 256) -1 ;Colors we can use
	if !d.name eq 'PS' then begin
		tcol = 1
	endif else tcol = 10 ;ncol-1
endif else begin
	spawn, '/usr/bin/rm -f idl.ps'
	device, font_size=8, filename='idl.ps'
	device, /inches,xsize=6.5,ysize=10.,xoffset=1.0,yoffset =0.75
	tcol = 1
endelse

S = findgen(16)*(!PI*2/16.)
usersym,cos(S),sin(S),/FILL


thetitle = 'SuperDARN Instantaneous Velocity Map'
latmin = 60.
latmax = 90.
lonmin = 0.
lonmax = 360.

lonpos = 45.
latpos = latmin

dlat = 10.
dlon = 45.

pollat = 90.
pollon = 0.

if KEYWORD_SET(colors) then begin
	NLV = 6
	btl = 0
	tpl = 6.0*scale/5.

	CIN   =  (1+INDGEN(NLV+1));*((8)/(NLV+1))
	LVL   =   BTL+(INDGEN(NLV))*(TPL-BTL)/(NLV)
endif

Re    =  6362.
dist_ref = 120.
dt = dist_ref/scale


;Starts Processing data
;----------------------
!p.multi(0) = 0
!p.multi(1) = 2
!p.multi(2) = 3
nb_to_draw = 6
page = 0

map = 0
nextscan:


lat = fltarr(1024)
lon = fltarr(1024)
Vvmag = fltarr(1024)
Aazim = fltarr(1024)

stime = lonarr(n)
etime = lonarr(n)
first = 0
last = 0
nb_vectors = 0
qlty = 0

for i=0,n-1 do begin
   read_merge, mrg_unit(i),nvects,yr,mo,dy,shr,smin,ssec,ehr,emin,esec, $
			   niter,map_qlty,height_r,data
   stime(i) = cnv_mdhms_sec(yr,mo,dy,shr,smin,ssec)
   etime(i) = cnv_mdhms_sec(yr,mo,dy,ehr,emin,esec)
   nb_vectors = nb_vectors + nvects
   qlty = qlty + fix(map_qlty/1000)
   if (nvects ne 0) then begin
      last = nb_vectors - 1
      lat(first:last) = data(0,0:nvects-1)
      lon(first:last) = data(1,0:nvects-1)
      Vvmag(first:last) = data(2,0:nvects-1)
      Aazim(first:last) = data(3,0:nvects-1)
      first = first + nvects
   endif
endfor

;if (nb_vectors eq 0) or (qlty lt 2) then goto, loop
if (nb_vectors eq 0) then goto, loop
map = map+1

!p.multi(0) = nb_to_draw

if (nb_to_draw eq 6) then begin
	page = page + 1
	page_nb = strcompress(string(fix(page)),/remove_all)
	print, ' '
	print, 'Plotting page ', page_nb
	print, '---------------'
endif

l = (nb_to_draw/2) * 2
if (l eq nb_to_draw) then colonne = 1 $
else colonne = 2

l = nb_to_draw + 1
m = l/2
ligne = m

lats = lat(0:nb_vectors-1)
lons = lon(0:nb_vectors-1)
Vmag = Vvmag(0:nb_vectors-1)
azim = Aazim(0:nb_vectors-1)
start_time = min(stime)
end_time   = max(etime)

month = 0
day = 0
hr_s = 0
hr_e = 0
min_s = 0
min_e = 0
sec_s = 0
sec_e = 0
start = cnv_sec_mdhms(yr,month,day,hr_s,min_s,sec_s,start_time)
stopt  = cnv_sec_mdhms(yr,month,day,hr_e,min_e,sec_e,end_time)
print, ' '
print, 'Map =' ,map
print, 'Start =', yr,month,day,hr_s,min_s,sec_s
print, 'Stop  =', yr,month,day,hr_e,min_e,sec_e

mean_time = (start_time + end_time) / 2
mlt0 = MLT(yr,mean_time,0.)


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

	outlats = fltarr(nb_vectors)
	outlons = fltarr(nb_vectors)
	outlate = fltarr(nb_vectors)
	outlone = fltarr(nb_vectors)
	for i = 0,nb_vectors-1 do begin
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
	if (nb_to_draw eq 6) then $
	   map_set,pollat,pollon,limit=[latmin,lonmin,latmax,lonmax],/GRID,$
		glinethick=0.5,/stereo,color=tcol,$
		latdel=dlat,londel=dlon $
	else $
	   map_set,pollat,pollon,limit=[latmin,lonmin,latmax,lonmax],/GRID,$
		glinethick=0.5,/stereo,color=tcol,/noerase,$
		latdel=dlat,londel=dlon

	for i=0,2 do begin
	   y = 60. + 10.*i
	   yy = strcompress(string(fix(y)),/remove_all)
	   xyouts,45.,y,yy,color=tcol,alignment=0.5
	endfor
	for i=0,3 do begin
	   x = 6.* i * 15.
	   xx = strcompress(string(fix(x/15.)),/remove_all)
	   xyouts,x,60.,xx,color=tcol,alignment=0.5
	endfor
	xyouts,3.,60.,'MLT',color=tcol
	

;Calculating the final position of the arrows whose lengths are proportional to
;Vmag, and which pass through the points (late,lone) calculated before
;------------------------------------------------------------------------------
	tlat = fltarr(nb_vectors)
	tlon = fltarr(nb_vectors)
	for i=0,nb_vectors-1 do begin
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
	

;CONVERTING GEOMAGNETIC LONGITUDE TO MLT
;---------------------------------------
	tcolor = intarr(nb_vectors)
	if KEYWORD_SET(colors) then begin
		for i=0,nb_vectors-1 do $	      
			if (tlon(i) ge lons(i)) then tcolor(i) = cin(5) $
						else tcolor(i) = cin(1)
	endif else begin
		for i=0,nb_vectors-1 do tcolor(i) = tcol
	endelse

	lons = lons + mlt0 * 15.
	tlon = tlon + mlt0 * 15.

	for i=0,nb_vectors-1 do begin	
	  if (lons(i) gt 180.) then lons(i) = lons(i)-360.
	  if (tlon(i) gt 180.) then tlon(i) = tlon(i)-360.
	endfor
	

;PLOT THE VELOCITY ARROWS
;------------------------
	for i = 0,nb_vectors-1 do begin
	  if (lats(i) gt latmin) and (lats(i) lt latmax) then begin
	     plots,lons(i),lats(i),psym=8,symsize=0.3,color=tcolor(i)
	     plots,[lons(i),tlon(i)],[lats(i),tlat(i)], $
		   color=tcolor(i),thick=lthik
	  endif
	endfor


;PLOT THE SCALE
;--------------
	x0 = 0.84
	y0 = 0.92
	x = ( x0 + (colonne-1) ) * 0.5
	y = ( y0 + (ligne-1) ) * 0.3333
	plots,x,y,psym=8,symsize=0.4,color=tcol,/normal

	x0  = 0.84
	xx0 = x0 + 0.08
	y0  = 0.92
	x = ( x0 + (colonne-1) ) * 0.5	
	xx = ( xx0 + (colonne-1) ) * 0.5
	y = ( y0 + (ligne-1) ) * 0.3333
	plots,[x,xx],[y,y],color=tcol,thick=lthik,/normal

	x0 = 0.82
	y0 = 0.87
	x = ( x0 + (colonne-1) ) * 0.5
	y = ( y0 + (ligne-1) ) * 0.3333
	scstring = strtrim(string(scale),2)+'m/s'
	xyouts,x,y,scstring,color=tcol,/normal


;WRITES COMMENTS ON MAPS
;-----------------------

	!p.font = 0
	if (nb_to_draw eq 6) then begin	
	   xyouts,0.01,1.0,thetitle,/normal,color=tcol,charsize=1.75
	   name_month = month_name(month)
	   thedate = strcompress(string(fix(day)),/remove_all)+name_month+ $
		     strcompress(string(fix(yr)),/remove_all)
	   xyouts,0.99,1.0,thedate,/normal,color=tcol,charsize=1.75,alignment=1.0
	   q = where(istnd ne 0, count)
	   if (count ne 0) then options = 'non-standard options' $
	   else options = 'standard options'
	   xyouts, 0.02,-0.01,options,/normal,color=tcol,charsize=0.8
	   xyouts,0.98,-0.01,proc_day,/normal,color=tcol,charsize=0.8,alignment=1.0
	endif

	if (sec_s ge 10.) then begin
	   sstring = string(fix(sec_s))
	endif else begin
	   sstring = '0'+string(fix(sec_s))
	endelse

	if (min_s ge 10.) then begin
	   mstring = string(fix(min_s))
	endif else begin
	   mstring = '0'+string(fix(min_s))
	endelse

	sttim = strcompress(string(fix(hr_s))+':'+mstring $
		+':'+sstring,/remove_all)

	if (sec_e ge 10.) then begin
	   sstring = string(fix(sec_e))
	endif else begin
	   sstring = '0'+string(fix(sec_e))
	endelse

	if (min_e ge 10.) then begin
	   mstring = string(fix(min_e))
	endif else begin
	   mstring = '0'+string(fix(min_e))
	endelse


	endtim = strcompress(string(fix(ehr))+':'+mstring $
		+':'+sstring,/remove_all)

	times=sttim+' - '+endtim+' UT'

	x0 = 0.03
	y0 = 0.92
	x = ( x0 + (colonne-1) ) * 0.5
	y = ( y0 + (ligne-1) ) * 0.3333
	xyouts,x,y,times,/normal,color=tcol

	EMPTY

nb_to_draw = nb_to_draw - 1
if (nb_to_draw eq 0) then nb_to_draw = 6


loop:

for i=0,n-1 do if eof(mrg_unit(i)) then goto, stopcode
goto, nextscan

stopcode:
for i=0,n-1 do free_lun,mrg_unit(i)

answer = ''
q = where(istnd ne 0, count)
if (count ne 0) then begin
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

;init_default

if KEYWORD_SET(colors) then begin
	device,/close
	que = getenv('IDL_CPRINT')
	cmnd = 'lp -d'+que+' idl.psc'
	;print, cmnd
	spawn, cmnd
endif else begin
	output_ps
endelse

!p.multi = [0,0,0,0,0]

END ; instant_plus

