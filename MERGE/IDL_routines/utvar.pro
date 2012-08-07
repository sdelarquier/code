
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


;;*******************************************************************************
;
; Ce programme trace les vitesses MERGE le long d'un meridien magnetique
; en fonction du temps.
;
; Les vitesses sont d'abord tracees a l'ecran, et on peut produire des
; fichiers postscript (noir et blanc, ou couleur) ou postscript encapsule
; des cartes sur demande.
; Les fichiers postscript sortent un a un de l'imprimante definie par IDL_BWPRINT.
; Les fichiers postscript encapsule sont gardes un a un dans des fichiers
;   idlhh:mm.epsf, ou hh:mm est le MLT choisi
;
; On peut tracer les vitesses sur plusieurs meridiens successifs, mais a 
;  condition de garder le meme intervalle UT. 
;
;*******************************************************************************

PRO UTVAR, date=dte, scale=scl


if (KEYWORD_SET(scl)) then scale=scl else scale=1000

spawn, 'date', proc_day
proc_day = 'Processed on: ' + proc_day

init_default

delta_mlt = 1.6/15.

;Fetch the .mrg files
;--------------------

f_path = getenv('SD_MRGOPEN_PATH')+'/'

if (KEYWORD_SET(dte)) then date=dte $
else begin
   print, ' '
   print, 'Existing .mrg files: '
   cmnd = 'ls -1 '+f_path+'*.mrg'
   spawn, cmnd
   print, ' '
   file = ' '
   read, 'UTVAR for which Date (YYMMDD) ?... ', date
endelse
yr = fix(date/10000)
mo = fix(date/100) - yr*100
dy = fix(date) - (yr*10000+mo*100)
str_date = strcompress(string(date),/remove_all)
cmnd = 'ls -1 ' + f_path + str_date + '*.mrg'
spawn, cmnd, file_ar
n = n_elements(file_ar)
print, ' '
print, 'Number of Pairs =', n
print, file_ar


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

pair = strarr(n)
pos = lonarr(n)
for i=0,n-1 do begin
   get_lun, u 
   mrg_unit(i) = u
   openr, mrg_unit(i), file_ar(i)
   posit = strpos(file_ar(i),str_date) + 6
   pair(i) = strmid(file_ar(i),posit,2)
   readu, mrg_unit(i), recl1,version,infi,infr,recl2
   point_lun, -mrg_unit(i),p
   pos(i) = p
   vers(i) = 'MERGE: version ' + string(strmid(version,11,3))
   print, ' '
   print, 'Pair : ', pair(i)
   print, '	Start time :', infi(2:7)
   print, '	Stop  time :', infi(8:13)
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

;Compare the options
;-------------------

if (n ne 1) then begin

   ans = ''
   for i=18,27 do begin
      choice = pair_infi(0,i)
      q = where(pair_infi(*,i) ne choice, count)
      if (count ne 0) then begin
	 option = 'infi(' + strcompress(string(i+1),/remove_all) + ')'
	 print, ' '
	 print, 'Different Option in Input Files: ', option, pair_infi(*,i)
 	 read, 'Do you want to continue (y/n)? ', ans
	 if (ans eq 'n') or (ans eq 'N') then goto, stopcode
      endif
   endfor
   for i=0,8 do begin
      choice = pair_infr(0,i)
      q = where(pair_infr(*,i) ne choice, count)
      if (count ne 0) then begin
	 option = 'infr(' + strcompress(string(i+1),/remove_all) + ')'
	 print, ' '
	 print, 'Different Option in Input Files: ', option, pair_infr(*,i)
	 read, 'Do you want to continue (y/n)? ', ans
	 if (ans eq 'n') or (ans eq 'N') then goto, stopcode
      endif
   endfor

endif

;Period of time to process
;-------------------------

t = intarr(6)
print, ' '
read, 'At which time do you want to start (hr,min) ?... ', hh,mm
t(0) = yr
t(1) = mo
t(2) = dy
t(3) = hh
t(4) = mm
t(5) = 0
stm = cnv_mdhms_sec(t)
nb_hr = 6

ans = ''
print, ' '
read, '6 hours of data will be processed. Do you want to change it ? ',ans
if (ans eq 'y') or (ans eq 'Y') then $
   read, 'How many hours do you want to process... ', nb_hr
etm = stm + nb_hr*3600

;Latitude Limits
;---------------

latmin = 60
latmax = 90
print, ' '
print, 'The extreme latitudes are 60 and 90 degrees'
read, 'Do you want to change them (y/n) ? ',ans
if (ans eq 'y') or (ans eq 'Y') then begin
   read, 'Enter latmin, latmax... ', latmin, latmax
   latmin = fix(latmin)
   latmax = fix(latmax)
endif


;Setting the Plot
;----------------

lthik = 1.5
!p.font = 0
new_vel_col

S = findgen(16)*(!PI*2/16.)
usersym,cos(S),sin(S),/FILL

NLV = 6
btl = 0
tpl = 6.0*scale/5.

CIN   =  (1+INDGEN(NLV+1));*((8)/(NLV+1))
LVL   =   BTL+(INDGEN(NLV))*(TPL-BTL)/(NLV)

s1 = cnv_sec_mdhms(yr,mois,jour,x4,x5,x6,stm)
first_day = jour
utmin = x4
s2 = cnv_sec_mdhms(yr,x2,x3,x4,x5,x6,etm)
if(x5 eq 0) then utmax = x4 else utmax = x4 + 1
if (utmax lt utmin) then utmax = utmax + 24

name_month = month_name(mois)
annee = yr
if (yr lt 1000) then annee = annee + 1900
thedate = strcompress(string(fix(jour)),/remove_all)+name_month+ $
		  strcompress(string(fix(annee) ),/remove_all)

!order = 0
!x.style = 1
!y.style = 1
!x.range = [utmin,utmax]
!y.range = [latmin,latmax]
!x.ticks = utmax-utmin
!y.ticks = fix((latmax-latmin)/10)
!x.minor = 4
!y.minor = 5
!x.title = 'Universal Time (hours)'
!y.title = 'Geomagnetic Latitude (degrees)'
!x.thick = 2.0
!y.thick = 2.0

Re = 6362.
dist_ref = 120.
dt = dist_ref/scale

answer = 'n'

start:

if (answer eq 'n') or (answer eq 'N') then begin
   print, ' '
   read, 'Enter MLT (decimal hour) ... ', mlt_in
   mlt_min = mlt_in - delta_mlt
   mlt_max = mlt_in + delta_mlt
   hour = fix(mlt_in)
   minute = fix((mlt_in-hour)*60)
   if (minute ge 10) then mstring = string(minute) $
   else mstring = '0'+string(minute)
   mlt0 = strcompress(string(hour),/remove_all) + ':' + $
          strcompress(mstring,/remove_all)
   mlt_fl = strcompress(string(hour),/remove_all) + '_' + $
            strcompress(mstring,/remove_all)

endif

thetitle = 'Velocities Along the ' + mlt0 + ' MLT Meridian'
print, ' '
print, thetitle

print, ' '

ncol = (!d.n_colors < 256) -1 ; Colors we can use
if !d.name eq 'PS' then begin
	tcol = 1
endif else tcol = 10 ;ncol-1

plot, [utmin,utmax],[latmin,latmax],/nodata,color=tcol

xyouts,0.14,0.97,thetitle,/normal,color=tcol
xyouts,0.96,0.97,thedate, /normal,color=tcol,alignment=1.0

;WRITES PROCESSING INFORMATIONS
;------------------------------
q = where(istnd ne 0, count)
if (count ne 0) then options = 'non-standard options' $
else options = 'standard options'
xyouts, 0.14,0.0, options,/normal,color=tcol,charsize=0.5

xyouts, 0.97,0.0,proc_day,/normal,color=tcol,charsize=0.5,alignment=1.0

;PLOT THE SCALE
;--------------
x = 0.84
y = 0.87
if (!d.name eq 'PS') then ssize=0.35 else ssize=0.5
plots,x,y,psym=8,symsize=ssize,color=tcol,/normal

xx = x + 0.08
plots,[x,xx],[y,y],color=tcol,thick=lthik,/normal

y = 0.83
scstring = strtrim(string(scale),2)+'m/s'
xyouts,x,y,scstring,color=tcol,/normal

scale_ut  = 0.08*!d.x_size
scale_lat = 0.08*!d.y_size


;Starts Processing data
;----------------------

stime = lonarr(n)
etime = lonarr(n)

for i=0,n-1 do begin

   print, 'Pair : ', pair(i)
   nextscan:
   if eof(mrg_unit(i)) then goto, nextpair

   lats = fltarr(30)
   lons = fltarr(30)
   Vmag = fltarr(30)
   Azim = fltarr(30)
   mlats = fltarr(30)
   mlons = fltarr(30)
   xmlt = fltarr(30)

   read_merge, mrg_unit(i),nvects,yr,mo,dy,shr,smin,ssec,ehr,emin,esec, $
			   niter,map_qlty,height_r,data
   if (nvects eq 0) then goto, nextscan
   stime = cnv_mdhms_sec(yr,mo,dy,shr,smin,ssec)
   etime = cnv_mdhms_sec(yr,mo,dy,ehr,emin,esec)
   if (stime lt stm) then goto, nextscan
   if (stime gt etm) then goto, nextpair
   mean_time = (stime + etime) / 2

   lat = data(0,0:nvects-1)
   lon = data(1,0:nvects-1)
   Vvmag = data(2,0:nvects-1)
   Aazim = data(3,0:nvects-1)

   outlat = fltarr(nvects)
   outlon = fltarr(nvects)
   outmlt = fltarr(nvects)
   for j=0,nvects-1 do begin
	inlat = lat(j)
	inlon = lon(j)
	outpos = cnvcoord(inlat,inlon,height_r)
	outlat(j) = outpos(0)
	outlon(j) = outpos(1)
	outmlt(j) = MLT(yr,mean_time,outlon(j))
   endfor
   mlat = outlat
   mlon = outlon
   mmlt = outmlt

   nb_vectors = 0
					;Keep the data points which are close
   for j=0,nvects-1 do begin		;to the selected magnetic meridian
      if (mmlt(j) gt mlt_min) and (mmlt(j) le mlt_max) then begin
	nb_vectors = nb_vectors + 1
	lats(nb_vectors-1) = lat(j)
	lons(nb_vectors-1) = lon(j)
	Vmag(nb_vectors-1) = Vvmag(j)
	Azim(nb_vectors-1) = Aazim(j)
	mlats(nb_vectors-1) = mlat(j)
	mlons(nb_vectors-1) = mlon(j)
	xmlt(nb_vectors-1) = mmlt(j)
      endif
   endfor

   if (nb_vectors eq 0) then goto, nextscan

   lats = lats(0:nb_vectors-1)
   lons = lons(0:nb_vectors-1)
   Vmag = Vmag(0:nb_vectors-1)
   azim = Azim(0:nb_vectors-1)
   mlats = mlats(0:nb_vectors-1)
   mlons = mlons(0:nb_vectors-1)
   xmlt = xmlt(0:nb_vectors-1)


; Calculating the positions (late,lone) of the points, initially at
; (lats,lons), and moving with the velocity Vmag during the time dt
;------------------------------------------------------------------

   cside = (90. - lats)*!dtor		;in geographic coordinate system
   bside = Vmag*dt/Re			;-------------------------------
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


   outlate = fltarr(nb_vectors)		;in geomagnetic coordinate system
   outlone = fltarr(nb_vectors)		;--------------------------------
   for j = 0,nb_vectors-1 do begin
	inlat = late(j)
	inlon = lone(j)
	outpos = cnvcoord(inlat,inlon,height_r)
	outlate(j) = outpos(0)
	outlone(j) = outpos(1)
   endfor
   late = outlate
   lone = outlone
   lats = mlats
   lons = mlons


;Calculating the azimuth of the velocity in the geomagnetic coordinate system
;----------------------------------------------------------------------------

   cside = (90. - lats)*!dtor
   bside = (90. - late)*!dtor
   angs  = (lone-lons)*!dtor

   r = where( angs lt 0, counts )
 	if counts ne 0 then angs(r) = -1*angs(r)
	
   arg = cos(bside)*cos(cside) + sin(bside)*sin(cside)*cos(angs)

   q = where( arg lt -1, count )
 	if count ne 0 then arg(q) = -1.0
   q = where( arg gt  1, count )
	if count ne 0 then arg(q) = 1.0

   aside = acos(arg)

   arg = (cos(bside)-cos(aside)*cos(cside))/(sin(aside)*sin(cside))

   q = where( arg lt -1,count )
 	if count ne 0 then arg(q) = -1.0
   q = where( arg gt  1 ,count )
	if count ne 0 then arg(q) = 1.0

   bang = acos(arg)*!radeg
	if counts ne 0 then bang(r) = -1.*bang(r)

   Azim = bang
			;at this point, lats, lons and azim are
			;in the geomagnetic coordinate system


;Averaging the velocities at grid points along the meridian (every 0.8 degrees).
;The averaging is done in a weighted manner, with a weight equal to the distance
;between the data point and the grid point.
;-------------------------------------------------------------------------------

	;At present, all points which are less than 1.6 degrees each side of the
	;chosen meridian are kept without averaging.

;Calculating the final position of the arrows whose lengths are proportional to Vmag
;-----------------------------------------------------------------------------------

   ut_end = fltarr(nb_vectors) 
   tlat = fltarr(nb_vectors)  
   time = cnv_sec_mdhms(yr,x1,x2,x3,x4,x5,mean_time)
   ut = x3 + x4/60. + x5/3600.
   if (x2 gt first_day) then ut = ut + 24.
   if (!d.name ne 'PS') then $
   print, 'UT, nb of points =', ut, nb_vectors
   for j=0,nb_vectors-1 do begin   
	nx = convert_coord(ut,lats(j),/data,/to_device)
	x0 = nx(0)
	y0 = nx(1)
	x1 = x0 + scale_ut * Vmag(j) * sin(Azim(j)*!dtor) / scale
	y1 = y0 + scale_lat* Vmag(j) * cos(Azim(j)*!dtor) / scale

	nx = convert_coord(x1,y1,/device,/to_data)
	ut_end(j) = nx(0)
	tlat(j) = nx(1)

	
;PLOT THE VELOCITY ARROWS
;------------------------

	if (ut_end(j) lt ut) then col = cin(1) $
	else col = cin(5)
	
	if (answer eq 'PS') or (answer eq 'ps') or (answer eq 'EPS') $
		or (answer eq 'eps') then col = tcol

	plots,ut,lats(j),psym=8,symsize=ssize,color=col
	plots,[ut,ut_end(j)],[lats(j),tlat(j)], $
		   color=col,thick=lthik
   endfor

   if eof(mrg_unit(i)) then goto, nextpair
   goto, nextscan

   nextpair:

endfor

EMPTY


;CONTINUE or PRINT THE MAP
;-------------------------

if (!d.name eq 'PS') then begin
   if (answer eq 'PS' ) or (answer eq 'ps' ) then $
	output_ps ;& spawn, 'rm idl.ps'
   if (answer eq 'PSC') or (answer eq 'psc') then begin
	output_psc
	spawn, 'mv idl.psc utvar_' + mlt_fl + '.psc'
   endif	 
   if (answer eq 'EPS') or (answer eq 'eps') $
     or (answer eq 'EPSC') or (answer eq 'epsc') then $
	spawn, 'mv idl.epsf utvar_' + mlt_fl + '.epsf'
   init_default
endif

read, 'Enter N(ext MLT), S(top), PS, PSC, EPS or EPSC... ',answer

if (answer eq 's') or (answer eq 'S') then goto, stopcode

if (answer eq 'n') or (answer eq 'N') then begin
   for i=0,n-1 do point_lun,mrg_unit(i),pos(i)
   goto, start
endif

if (answer eq 'PS' ) or (answer eq 'ps' ) then init_ps
if (answer eq 'PSC') or (answer eq 'psc') then init_psc
if (answer eq 'EPS') or (answer eq 'eps') then init_epsf
if (answer eq 'EPSC') or (answer eq 'epsc') then init_epsf,/color

if (!d.name eq 'PS') then begin
   for i=0,n-1 do point_lun,mrg_unit(i),pos(i)
   goto, start
endif

stopcode:
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

init_default
for i=0,n-1 do free_lun,mrg_unit(i)

END ; utvar
