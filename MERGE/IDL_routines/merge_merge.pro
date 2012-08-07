
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


;
;******************************************************************************

	PRO MK_CONT,BOTTOMLEVEL,TOPLEVEL,NLEVELSIN,CINPUT,XTITLE=XTI, $
	CHARSIZE=CHAR,NCOLOR=NCOLO
;
; Given BOTTOMLEVEL, TOPLEVEL and NLEVELS and CIN, a vector with NLEVELS+1
; color indices, produces a color key for a contour plot.
;
	IF( KEYWORD_SET(NCOLO)) THEN NCOL = NCOLO ELSE $
		ncol = (!d.n_colors < 256) -1 ;Colors we can use

	NLEVELS =  NLEVELSIN

        CIN     =  CINPUT + (ncol-3)/(nlevels+1)	


		ncol = (!d.n_colors < 256) -1 ;Colors we can use
		if !d.name eq 'PS' then begin
			tcol = 1
		endif else tcol = ncol-1


	IF(KEYWORD_SET(XTI)) THEN XTL=XTI ELSE XTL=' '
	BOXES=REPLICATE(-1000.*(TOPLEVEL-BOTTOMLEVEL),4,NLEVELS+3)

	FOR I=1,2 DO BOXES(I,0:NLEVELS+1)=BOTTOMLEVEL + $
	(FINDGEN(NLEVELS+2)-1.)*(TOPLEVEL-BOTTOMLEVEL)/(NLEVELS-1)

	CLVLS=BOTTOMLEVEL+FINDGEN(NLEVELS)*(TOPLEVEL-BOTTOMLEVEL)/ $
	(NLEVELS-1)

	LVLS  = BOTTOMLEVEL + $
	            (FINDGEN(NLEVELS+3)-1)*(TOPLEVEL-BOTTOMLEVEL) / NLEVELS


	XTV   = BOTTOMLEVEL + $
	            (FINDGEN(NLEVELS+1))*(TOPLEVEL-BOTTOMLEVEL) / NLEVELS

	YAX=[0,1,2,3]



	CONTOUR,BOXES,yax,LVLS,XSTYLE=4,YSTYLE=4,POSITION=[.85,.15,.90,.85], $
	        /NOERASE,LEVELS=CLVLS,YTICKS=NLEVELS,YTICKV=XTV,XTICKLEN=.1, $
	        CHARSIZE=CHAR,color=tcol, /FILL,C_COLOR=CIN

;	POLYCONTOUR,'TEMP.DAT',COLOR_INDEX=CIN,/DELETE_FILE


	CONTOUR,BOXES,YAX,lvls,XSTYLE=4,YSTYLE=4,POSITION=[.85,.15,.90,.85], $
	        /NOERASE,LEVELS=CLVLS,YTICKS=NLEVELS,YTICKV=XTV,XTICKLEN=.1, $
	        XTITLE=XTL,CHARSIZE=CHAR,color=tcol


	decr = .7/(nlevels+2)

	pos0 = 0.14 + decr
	
	j0 = 0
	if ( xtl eq 'vel' ) then j0 = 1

	for j=j0,nlevels-1 do begin

		xpos = 0.89

		ypos = pos0 + j*decr
		value= strtrim(fix(xtv(j)),2)
		xyouts,xpos,ypos,value,/normal,color=tcol
	endfor

	if xtl eq 'pwr_l' then xtl = 'dB'
	if xtl eq 'width_l' then xtl = 'm/s'
	if xtl eq 'vel' then xtl = 'm/s'

		xpos = 0.86
		ypos = pos0 - .05
		value= strtrim(xtl,2)
		xyouts,xpos,ypos,value,/normal,color=tcol

	RETURN
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

return
end

;*******************************************************************************
;
; Ce programme trace, sur des cartes polaires de l'ionosphere,
; les vitesses MERGE pour toutes les paires de radars
;
; Les vitesses sont d'abord tracees a l'ecran, et on peut produire des
; fichiers postscript (noir et blanc, ou couleur) ou postscript encapsule
; des cartes sur demande.
; - Les fichiers postscript sortent un a un de l'imprimante definie par
;   les variables d'environnement IDL_BWPRINT et IDL_CPRINT.
; - Les fichiers postscript encapsule sont gardes un a un dans des fichiers
;   idlxxx.epsf, ou xxx est le numero de la carte
;
;*******************************************************************************

pro INSTANT, date=dte, scale=scl, geom=geom, cntrs=cntrs


if(KEYWORD_SET(scl)) then scale=scl else scale=1000

spawn, 'date',proc_day
proc_day = 'Processed on: ' + proc_day

init_default

;Fetch the .mrg files
;--------------------

f_path = getenv('SD_MRGOPEN_PATH') + '/'

if(KEYWORD_SET(dte)) then date=dte $
else begin
   print, ' '
   print, 'Existing .mrg files: '
   cmnd = 'ls -1 '+f_path+'*.mrg'
   spawn, cmnd
   print, ' '
   date = 0L
   read, 'Maps for which Date (YYMMDD) ?... ',date
endelse
str_date = strcompress(string(date),/remove_all)
cmnd = 'ls -1 ' + f_path + str_date + '*.mrg'
spawn, cmnd, file_ar

n = n_elements(file_ar)
print, ' '
print, 'Number of Pairs =', n
if (n le 1) then begin
   print, 'There is only one pair available for that day'
   print, 'Please use PLTMRG.PRO'
   goto, stopcode
endif


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
   vers(i) =  'MERGE: version ' + string(strmid(version,11,3))
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
print, 'Integration Time =', int_time

time = pair_infi(*,2:7)
tstart = cnv_mdhms_sec(time)
tstart_min = min(tstart)
time = pair_infi(*,8:13)
tend = cnv_mdhms_sec(time)
tend_min = min(tend)
delta_t = tstart - tstart_min
diff = delta_t/int_time
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
   
;Compare the other options
;-------------------------

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
   for i=0,7 do begin
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

;Positions the .mrg files to the first common scan
;-------------------------------------------------

units = -mrg_unit
pos = lonarr(n)
start_time = lonarr(n)
nb_tint = lonarr(n)
for i=0,n-1 do begin			;reads the first map
   point_lun, units(i), p
   pos(i) = p
   read_merge, mrg_unit(i),nvects,yr,mo,dy,shr,smin,ssec,ehr,emin,esec, $
			   niter,map_qlty,height_r,data
   start_time(i) = cnv_mdhms_sec(yr,mo,dy,shr,smin,ssec)
   nb_tint(i) = fix((start_time(i)-tstart_min)/int_time)
endfor
nbt = max(nb_tint)
intt = fix(int_time)
;print, 'nbt, int_time, tstart_min =', nbt,intt, tstart_min
start_time_max = tstart_min + nbt*intt
;print, 'start_time =', start_time
;print, 'start_time_max =', start_time_max

for i=0,n-1 do begin
   while (start_time(i) lt start_time_max) do begin
	point_lun, units(i),p
	pos(i) = p
	read_merge, mrg_unit(i),nvects,yr,mo,dy,shr,smin,ssec,ehr,emin,esec, $
				niter,map_qlty,height_r,data
	start_time(i) = cnv_mdhms_sec(yr,mo,dy,shr,smin,ssec)
;	print, 'pair #, start_time, pos =', i+1, start_time(i), pos(i)
   endwhile
endfor
;print, 'pos =', pos

tdeb = cnv_sec_mdhms(yr,month,s_day,s_hr,s_min,s_sec,start_time_max)
tfin = cnv_sec_mdhms(yr,month,e_day,e_hr,e_min,e_sec,tend_min)
print, ' '
print, 'The common data period starts at: ', yr,month,s_day,s_hr,s_min,s_sec
print, 'Indicative end time is:           ', yr,month,e_day,e_hr,e_min,e_sec
print, ' '
print, 'At which time do you want to start (hr,min) ?...'
read, 'Enter [-1 -1] for beginning of common period... ', hr, min
t = intarr(6)
t(0) = yr
t(1) = month
t(2) = s_day
t(3) = hr
t(4) = min
t(5) = 0
if (t(3) eq -1) then stm = start_time_max $
else stm = cnv_mdhms_sec(t)

for i=0,n-1 do point_lun, mrg_unit(i), pos(i)

;Setting the plot
;----------------

lthik = 1.5
!p.font = 0
new_vel_col

S = findgen(16)*(!PI*2/16.)
usersym,cos(S),sin(S),/FILL

NLV = 6
btl = 0
tpl = 6.0*scale/5.
CIN = (1+INDGEN(NLV+1));*((8)/(NLV+1))
LVL = BTL+(INDGEN(NLV))*(TPL-BTL)/(NLV)

thetitle = 'SuperDARN Instantaneous Velocity Map'

if(KEYWORD_SET(geom)) then begin
	latmin = 60.
	latmax = 90.
	lonmin = -60.
	lonmax = 60.
endif else begin
	latmin = 55.
	latmax = 85.
	lonmin = -135.
	lonmax = -15.
endelse

resp = ''
print, ' '
print, 'The extreme limits of the map are (latmin,latmax,lonmin,lonmax):'
print, latmin, latmax, lonmin, lonmax
read, 'Do you want to change them (y/n) ?... ', resp
if (resp eq 'y') or (resp eq 'Y') then $
read, 'Enter latmin, latmax, lonmin, lonmax... ',latmin, latmax, lonmin, lonmax

latpos = latmin

dlat = 5.
dlon = 15.

pollat = (latmin+latmax)/2.
pollon = (lonmin+lonmax)/2.

Re    =  6362.
dist_ref = 120.
dt = dist_ref/scale


;GETTING THE L-SHELL CONTOURS
;----------------------------
if (not KEYWORD_SET(geom)) then begin
  lshlat=fltarr(4,360)
  lshlon=fltarr(4,360)
  get_lun,uls
  openr,uls,getenv('SD_TABLES')+'/lsh_geog.dat'
  readf,uls,lshlon
  readf,uls,lshlat
  close,uls
  free_lun,uls
endif

;Starts Processing data
;----------------------

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

for i=0,n-1 do begin
   read_merge, mrg_unit(i),nvects,yr,mo,dy,shr,smin,ssec,ehr,emin,esec, $
			   niter,map_qlty,height_r,data
   stime(i) = cnv_mdhms_sec(yr,mo,dy,shr,smin,ssec)
   etime(i) = cnv_mdhms_sec(yr,mo,dy,ehr,emin,esec)
   nb_vectors = nb_vectors + nvects
   if (nvects ne 0) then begin
      last = nb_vectors - 1
      lat(first:last) = data(0,0:nvects-1)
      lon(first:last) = data(1,0:nvects-1)
      Vvmag(first:last) = data(2,0:nvects-1)
      Aazim(first:last) = data(3,0:nvects-1)
      first = first + nvects
   endif 
endfor

if (nb_vectors eq 0) then goto, loop

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
if (start_time lt stm) then goto, loop
map = map+1


; Calculating the positions (late,lone) of the points, initially at
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

	if (KEYWORD_SET(geom)) then begin
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
	endif


;SETTING THE MAP
;---------------
	answer = 'c'
	colbar = 'y'

	start:

	ncol = (!d.n_colors < 256) -1 ; Colors we can use
	if !d.name eq 'PS' then begin
		tcol = 1
	endif else tcol = 10 ;ncol-1

	if (answer eq 'PS' ) or (answer eq 'ps' ) then colbar = 'n'
	if (answer eq 'PSC') or (answer eq 'psc') then colbar = 'y'
	if (answer eq 'EPS') or (answer eq 'eps') then colbar = 'n'

	map_set,pollat,pollon,limit=[latmin,lonmin,latmax,lonmax],/GRID,$
		glinethick=0.5,/stereo,color=tcol, $
		latdel=dlat,londel=dlon $
		,/label,latlab=lonmin+.25*dlon,lonlab=latpos
	
	if (KEYWORD_SET(cntrs)) then $
	map_continents,color=tcol
	

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
;	(1)	sqrt[(x2-x0)^2+(y2-y0)^2] = Vmag*0.08/scale
;	(2)	(x2-x0)/(x1-x0) = (y2-y0)/(y1-y0)
; where (1) expresses that the length of the arrow is proportional to the scaled velocity
; and (2) expresses that P0-P1 and P0-P2 are parallel

	   size = Vmag(i)*0.08*!d.x_size/scale
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
	for i = 0,nb_vectors-1 do begin
	  if (lats(i) gt latmin) and (lats(i) lt latmax) and $
	     (lons(i) gt lonmin) and (lons(i) lt lonmax) then begin
	     ind = max(where(LVL le Vmag(i))) + 1
	     ttcol = cin(ind)

; For a two-color plot, uncomment the two following lines
	     if (tlon(i) ge lons(i)) then ttcol = cin(5) $
	     else ttcol = cin(1)

	     if (answer eq 'PS') or (answer eq 'ps') or (answer eq 'EPS') $
		or (answer eq 'eps') then ttcol = tcol
;		print, answer

	     plots,lons(i),lats(i),psym=8,symsize=0.3,color=ttcol
	     plots,[lons(i),tlon(i)],[lats(i),tlat(i)], $
		   color=ttcol,thick=lthik
	  endif
	endfor


;PLOT THE SCALE
;--------------
	x = 0.84
	y = 0.87
	plots,x,y,psym=8,symsize=0.4,color=tcol,/normal

	xx = x + 0.08
	plots,[x,xx],[y,y],color=tcol,thick=lthik,/normal

	y = 0.83
	scstring = strtrim(string(scale),2)+'m/s'
	xyouts,x,y,scstring,color=tcol,/normal


;PLOTS THE L-SHELL CONTOURS
;--------------------------
	if(not KEYWORD_SET(geom)) then begin
	  for i=0,358 do begin
	    for j=0,3 do begin
	      if lshlat(j,i) gt latmin then if lshlat(j,i) lt latmax then $
	      if lshlon(j,i) gt lonmin then if lshlon(j,i) lt lonmax then begin
	         plots,[lshlon(j,i),lshlon(j,i+1)]$
			,[lshlat(j,i),lshlat(j,i+1)],color=tcol
	      endif	
	    endfor
	  endfor

;	  !p.font=-1
;	  lambda= '!7K=70!9%'
;	  xyouts, -82.,57.,lambda,color=tcol,/data
   	endif	
   	
   	
;WRITES COMMENTS ON MAPS
;-----------------------

	!p.font = 0

	xyouts,0.02,0.965,thetitle,/normal,color=tcol

	name_month = month_name(month)
	thedate = strcompress(string(fix(day)),/remove_all)+name_month+ $
		  strcompress(string(fix(yr) ),/remove_all)
	xyouts,0.98,0.965,thedate,/normal,color=tcol,alignment=1.0

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

	xyouts,0.05,0.87,times,/normal,color=tcol

	if (not KEYWORD_SET(geom)) then begin	  
           mlt = MLT(yr,mean_time,0.)
	   mlthr = fix(mlt)
	   mltmin= fix((mlt-mlthr)*60.)
	   if (mltmin ge 10.) then mstring = string(fix(mltmin)) $
	   else mstring = '0'+string(fix(mltmin))
	   !p.font=-1
	   smlt = strcompress('!9A!5'+string(mlthr)+':'$
	 	+mstring,/remove_all)+' MLT'
	   xyouts,0.1, 0.83,smlt,/normal,color=tcol
	endif else begin
	   yy = latmin + 3.
	   num = (lonmax-lonmin)/(2.*dlon)
	   num = fix(num)
	   for i=1,num do begin
	      xx = lonmin + (2*i-1) * dlon
	      mlt = MLT(yr, mean_time, xx)
	      mlthr = fix(mlt)
	      mltmin= fix((mlt-mlthr)*60.)
	      if (mltmin ge 10.) then mstring = string(fix(mltmin)) $
	      else mstring = '0'+string(fix(mltmin))
	      !p.font=-1
	      smlt = strcompress(string(mlthr)+':'+mstring,/remove_all)+' MLT'
	      xyouts, xx,yy,smlt,alignment=0.5,color=tcol
	   endfor
	endelse

	parameter='vel'
	char = 1.0
;	if (!d.name eq 'WIN' or colbar eq 'y') then begin
;	if (colbar eq 'y') then $
;	    MK_CONT,BTL,TPL,NLV,CIN,XTITLE=parameter,CHARSIZE=CHAR,ncolor=10


;WRITES PROCESSING INFORMATIONS
;------------------------------
	q = where(istnd ne 0, count)
	if (count ne 0) then options = 'non-standard options' $
	else options = 'standard options'
	xyouts, 0.02,0.0, options,/normal,color=tcol,charsize=0.5

	xyouts, 0.98,0.0,proc_day,/normal,color=tcol,charsize=0.5,alignment=1.0

	EMPTY

;CONTINUE or PRINT THE MAP
;-------------------------
	if (!d.name ne 'PS') then begin
	   read, 'Enter C(ontinue), S(top), PS, PSC, EPS or EPSC... ',answer
	   ln = strlen(answer)
	   if (ln eq 1) then goto, cont $
	   else begin
	      if (answer eq 'PS' ) or (answer eq 'ps' ) then init_ps
	      if (answer eq 'PSC') or (answer eq 'psc') then init_psc
	      if (answer eq 'EPS') or (answer eq 'eps') then init_epsf
	      if (answer eq 'EPSC') or (answer eq 'epsc') then init_epsf,/color
	      goto, start
	   endelse
	endif else begin
	   if (answer eq 'PS' ) or (answer eq 'ps' ) then begin
		output_ps
;		spawn, 'rm idl.ps'
;		wait, 1.
	   endif 
	   if (answer eq 'PSC') or (answer eq 'psc') then begin
		output_psc
		spawn, "mv idl.psc idl"+strcompress(string(fix(map)),/remove_all)+".psc"
;		spawn, 'rm idl.psc'
;	   	wait, 1.
	   endif
	   if (answer eq 'EPS') or (answer eq 'eps') or $
	      (answer eq 'EPSC') or (answer eq 'epsc') then begin
		spawn, "mv idl.epsf idl"+strcompress(string(fix(map)),/remove_all)+".epsf"
	   endif
	   init_default
	endelse
	
cont: if (answer eq 'S') or (answer eq 's') then goto, stopcode

loop:
for i=0,n-1 do if eof(mrg_unit(i)) then goto, stopcode
goto, nextscan

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

END ; instant



