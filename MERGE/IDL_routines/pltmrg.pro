
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

        vvx         =  vx(ib,jb)
        vvy         =  vy(ib,jb)
	vvz 	    =  vz(ib,jb)
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
; Ce programme trace les vitesses MERGE carte par carte.
;
; Les vitesses sont d'abord tracees a l'ecran, et on peut produire des
; fichiers postscript (noir et blanc, ou couleur) ou postscript encapsule
; des cartes sur demande.
; - Les fichiers postscript sortent un a un de l'imprimante standard definie
;   par les variables d'environnement IDL_BWPRINT et IDL_CPRINT.
; - Les fichiers postscript encapsule sont gardes un a un dans des fichiers
;   idlxxx.epsf, ou xxx est le numero de la carte
;
; UTILISATION:
;	pltmrg, file=flnm, scale=scl, geom=geom, cntrs=cntrs
;*******************************************************************************

PRO pltmrg, file=flnm, scale=scl, geom=geom, cntrs=cntrs

common pair_data, pair_info

if (KEYWORD_SET(scl)) then scale=scl else scale=1000

spawn, 'date', proc_day
proc_day = 'Processed on: ' + proc_day

init_default

;Fetch the .mrg file
;-------------------

f_path = getenv('SD_MRGOPEN_PATH')

if (KEYWORD_SET(flnm)) then file=flnm $
else begin
   print, ' '
   print, 'Existing .mrg files: '
   cmnd = 'ls -1 '+f_path+'/*.mrg'
   spawn, cmnd
   print, ' '
   file = ' '
   read, 'MERGE data file to be plotted (no extension) ?... ',file
endelse
file = f_path + '/' + file + '.mrg'

lthik = 1.7
!P.FONT = 0
new_vel_col
get_pair_info


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

sdate = intarr(6)
edate = intarr(6)
sdate = infi(2:7)
edate = infi(8:13)
print, ' '
print, format='("The data in this file are from:   19",i2,5i4)',sdate
print, format='("                            to:   19",i2,5i4)',edate

t = intarr(6)
t(0) = sdate(0)
t(1) = sdate(1)
t(2) = sdate(2)
t(5) = 0
print, ' '
print, 'At which time do you want to start (hr,min) ?...'
read, 'Enter [-1 -1] for beginning of file... ', hr, min
t(3) = hr
t(4) = min
if (t(3) eq -1) then t = sdate
stm = cnv_mdhms_sec(t)

istnd	= infi(27)

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


thetitle = 'SUPERDARN VELOCITY MAP'

;SETTING THE PLOT LIMITS, GRID AND COLORS
;----------------------------------------
radars = pair_info(pair).name
misc = fltarr(3)
if (KEYWORD_SET(geom)) then lims = pair_info(pair).maglim $
else lims = pair_info(pair).geolim & misc = pair_info(pair).misc

ymin = lims(0)
ylim = lims(1)
xmin = lims(2)
xlim = lims(3)
lambx = misc(0)
lamby = misc(1)
reflon = misc(2)

resp = ''
print, ' '
print, 'The extreme limits of the map are (latmin,latmax,lonmin,lonmax):'
print, ymin, ylim, xmin, xlim
read, 'Do you want to change them (y/n) ?... ', resp
if (resp eq 'y') or (resp eq 'Y') then $
read, 'Enter latmin, latmax, lonmin, lonmax... ',ymin, ylim, xmin, xlim


dlat = abs(ylim-ymin)/5.
dlon = abs(xlim-xmin)/5.

pollat = (ymin+ylim)/2.
pollon = (xmin+xlim)/2.

S = findgen(16)*(!PI*2/16.)
usersym,cos(S),sin(S),/FILL

NLV = 6
btl = 0
tpl = 6.0*scale/5.

CIN   =  (1+INDGEN(NLV+1));*((8)/(NLV+1))
LVL   =   BTL+(INDGEN(NLV))*(TPL-BTL)/(NLV)

Re    =  6362.
dist_ref = 120.
dt = dist_ref/scale


;GETTING THE L-SHELL CONTOURS
;----------------------------
if (not KEYWORD_SET(geom)) then begin
  lshlat=fltarr(4,360)
  lshlon=fltarr(4,360)
  get_lun,u1
  openr,u1,getenv('SD_TABLES')+'/lsh_geog.dat'
  readf,u1,lshlon
  readf,u1,lshlat
  close,u1
  free_lun,u1
endif

;READING THE DATA
;----------------
count2 = 0
while( NOT EOF(u) ) do begin

	count2 = count2 + 1
	nvects = 0

	read_merge,u, nvects, yr, mo, dy, shr, smin, ssec, ehr, emin, esec, $
	    niter, map_qlty, height_r, data
	
	if (nvects eq 0) then goto, loop

	start_time = cnv_mdhms_sec(yr,mo,dy,shr,smin,ssec)
	if (start_time lt stm) then goto, loop

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

	if (KEYWORD_SET(geom)) then begin
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
	endif


;SETTING THE MAP
;---------------
	print,'Plotting map :', count2

	answer = 'c'
	colbar = 'y'

	start:

	ncol = (!d.n_colors < 256) -1 ;Colors we can use
	if !d.name eq 'PS' then begin
		tcol = 1
	endif else tcol = 10 ;ncol-1

	if (answer eq 'PS' ) or (answer eq 'ps' ) then colbar = 'n'
	if (answer eq 'PSC') or (answer eq 'psc') then colbar = 'y'
	if (answer eq 'EPS') or (answer eq 'eps') then colbar = 'n'

	  map_set,pollat,pollon,limit=[ymin,xmin,ylim,xlim],/GRID,$
		glinethick=0.5,/stereo,color=tcol,$
		latdel=dlat,londel=dlon,/label,latlab=xmin+.25*dlon,$
		lonlab=ymin+dlat,charsize=0.9

	if (KEYWORD_SET(cntrs)) then $
	map_continents,color=tcol


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
	for i = 0,nvects-1 do begin
	  if (lons(i) gt xmin) and (lons(i) lt xlim) and (lats(i) gt ymin) $
	      	  	       and (lats(i) lt ylim) then begin
             ind = max(where(LVL le Vmag(i))) + 1
	     ttcol = cin(ind)	      
	      
; For a two-color plot, then comment out the following lines
	     if (tlon(i) ge lons(i)) then ttcol = cin(5) $
	     else ttcol = cin(1)
	     
	     if (answer eq 'ps') or (answer eq 'PS') or (answer eq 'eps') $
		or (answer eq 'EPS') then ttcol = tcol

 	     plots,lons(i),lats(i),psym=8,symsize=0.4,color=ttcol
	     plots,[lons(i),tlon(i)],[lats(i),tlat(i)], $
		   color=ttcol,thick=lthik
	
	  endif
	endfor

;PLOT THE SCALE (ARROW AND COLOR BAR)
;------------------------------------
	x  = 0.84
	y  = 0.87
	y1 = 0.83
	xx = x + 0.08
	plots,x,y,psym=8,symsize=0.4,color=tcol,/normal
	plots,[x,xx],[y,y]$
		,color=tcol,thick=lthik,/normal
	scstring = strtrim(string(scale),2)+'m/s'
	xyouts,x,y1,scstring,color=tcol,/normal


;PLOTS THE L-SHELL CONTOURS
;--------------------------
	if(not KEYWORD_SET(geom)) then begin
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
	  xyouts, lambx,lamby,lambda,color=tcol,/data
   	endif	

;WRITES COMMENTS ON MAPS
;-----------------------

	!p.font = 0	
	xyouts, 0.02, 0.965, thetitle, /normal, color=tcol
	xyouts, 0.98, 0.965, radars, /normal, alignment=1.0,color=tcol

	name_month = month_name(mo)
	date = strcompress(string(fix(dy)),/remove_all) + name_month + $
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

	y1 = 0.89
	y2 = 0.84
	xyouts,0.09, y1,date,/normal,color=tcol
	xyouts,0.05, y2,times,/normal,color=tcol

	b1 = cnv_mdhms_sec(yr,mo,dy,shr,smin,ssec)
	b2 = cnv_mdhms_sec(yr,mo,dy,ehr,emin,esec)
	b = (b1 + b2) / 2

	if (not KEYWORD_SET(geom)) then begin	  
           mlt = MLT(yr,b,reflon)
	   mlthr = fix(mlt)
	   mltmin= fix((mlt-mlthr)*60.)
	   if (mltmin ge 10.) then mstring = string(fix(mltmin)) $
	   else mstring = '0'+string(fix(mltmin))
	   !p.font=-1
	   smlt = strcompress('!9A!5'+string(mlthr)+':'$
	 	+mstring,/remove_all)+' MLT'
	   if (!d.name eq 'PS') and (!d.y_size gt 17790) then y = 0.85 $
	   else y = 0.79
	   xyouts,0.1, y,smlt,/normal,color=tcol
	endif else begin
	   yy = ymin + 1.
	   for i=1,3 do begin
	      xx = xmin + (2*i-1) * dlon
	      mlt = MLT(yr, b, xx)
	      mlthr = fix(mlt)
	      mltmin= fix((mlt-mlthr)*60.)
	      if (mltmin ge 10.) then mstring = string(fix(mltmin)) $
	      else mstring = '0'+string(fix(mltmin))
	      !p.font = -1
	      smlt = strcompress(string(mlthr)+':'+mstring,/remove_all)+' MLT'
	      xyouts, xx,yy,smlt,alignment=0.5,color=tcol
	   endfor
	endelse

	parameter='vel'
	char = 1.0
;	if (!d.name eq 'WIN') or (colbar eq 'y') then $
; For two-color plot, then uncomment the two following lines
;	if (colbar eq 'y') then $
;	    MK_CONT,BTL,TPL,NLV,CIN,XTITLE=parameter,CHARSIZE=CHAR,ncolor=10


;WRITES PROCESSING INFORMATIONS
;------------------------------

	if (istnd eq 1) then options = 'non-standard options' $
	else options = 'standard options'
	xyouts, 0.02,0.0, options,/normal,color=tcol,charsize=0.5

	xyouts, 0.98,0.0,proc_day,/normal,color=tcol,charsize=0.5,alignment=1.0

  	EMPTY

;CONTINUE or PRINT THE MAP
;-------------------------
	if (!d.name ne 'PS') then begin
	   read, 'Enter C(ontinue), S(top), PS, PSC, EPS, EPSC... ',answer
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
;		spawn, 'rm idl.psc'
;	   	wait, 1.
	   endif
	   if (answer eq 'EPS') or (answer eq 'eps') $
	   or (answer eq 'EPSC') or (answer eq 'epsc') then begin
		spawn, "mv idl.epsf idl"+strcompress(string(fix(count2)),/remove_all)+".epsf"
	   endif
	   init_default
	endelse
	
cont: if (answer eq 'S') or (answer eq 's') then goto, endloop

loop:
endwhile

endloop: 

; Keeps the options in the file options.prt
;------------------------------------------
get_lun, up
openw, up, 'options.prt'
printf, up, proc_day
print_options, up,file,vers,infi,infr
close, up
free_lun, up

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

init_default
close,u
free_lun,u
END ; pltmrg
