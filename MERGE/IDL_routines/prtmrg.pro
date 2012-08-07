;===============================================================================
;
	PRO PRTMRG, file=flnm, head=head, long=long
;
;  PURPUSE  : 	To print results of the MERGE runs kept in the .mrg output files
;		The printout is in the file 'merge.prt'
;
;  KEYWORDS :
;		FILE	The name of the input .mrg file. If this keyword
;			is not given, the program gives all the existing
;			.mrg files, and the user can choose among them.
;		HEAD	If this is set, prtmrg only prints the general header
;		LONG	If this is set, prtmrg prints long outputs containing
;			all the output from merge. Otherwise, it just prints
;			the positions and velocities
;
;===============================================================================

f_path = getenv('SD_MRGOPEN_PATH')+'/'

if (KEYWORD_SET(flnm)) then file = flnm  $
else begin
   cmnd = 'ls -1 '+f_path+'*.mrg'
   spawn, cmnd
   print, ' '
   file = ' '
   read, 'Which file do you want to print (no extension) ?... ',file
endelse
file = f_path + file + '.mrg'
print, 'Input file is : ', file

get_lun, mrg_unit 
openr, mrg_unit, file

get_lun, prt_unit
openw, prt_unit, 'merge.prt'

recl1    =  0L
recl2    =  0L
sdate    =  intarr(2,6)
edate    =  intarr(2,6)
mhz 	 =  intarr(2)
khz	 =  intarr(2)
lagfr    =  intarr(2)
smsep    =  intarr(2)
map_qlty =  0L
ngood    =  0
niter    =  0L
height_r =  0.

version  =  '01234567890123456789'
infi     =  intarr(32)
infr     =  fltarr(10)

readu, mrg_unit, recl1,version,infi,infr,recl2

version = ['MERGE: version '+strmid(version,11,3)]
printf, prt_unit,' '
printf, prt_unit, version
printf, prt_unit,'------------------ '
printf, prt_unit,' '

st1 	= infi(0)
st2 	= infi(1)
syr	= infi(2)
smo	= infi(3)
sdy 	= infi(4)
shr 	= infi(5)
smn 	= infi(6)
ssc	= infi(7)
eyr	= infi(8)
emo	= infi(9)
edy 	= infi(10)
ehr 	= infi(11)
emn 	= infi(12)
esc	= infi(13)
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
istnd 	= infi(27)
tperiod = infr(0)
height  = infr(1)
frang_min = infr(3)
pwr_min   = infr(4)
vel_max   = infr(5)
velerr_max = infr(6)
vel_min   = infr(7)

printf, prt_unit, 'Stations:', st1, st2
printf, prt_unit, ' '

start_time = strcompress(string(syr),/remove_all)+' '+strcompress(string(smo),/remove_all)+' '+ $
	     strcompress(string(sdy),/remove_all)+' '+strcompress(string(shr),/remove_all)+' '+ $
	     strcompress(string(smn),/remove_all)+' '+strcompress(string(ssc),/remove_all)
end_time   = strcompress(string(eyr),/remove_all)+' '+strcompress(string(emo),/remove_all)+' '+ $
	     strcompress(string(edy),/remove_all)+' '+strcompress(string(ehr),/remove_all)+' '+ $
	     strcompress(string(emn),/remove_all)+' '+strcompress(string(esc),/remove_all)
printf, prt_unit, 'Processing Start end Stop Times:'
printf, prt_unit, '	   Start:  ', start_time
printf, prt_unit, '	   Stop :  ', end_time
printf, prt_unit, ' '


if (not KEYWORD_SET(head)) then begin   t = intarr(6)
   t(0) = syr
   t(1) = smo
   t(2) = sdy
   t(5) = 0
   print, ' '
   print, 'Processing start time = ', start_time
   read, 'At what time do you want to start (hour,min) ?... ', hr,min
   t(3) = hr
   t(4) = min
   sttm = cnv_mdhms_sec(t)
endif

printf, prt_unit, 'Processing Parameters:'
if idur eq 1 then printf, prt_unit,'    Map for Each Scan'
if idur eq 2 then printf, prt_unit,'    Averaged Map with integration ', $
					'time =', tperiod
if ialt eq 1 then printf, prt_unit,'    Constant Virtual Height of ', height
if ialt eq 2 then printf, prt_unit,'    Model for Virtual Height'
if ialt eq 3 then printf, prt_unit,'    Virtual Height Determined from ', $
					'elevation angle information'
printf, prt_unit, '    ifltr =', ifltr
printf, prt_unit, '    inter =', inter
printf, prt_unit, '    idiv  =', idiv 
printf, prt_unit, '    iext  =', iext 
printf, prt_unit, '    ires  =', ires
printf, prt_unit, '    istnd =', istnd
printf, prt_unit, ' '

printf, prt_unit, 'Data Selection Parameters'
printf, prt_unit, '    Frequency Range for Radar',st1,':', $
		       f_low1, f_high1
printf, prt_unit, '    Frequency Range for Radar',st2,':', $
		       f_low2, f_high2
printf, prt_unit, '    igs   =                    ', igs
printf, prt_unit, '    iereg =                    ', iereg
printf, prt_unit, '    Minimum Power =            ', pwr_min
printf, prt_unit, '    Minimum Velocity =         ', vel_min
printf, prt_unit, '    Maximum Velocity =         ', vel_max
printf, prt_unit, '    Maximum Error on Velocity =', velerr_max
printf, prt_unit, ' '

if(KEYWORD_SET(head)) then goto, endloop

ans = 'n'
map = 0
while ( not eof(mrg_unit) ) do begin
  map = map + 1
  readu, mrg_unit, recl1,sdate,edate,mhz,khz,lagfr,smsep,map_qlty,ngood, $
		   niter,height_r,recl2
  cct = sdate(0,*)
  ct = cnv_mdhms_sec(cct(0),cct(1),cct(2),cct(3),cct(4),cct(5))

if (ct gt sttm) then begin
  printf, prt_unit, format = '(2("****************************")," MAP",i3)',map
  printf, prt_unit, 'Radar ', st1
  printf, prt_unit, format = '(" From:",6i8)', sdate(0,*)
  printf, prt_unit, format = '("   To:",6i8)', edate(0,*)
  printf, prt_unit, 'Radar ', st2
  printf, prt_unit, format = '(" From:",6i8)', sdate(1,*)
  printf, prt_unit, format = '("   To:",6i8)', edate(1,*)
  printf, prt_unit, ' '

  freq = mhz *1000 + khz
  printf, prt_unit, 'Lagfr (us):            ', lagfr
  printf, prt_unit, 'Sample separation (us):', smsep
  printf, prt_unit, 'Frequency (Khz):       ', freq
  printf, prt_unit, 'Map quality:           ', map_qlty
  printf, prt_unit, 'Number of iterations:  ', niter
  printf, prt_unit, 'Number of good points: ', ngood
  printf, prt_unit, ' '
endif
  ib      =  0L
  jb      =  0L
  g1      =  0

  if (ngood eq 0) then begin
     if (ct ge sttm) then printf, prt_unit,'No common data'
     goto, nextscan
  endif

if (ct gt sttm) then begin
  if (not KEYWORD_SET(long)) then begin
     printf, prt_unit, '  i  j     GGlat     GGlon     GMlat     GMlon      Vx        Vy        Vz    Qlty'
     printf, prt_unit, ' ------   -------   -------   -------   -------    ----      ----      ----  ------
  endif
endif

  for n = 0,ngood-1 do begin

    readu, mrg_unit, recl1,ib,jb,a1,a2,b1,b2,c1,c2,c3,h1,h2,d1,d2,d3, $
                                 e1,e2,e3,f1,f2,f3,g1,recl2

  if (ct ge sttm) then begin
    if (KEYWORD_SET(long)) then begin
      printf, prt_unit, '========== Indices i, j: ', ib, jb, ' =========='
      printf, prt_unit, 'GG Latitude and Longitude: ', a1, a2
      printf, prt_unit, 'MG Latitude and Longitude: ', b1, b2
      printf, prt_unit, 'Vlos1, Vlos2:              ', h1, h2
      printf, prt_unit, 'Bx By Bz:                  ', c1, c2, c3
      printf, prt_unit, 'Vx Vy Vz:                  ', d1, d2, d3
      printf, prt_unit, 'EVxyz :                    ', e1, e2, e3
      printf, prt_unit, 'EWxyz and qlty :           ', f1, f2, f3, g1
    endif else begin
      printf, prt_unit, format = '(2i3,7f10.2,i6)', ib,jb,a1,a2,b1,b2,d1,d2,d3,g1
    endelse
  endif

  endfor

nextscan:
if (ct gt sttm) then begin
  printf, prt_unit, ' '
  read, ' Next Map ? (y/n) ', ans
  if (ans eq 'n') or (ans eq 'N') then goto, endloop
endif

endwhile

endloop:

close, mrg_unit
close, prt_unit
free_lun, mrg_unit
free_lun, prt_unit

END ; prtmrg

