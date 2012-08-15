function read_merge,u_mrg,u_los,mrg_str

; The prgram that created the MERGE.DAT file is MERGE.f
;
; Created from READ_MERGE.pro on Dec 20/94 - JMR LOS vel in diamonds passed


status   =  0
on_ioerror, exit

recl1    =  0L
recl2    =  0L
recl3    =  0L
version  =  1.0
infi     =  intarr(27)
infr     =  fltarr(7)
sdate    =  intarr(2,6)
edate    =  intarr(2,6)
mhz      =  intarr(2)
khz      =  intarr(2)
lagfr    =  intarr(2)
smsep    =  intarr(2)
map_qlty =  0L
ngood    =  0
lgood    =  0
niter    =  0L
height_r =  0.
ib       =  0L
jb       =  0L
lat_g    =  fltarr(16,16)
lon_g    =  fltarr(16,16)
lat_m    =  fltarr(16,16)
lon_m    =  fltarr(16,16)
vlos     =  fltarr(2,16,16)
vmag     =  fltarr(16,16)
azim     =  fltarr(16,16)
bx       =  fltarr(16,16)
by       =  fltarr(16,16)
bz       =  fltarr(16,16)

mrg_str  =  mrg_str_def()


;READ THE .LOS FILE   <<<<<<<

readu,U_los, recl1,sdate,edate,lagfr,smsep,map_qlty,lgood, $
							  niter,height_r,recl2

sdate_ck = sdate					
edate_ck = edate

if (lgood ne 0) then begin

  for ind = 0,lgood-1 do begin

    readu,U_los, recl1,ib,jb,a1,a2,b1,b2,v1,v2,w1,w2,w3,recl2

    lat_g(ib,jb)  =  a1
    lon_g(ib,jb)  =  a2
    lat_m(ib,jb)  =  b1
    lon_m(ib,jb)  =  b2
    vlos(0,ib,jb) =  v1
    vlos(1,ib,jb) =  v2
    bx(ib,jb)     =  w1
    by(ib,jb)     =  w2
    bz(ib,jb)     =  w3

  endfor

endif


;READ the .MRG FILE   <<<<<<

readu,U_mrg, recl1,sdate,edate,mhz,khz,lagfr,smsep,map_qlty,ngood, $
                                                          niter,height_r,recl2

for n = 0,5 do begin
  if (sdate_ck(0,n) ne sdate(0,n)) then begin			;same records
    print,' Inconsistent times between .los and .mrg files!'  ;for .los and
    print,' Caught in READ_MERGE.pro'				;.mrg ?
    print,sdate_ck
    print,sdate
  endif
endfor

ncnt    =  intarr(16,16)
vx      =  fltarr(16,16) + 9999
vy      =  fltarr(16,16) + 9999
vz      =  fltarr(16,16) + 9999

h1      =  0							;quality flag

for n = 0,ngood-1 do begin

  readu,U_mrg, recl1,ib,jb,a1,a2,b1,b2,c1,c2,c3,d1,d2,e1,e2,e3, $
                                              f1,f2,f3,g1,g2,g3,h1,recl2
  ncnt(ib,jb)  =   1
  vx(ib,jb)    =  e1
  vy(ib,jb)    =  e2
  vz(ib,jb)    =  e3

endfor

b = cnv_mdhms_sec(sdate(0,0),sdate(0,1),sdate(0,2),sdate(0,3),sdate(0,4), $
	                                                        sdate(0,5))
a = MLT(sdate(0,0),b,-30.)

nlos =  intarr(16,16) * 0

for ib = 0,15 do begin
  for jb = 0,15 do begin

    vmag(ib,jb)  =  9999.
    azim(ib,jb)  =  0.

    if (vlos(0,ib,jb) lt 9999. or vlos(1,ib,jb) lt 9999.) then begin

      nlos(ib,jb) = 1

      if (ncnt(ib,jb) gt 0) then begin
         nlos(ib,jb)  =  2
         vmag(ib,jb)  =  sqrt( vx(ib,jb)^2 + vy(ib,jb)^2 + vz(ib,jb)^2 )
         azim(ib,jb)  =  atan(vy(ib,jb),-vx(ib,jb)) * 180./3.1415
;        if (azim(ib,jb) lt 0.) then azim(ib,jb) = azim(ib,jb) + 360.
         if (azim(ib,jb) gt 180) then azim(ib,jb) = azim(ib,jb) - 360.
      endif

    endif

  endfor
endfor

q = where(vmag ne 9999, a)
if (a ne ngood) then begin
  print,' inconsistent ngood,a = ',ngood,a
  print,' caught in READ_MERGE'
  stop
endif


mrg_str.sdate    =  sdate
mrg_str.edate    =  edate
mrg_str.mhz      =  mhz
mrg_str.khz      =  khz
mrg_str.lagfr    =  lagfr
mrg_str.smsep    =  smsep
mrg_str.map_qlty =  map_qlty
mrg_str.ngood    =  ngood
mrg_str.niter    =  niter
mrg_str.height_r =  height_r
mrg_str.mlt      =  a
mrg_str.lat_g    =  lat_g
mrg_str.lon_g    =  lon_g
mrg_str.lat_m    =  lat_m
mrg_str.lon_m    =  lon_m
mrg_str.nlos     =  nlos
mrg_str.vlos     =  vlos
mrg_str.vmag     =  vmag
mrg_str.azim     =  azim
mrg_str.vx       =  vx
mrg_str.vy       =  vy
mrg_str.vz       =  vz
mrg_str.bx       =  bx
mrg_str.by       =  by
mrg_str.bz       =  bz

status=1
exit:
return,status
end

           
 
