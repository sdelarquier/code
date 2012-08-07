;******************************************************************************
PRO write_merge,u_mik,mrg_str,nlos,vmag,vazm_l
;
; This program creates the pseudo-mrg file that is sometimes required
; (e.g. FAC mapping using L shell fit vectors).
;
; The program that created the MERGE.DAT file is MERGE.f
;
; Created from READ_MERGE.pro on Dec 20/94 - JMR LOS vel in diamonds passed
;
; Caution: the last three arrays are not guarenteed to apply to the same 
; radar pair as the mrg structure.  REALLY, this should be made clever enough
; to output multiple .mik files for multiple radar pairs.
;
; Notice that ngood is recalculated because vectors might have been added
; by L shell fitting.
;
; Last written: Apr 12/95
;******************************************************************************

recl1    =  0L
recl2    =  0L

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
ib_L     =  0L
jb_L     =  0L

lat_g    =  fltarr(16,16)
lon_g    =  fltarr(16,16)
lat_m    =  fltarr(16,16)
lon_m    =  fltarr(16,16)
vlos     =  fltarr(2,16,16)
;vmag    =  fltarr(16,16)
;azim    =  fltarr(16,16)
vx       =  fltarr(16,16)
vy       =  fltarr(16,16)
vz       =  fltarr(16,16)
bx       =  fltarr(16,16)
by       =  fltarr(16,16)
bz       =  fltarr(16,16)

sdate    =  mrg_str.sdate
edate    =  mrg_str.edate
mhz      =  mrg_str.mhz
khz      =  mrg_str.khz
lagfr    =  mrg_str.lagfr
smsep    =  mrg_str.smsep
map_qlty =  mrg_str.map_qlty
ngood    =  mrg_str.ngood
niter    =  mrg_str.niter
height_r =  mrg_str.height_r


a        =  mrg_str.mlt
lat_g    =  mrg_str.lat_g
lon_g    =  mrg_str.lon_g
lat_m    =  mrg_str.lat_m
lon_m    =  mrg_str.lon_m
;nlos    =  mrg_str.nlos
vlos     =  mrg_str.vlos
;vmag    =  mrg_str.vmag
;azim    =  mrg_str.azim
;vx      =  mrg_str.vx
;vy      =  mrg_str.vy
;vz      =  mrg_str.vz
bx       =  mrg_str.bx
by       =  mrg_str.by
bz       =  mrg_str.bz

c1       =  0.
c2       =  0.
c3       =  0.
d1       =  0.
d2       =  0.
f1       =  0.
f2       =  0.
f3       =  0.
g1       =  0.
g2       =  0.
g3       =  0.
h1       =  0                                                   ;JMR


ngood  = 0

for ib = 0,15 do begin
  for jb = 0,15 do begin
    if (nlos(ib,jb) ge 2) then begin
      ngood     =  ngood + 1
      az        =  vazm_l(ib,jb) * !dtor
      vx(ib,jb) = -cos(az) * vmag(ib,jb)
      vy(ib,jb) =  sin(az) * vmag(ib,jb)
      vz(ib,jb) =  0.
;     print,ib,jb,vmag(ib,jb),vazm_l(ib,jb),fix(vx(ib,jb)),fix(vy(ib,jb))
    endif
  endfor
endfor

writeu,u_mik, recl1,sdate,edate,mhz,khz,lagfr,smsep,map_qlty,ngood, $
                                                    niter,height_r,recl2
for ib = 0,15 do begin
  for jb = 0,15 do begin
    if (nlos(ib,jb) ge 2) then begin

      writeu,u_mik,recl1,LONG(ib),LONG(jb),lat_g(ib,jb),lon_g(ib,jb), $
			       lat_m(ib,jb),lon_m(ib,jb), $
			       c1,c2,c3,d1,d2, $
			       vx(ib,jb),vy(ib,jb),vz(ib,jb), $
			       f1,f2,f3,g1,g2,g3,h1,recl2	

    endif
  endfor
endfor


return
end
