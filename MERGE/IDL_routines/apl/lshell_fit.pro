;*****************************************************************************
PRO lshell_fit,lon_m,lat_m,alos_m,vlos,alos, $
                                       ind_r,iplot,nlsh,vmag_l,vazm_l,vazm_m_l

nlsh     =  intarr(16,16)
vmag_l   =  fltarr(16,16)
vazm_l   =  fltarr(16,16)
vazm_m_l =  fltarr(16,16)


bm_x  =  intarr(16,16)
gt_x  =  intarr(16,16)
for ib = 0,15 do begin
  for jb = 0,15 do begin
    bm_x(ib,jb) = ib
    gt_x(ib,jb) = jb
  endfor
endfor

mlon = fltarr(16,16)
mlat = fltarr(16,16)
mazc = fltarr(16,16)
varr = fltarr(16,16)
azc  = fltarr(16,16)

mlon = lon_m
mlat = lat_m
mazc(*,*) = alos_m(ind_r,*,*)
varr(*,*) = vlos(ind_r,*,*)
azc(*,*)  = alos(ind_r,*,*)

; Coordinate transform:  Vx  - Vpar   (+poleward)  Vy  - Vper (+eastward)
;                        Vxp - Vtrans (+azimuth)   Vyp - Vrad (+|| to k)


for ind_lat = 0,21 do begin                                    ;should be 0,21

  mlat_low = 65. + ind_lat
  mlat_hgh = 65. + ind_lat + 1.

  q = where (mlat ge mlat_low and mlat lt mlat_hgh and abs(varr) lt 9500,count)

  if (count ge 5) then begin

  frms_lim  =  55.                              ;limit on RMS offset
  Ncount    =  count
  count_arr =  [5,fix(.667*count)]           ;can drop 1/3 points 
  count_lim =  max(count_arr)                ;can drop 1/3 points 
	    
  if (iplot ne 0) then !p.multi  = [0,1,2]

  bm_lsh    =  bm_x(q)
  gt_lsh    =  gt_x(q)  
  vel_lsh   =  varr(q)              
  lon_lsh   =  mlon(q)
  ang_lsh   =  90.-mazc(q)
  ang_azm   =  mazc(q)

  lon_spr   =  max(lon_lsh) - min(lon_lsh)
  bm_min    =  min(bm_lsh)
  bm_max    =  max(bm_lsh)
  ml_min    =  min(lon_lsh)
  ml_max    =  max(lon_lsh)

  sub_linreg,ang_lsh,vel_lsh,vpar,vper,frms,fchk,ferr

  if (iplot ne 0) then begin
    print,' '
    print,'    lat_m   count   vpar    vper    frms    fchk   lon_spr'
    print,fix(mlat_low),fix(count),fix(vpar),fix(vper),fix(frms),fix(fchk), $
                                                               fix(lon_spr)
  endif

  ang_azm_max =  fix(max(ang_azm)) + 5
  ang_azm_min =  fix(min(ang_azm)) - 5	
  nxdat       =  ang_azm_max - ang_azm_min + 1
  xdat        =  ang_azm_min + findgen(nxdat)
  ang_r       =  (90.-xdat)  * !dtor
  vrx         =  vpar*cos(ang_r) + vper*sin(ang_r)

  if (iplot ne 0) then begin
    plot, ang_azm,vel_lsh,psym=1
    oplot,xdat,vrx,linestyle=1
  endif

  while (frms gt frms_lim and count gt count_lim and lon_spr ge 15) do begin

    q_r      =  where(ferr lt max(ferr),count)

;   bm_lsh   =  bm_lsh(q_r)
;   gt_lsh   =  gt_lsh(q_r)  
    vel_lsh  =  vel_lsh(q_r)
    lon_lsh  =  lon_lsh(q_r)
    ang_lsh  =  ang_lsh(q_r)
    ang_azm  =  ang_azm(q_r)

    lon_spr  =  max(lon_lsh) - min(lon_lsh)

    sub_linreg,ang_lsh,vel_lsh,vpar,vper,frms,fchk,ferr
    if (iplot ne 0) then begin
      print,fix(mlat_low),fix(count),fix(vpar),fix(vper),fix(frms),fix(fchk), $
                                                        fix(lon_spr)
    endif
  endwhile

  ifit        =  0

  if (frms le frms_lim and frms lt .8*fchk and lon_spr ge 15) then ifit = 1

  if (iplot ne 0 and ifit eq 1) then $
			print,'                                        <<<'

  ang_azm_max =  fix(max(ang_azm)) + 5
  ang_azm_min =  fix(min(ang_azm)) - 5	
  nxdat       =  ang_azm_max - ang_azm_min + 1
  xdat        =  ang_azm_min + findgen(nxdat)
  ang_r       =  xdat * !dtor
  vry         =  vpar*sin(ang_r) + vper*cos(ang_r)
  if (iplot ne 0) then begin
    plot, ang_azm,vel_lsh,psym=1
    oplot,xdat,vry,linestyle=1
    if (ifit eq 1) then begin
      xyouts,ang_azm_max,max(vel_lsh),fix(vpar)
    endif
    wait,3
  endif

  if (ifit eq 1) then begin

    ang_r  =   ang_azm * !dtor
    vtrn   =   vpar*cos(ang_r) - vper*sin(ang_r)     ;Vxp
    vrad   =   vpar*sin(ang_r) + vper*cos(ang_r)     ;Vyp 

;   print,'     count     m       n    varr    vtrn    mazc    alp   vazm_m_l'

    for p = 0,count-1 do begin
      m             =  bm_lsh(p)
      n             =  gt_lsh(p)
      nlsh(m,n)     =  3
      vmag_l(m,n)   =  sqrt(varr(m,n)*varr(m,n)+vtrn(p)*vtrn(p))

      arg           =  vtrn(p)/varr(m,n)
      alp           =  atan(arg) * 1./!dtor
      if (varr(m,n) lt 0) then alp = alp + 180
      if (alp gt 180    ) then alp = alp - 360

      vazm_m_l(m,n) =  mazc(m,n) + alp
      vazm_l(m,n)   =  azc(m,n)  + alp    
      
;     print,p,m,n,fix(varr(m,n)),fix(vtrn(p)),fix(mazc(m,n)),fix(alp), $
;                 fix(vazm_m_l(m,n)),fix(vazm_l(m,n))

    endfor

   endif
	        
;  endif

   endif

endfor 


!p.multi = [0,1,1]

return
end
