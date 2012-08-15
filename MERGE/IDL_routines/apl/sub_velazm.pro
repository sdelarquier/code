;******************************************************************************
Pro sub_velazm,nlos,alos_m,vlos,vazm_m
;
; finds the pointing azimuth of the merged velocity vector in (nominally)
; magnetic corrdinates

for ib = 0,15 do begin
  for jb = 0,15 do begin
    if (nlos(ib,jb) eq 2) then begin
	      
      a0  =  alos_m(0,ib,jb) * !dtor
      v0  =  vlos(0,ib,jb)
      a1  =  alos_m(1,ib,jb) * !dtor
      v1  =  vlos(1,ib,jb)
	      
      det =  sin(a0)*cos(a1) - cos(a0)*sin(a1)
      Ve  = (1./det) * ( cos(a1)*v0 - cos(a0)*v1)
      Vn  = (1./det) * (-sin(a1)*v0 + sin(a0)*v1)

      arg =  Ve/Vn
      alp =  atan(arg) * 1./!dtor
      if (Vn lt 0) then alp = alp + 180

      vazm_m(ib,jb) = alp

    endif
  endfor
endfor
return
end
