;*****************************************************************************
PRO sub_pntazm,lon_g,lat_g,alos
;
; this subroutine determines pointing azimuths to the cell centers for the two
; radars
; the more western radar is indexed 0

for ib = 0,15 do begin                             ;azimuth - 1st radar
  for jb = 0,14 do begin
    xb = lon_g(ib,jb)
    yb = lat_g(ib,jb)
    xt = lon_g(ib,jb+1)
    yt = lat_g(ib,jb+1)
    sub_sphazm,xb,yb,xt,yt,azm
    alos(0,ib,jb) = azm
  endfor
endfor

alos(0,0:15,15) = alos(0,0:15,14) + (alos(0,0:15,14)-alos(0,0:15,13))

for jb = 0,15 do begin                             ;azimuth - 2nd radar
  for ib = 1,15 do begin
    xb = lon_g(ib,jb)
    yb = lat_g(ib,jb)
    xt = lon_g(ib-1,jb)
    yt = lat_g(ib-1,jb)
    sub_sphazm,xb,yb,xt,yt,azm
    alos(1,ib,jb) = azm
  endfor
endfor
alos(1,0,0:15) = alos(1,1,0:15) + (alos(1,1,0:15)-alos(1,2,0:15))

return
end
