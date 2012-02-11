;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;       RadarGeoFldPnt
;
; PURPOSE:
;       Internal function for RPos
; 			This subroutine calculates the geocentric coordinates (frho,flat,flon)
; 			of a field point given the angular geocentric coordinates (rrho,rlat,rlon)
; 			of the point of origin, the azimuth (ral), the elevation (rel), and the
; 			slant range (r). Note that the ezimuth and elevation are reckoned from the
; 			horizon that is defined by the plane perpendicular to the radial vector from
; 			the center of the Earth through the point of origin. For conversion from the
; 			radarpointing coordinates to these coordinates, call RadarGeoCnvrt.
; 
;
; CALLING SEQUENCE:
;       RadarGeoTGC,rrho,rlat,rlon,ral,rel,r,frho,flat,flon
;
;-----------------------------------------------------------------
;


pro RadarFldPnt,rrho,rlat,rlon,ral,rel,r,frho,flat,flon 

	; Convert from global spherical (rrho,lat,lon) to global cartesian (rx,ry,rz: Earth centered)
   sinteta=sin(!PI*(90.0-rlat)/180.0)
   rx=rrho*sinteta*cos(!PI*rlon/180.0)
   ry=rrho*sinteta*sin(!PI*rlon/180.0)
   rz=rrho*cos(!PI*(90.0-rlat)/180.0)

	; Convert from local spherical (r,ral,rel) to local cartesian (sx,sy,sz: South,East,Up)
   sx=-r*cos(!PI*rel/180.0)*cos(!PI*ral/180.0)
   sy=r*cos(!PI*rel/180.0)*sin(!PI*ral/180.0)
   sz=r*sin(!PI*rel/180.0)

	; Convert from local cartesian to global cartesian
   tx  =  cos(!PI*(90.0-rlat)/180.0)*sx + sin(!PI*(90.0-rlat)/180.0)*sz
   ty  =  sy
   tz  = -sin(!PI*(90.0-rlat)/180.0)*sx + cos(!PI*(90.0-rlat)/180.0)*sz
   sx  =  cos(!PI*rlon/180.0)*tx - sin(!PI*rlon/180.0)*ty
   sy  =  sin(!PI*rlon/180.0)*tx + cos(!PI*rlon/180.0)*ty
   sz  =  tz

	; Find global cartesian coordinates of new point by vector addition
   tx=rx+sx
   ty=ry+sy
   tz=rz+sz

	; Convert from global cartesian to global spherical
   frho=sqrt((tx*tx)+(ty*ty)+(tz*tz))
   flat=90.0-acos(tz/(frho))*180.0/!PI
   if ((tx eq 0) and (ty eq 0)) then flon=0 $
   else flon=atan(ty,tx)*180.0/!PI
end