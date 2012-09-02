; ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*
; ! Converts between geocentric coordinates and geodetic (World Geodetic System 1984 (WGS84))
; ! iopt: -1, geocentric to geodetic
; ! 			+1, geodetic to geocentric
; ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*
function CALC_GD2GC, iopt, gdlat, gdlon, rho, glat, glon

	Rav = 6370.						; Earth radius [_km]
	a = 6378.137					; Equatorial radius [_km]
	f = 1./298.257223563	; Flattening of the Earth

; ! semi-minor axis (polar radius)
	b = a*(1. - f)

; ! first eccentricity squared
	e2 = a^2./b^2. - 1.

; ! geodetic to geocentric
	case iopt of
		1: 	begin
				glat = atan( b^2./a^2. * tan(gdlat*!dtor) ) * !radeg
				glon = gdlon
				if (glon gt 180.) then glon = glon - 360.
			end
; ! geocentric to geodetic
		-1:	begin
				gdlat = atan( a^2./b^2. * tan(glat*!dtor) ) * !radeg
				gdlon = glon
			end
		else: print, 'CALC_GD2GC: wrong argument iopt = ', iopt
	endcase

; ! calculate Earth radius at point (uses geocentric latitude)
	rho = a / sqrt( 1. + e2*sin(glat*!dtor)^2. )

	success = 1
	return, success

END; CALC_GD2GC


; ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*
; ! Calculates azimuth and elevation for oblate Earth. 
; ! Input and output positions are in degrees
; ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*
function CALC_AZEL, lati, longi, azim, elev, gaz, gel


; ! Converts from geodetic to geocentric and find Earth radius
	is = CALC_GD2GC(1, lati, longi, Re, glat, glon)
	del = lati - glat

; ! Ray k-vector
	kxg = cos(elev*!dtor) * sin(azim*!dtor)
	kyg = cos(elev*!dtor) * cos(azim*!dtor)
	kzg = sin(elev*!dtor)
	if ~(azim mod 180.) then kxg = 0.
	if ~(elev+90. mod 180.) then kzg = 0.

; ! Correction to the k-vector due to oblateness
	kxr = kxg
	kyr = kyg * cos(del*!dtor) + kzg * sin(del*!dtor)
	kzr = -kyg * sin(del*!dtor) + kzg * cos(del*!dtor)

; ! Finally compute corrected elevation and azimuth
	gaz = atan(kxr,kyr) * !radeg
	gel = atan(kzr / sqrt(kxr^2. + kyr^2.) ) * !radeg

	success = 1
	return, success

END; CALC_AZEL


; ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*
; ! Calculates ground distance  and azimuth
; ! between two points in geographic coordinates
; ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*
function calc_azdist, start_pos, end_pos, azimuth=azimuth

; ! Converts from geodetic to geocentric and find Earth radius
	is = CALC_GD2GC(1, start_pos[0], start_pos[1], sRe, sglat, sglon)

; ! Pre-calculate sin and cos of lat and lon
	coslat = cos(sglat*!dtor)
	sinlat = sin(sglat*!dtor)
	coslon = cos(sglon*!dtor)
	sinlon = sin(sglon*!dtor)
	if abs(90.-abs(sglat)) le .01 then begin
		coslat = 0.
		sinlat = 1.
	endif
	if abs(90.-abs(sglon)) le .01 then begin
		coslon = 0.
		sinlon = eglon/abs(eglon)*1.
	endif
	if abs(270.-abs(sglon)) le .01 then begin
		coslon = 0.
		sinlon = -eglon/abs(eglon)*1.
	endif
; 	print, sglon, coslon, sinlon

; ! Convert from glabal spherical to global cartesian
	srx = (sRe) * coslat * coslon
	sry = (sRe) * coslat * sinlon
	srz = (sRe) * sinlat
	sr = sqrt(srx^2 + sry^2 + srz^2)

; ! Converts from geodetic to geocentric and find Earth radius
	is = CALC_GD2GC(1, end_pos[0], end_pos[1], eRe, eglat, eglon)

; ! Pre-calculate sin and cos of lat and lon
	coslat = cos(eglat*!dtor)
	sinlat = sin(eglat*!dtor)
	coslon = cos(eglon*!dtor)
	sinlon = sin(eglon*!dtor)
	if abs(90.-abs(eglat)) le .01 then begin
		coslat = 0.
		sinlat = 1.
	endif
	if abs(90.-abs(eglon)) le .01 then begin
		coslon = 0.
		sinlon = eglon/abs(eglon)*1.
	endif
	if abs(270.-abs(eglon)) le .01 then begin
		coslon = 0.
		sinlon = -eglon/abs(eglon)*1.
	endif
; 	print, eglon, coslon, sinlon

; ! Convert from glabal spherical to global cartesian
	erx = (eRe) * coslat * coslon
	ery = (eRe) * coslat * sinlon
	erz = (eRe) * sinlat
	er = sqrt(erx^2 + ery^2 + erz^2)

	; Angle between two positions
	theta = acos( (srx*erx + sry*ery + srz*erz)/(sr*er) )

	; ground distance
	Re = (eRe + sRe)/2.
	dist = theta*Re
; 	dist = Re* acos( cos(!pi/2.-sglat*!dtor)*cos(!pi/2.-eglat*!dtor) + $
; 			sin(!PI/2.-sglat*!dtor)*sin(!pi/2.-eglat*!dtor)*cos((eglon-sglon)*!dtor) )

; ! Calcultae azimuth
	sinaz = ( sin(!pi/2.-eglat*!dtor) * sin((eglon-sglon)*!dtor) / sin(dist/Re) )
	if abs(90.-abs(eglat)) le .01 then $
		cosaz = 1. $
	else $
		cosaz = ( cos(!pi/2.-eglat*!dtor) - cos(!pi/2.-sglat*!dtor)*cos(dist/Re) ) / ( sin(!pi/2.-sglat*!dtor)*sin(dist/Re) )
	;print, acos(cosaz)*!radeg, asin(sinaz)*!radeg
	azimuth = acos( cosaz )*!radeg *sinaz/abs(sinaz)
	;is = CALC_AZEL(start_pos[0], start_pos[1], taz, 0., azimuth, gel)

	return, dist

end


; ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*
; ! Calculates new position given an altitude, azimuth, distance and elevation
; ! Input and output positions are in degrees
; ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*
function calc_pos, lati, longi, alti, azim, dist, elev, latiout, longiout

; ! Converts from geodetic to geocentric and find Earth radius
	is = CALC_GD2GC(1, lati, longi, Re, glat, glon)

; ! Adjusts azimuth and elevation for the oblateness of the Earth
	is = CALC_AZEL(lati, longi, azim, elev, gaz, gel)

; ! Pre-calculate sin and cos of lat and lon
	coslat = cos(glat*!dtor)
	sinlat = sin(glat*!dtor)
	coslon = cos(glon*!dtor)
	sinlon = sin(glon*!dtor)

; ! Convert from glabal spherical to global cartesian
	rx = (Re + alti) * coslat * coslon
	ry = (Re + alti) * coslat * sinlon
	rz = (Re + alti) * sinlat

; ! Convert from local spherical to local cartesian
	sx = -dist * cos(gel*!dtor) * cos(gaz*!dtor)
	sy = dist * cos(gel*!dtor) * sin(gaz*!dtor)
	sz = dist * sin(gel*!dtor)

; ! Convert from local cartesian to global cartesian
	tx = sinlat * sx + coslat * sz
	ty = sy
	tz = -coslat * sx + sinlat * sz
	sx = coslon * tx - sinlon * ty
	sy = sinlon * tx + coslon * ty
	sz = tz

; ! Add vectors in global cartesian system
	tx = rx + sx
	ty = ry + sy
	tz = rz + sz

; ! Convert from global cartesian to global spherical
	rho = sqrt( tx^2. + ty^2. + tz^2. )
	glat = 90. - acos(tz/rho)*!radeg
	glon = atan(ty, tx)*!radeg

; ! Compute geodetic coordinates and Earth radius at new point
	is = CALC_GD2GC(-1, latiout, longiout, Re, glat, glon)

	success = 1
	return, success

end


