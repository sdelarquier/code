pro rt_new_edens, theta=theta, edens=edens

openr, unit, 'edens.dat', /get_lun

dum = 0L
edenstmp = fltarr(500)
edens = fltarr(500,500)
edenslats = fltarr(500)
edenslons = fltarr(500)

for i=1,500 do begin
	readu, unit, dum, edenstmp, dum
	edens(*,i-1) = edenstmp
endfor
readu, unit, dum, edenslats, dum
readu, unit, dum, edenslons, dum


free_lun, unit

nl = 500
theta = acos( cos(edenslats[0]*!PI/180.)*cos(edenslats*!PI/180.)*cos((edenslons - edenslons[0])*!PI/180.) $
			+ sin(edenslats[0]*!PI/180.)*sin(edenslats*!PI/180.))


end