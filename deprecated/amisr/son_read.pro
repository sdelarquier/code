pro son_read, elm=elm, slt=slt, azm=azm, ut=ut, alt=alt, lat=lat, $
	lon=lon, range=range, nel=nel, te=te, ti=ti, vo=vo



openr, unit, '~/Documents/IDL/amisr/sonasci.dat', /get_lun

tmp = fltarr(16)
readf, unit, tmp, format='(16f11.3)'
talt = tmp[4] 

nalt = 1
nalt_old = 0
nt = 1
nlines = 1
while ~eof(unit) do begin
	readf, unit, tmp, format='(16f11.3)'
	if tmp[4] gt talt then $
		nalt = nalt+1 $
	else begin
		nt = nt+1
		if nalt gt nalt_old then $
			nalt_old = nalt
		nalt = 1
	endelse
	talt = tmp[4]
	nlines = nlines + 1
endwhile
nalt = nalt + 1
nt = nt + 1

free_lun, unit

print, nt, nalt, nlines


openr, unit, '~/Documents/IDL/amisr/sonasci.dat', /get_lun

slt = fltarr(nt, nalt)
elm = fltarr(nt, nalt)
azm = fltarr(nt, nalt)
ut = fltarr(nt, nalt)
alt = fltarr(nt, nalt)
lat = fltarr(nt, nalt)
lon = fltarr(nt, nalt)
range = fltarr(nt, nalt)
nel = fltarr(nt, nalt, 2)
te = fltarr(nt, nalt, 2)
ti = fltarr(nt, nalt, 2)
vo = fltarr(nt, nalt, 2)

nalt = 0
nt = 0
talt = 0.
while ~eof(unit) do begin
	readf, unit, tmp, format='(16f11.3)'
	slt[nt,nalt] = tmp[0]
	ut[nt,nalt] = tmp[1]
	azm[nt,nalt] = tmp[2]
	elm[nt,nalt] = tmp[3]
	alt[nt,nalt] = tmp[4]
	lat[nt,nalt] = tmp[5]
	lon[nt,nalt] = tmp[6]
	range[nt,nalt] = tmp[7]
	nel[nt,nalt,0] = tmp[8]
	nel[nt,nalt,1] = tmp[9]
	te[nt,nalt,0] = tmp[10]
	te[nt,nalt,1] = tmp[11]
	ti[nt,nalt,0] = tmp[12]
	ti[nt,nalt,1] = tmp[13]
	vo[nt,nalt,0] = tmp[14]
	vo[nt,nalt,1] = tmp[15]
	if tmp[4] gt talt then $
		nalt = nalt+1 $
	else begin
		nt = nt + 1
		nalt = 0
	endelse
	talt = tmp[4]	
endwhile


end
