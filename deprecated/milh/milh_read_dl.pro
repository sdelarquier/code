pro	milh_read_dl

openr, unit, '~/Documents/IDL/milh/MHee', /get_lun

bla = ''
readf, unit, bla, format='(A)'
formatl = '(I7,5I10,F14.2,6F11.2)'
nlines = 0L
while ~(EOF(unit)) do begin
    nlines++
;     print, nlines
    readf, unit, day, hour, mint, month, sec, year, azm, elm, alt, range, nel, nelmodd, vo, $
	format=formatl
;     print, day, hour, mint, month, sec, year, azm, elm, alt, range, nel, nelmodd, vo
endwhile
free_lun, unit

juls = dblarr(nlines)
azm = fltarr(nlines)
elm = fltarr(nlines)
alt = fltarr(nlines)
range = fltarr(nlines)
nel = dblarr(nlines)
nelmodd = dblarr(nlines)
vo = dblarr(nlines)

openr, unit, 'MHee', /get_lun

bla = ''
readf, unit, bla, format='(A)'
formatl = '(I7,5I10,F14.2,6F11.2)'
nl = 0L
while ~(EOF(unit)) do begin
    readf, unit, day, hour, mint, month, sec, year, tazm, telm, talt, trange, tnel, tnelmodd, tvo, $
	format=formatl
    azm[nl] = tazm
    elm[nl] = telm
    alt[nl] = talt
    range[nl] = trange
    nel[nl] = tnel
    nelmodd[nl] = tnelmodd
    vo[nl] = tvo
    juls[nl] = julday(month, day, year, hour, mint, sec)
    nl++
endwhile
free_lun, unit

openw, unit, 'NHee_idl', /get_lun
writeu, unit, nlines, juls, azm, alt, alt, range, nel, nelmodd, vo
free_lun, unit



end