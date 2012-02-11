pro	noaa2iri

openr, unit, '~/Desktop/ap2011.dat', /get_lun
openw, Wunit, '~/Desktop/ap2011_iri.dat', /get_lun

format = '(3I2,25x,8I3,10x,F5.1)'
Wformat = '(11I3,F5.1)'
ap = intarr(8)
while ~eof(unit) do begin
  readf, unit, year, month, day, ap, f107, format=format
  printf, Wunit, year, month, day, ap, f107, format=Wformat
endwhile

free_lun, unit, Wunit




end