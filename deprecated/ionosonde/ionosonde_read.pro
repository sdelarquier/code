pro ionosonde_read, date=date, time=time, juls=juls, pos=pos, geoinds=geoinds, bfield=bfield, fof2=fof2


; formats
format_headers = '(A)'
; YEAR   MONTH     DAY    HOUR     MIN     SEC       UT    GDLAT     GLON     SZEN       KP      DST       AP       F10.7       BXGSM       BYGSM       BZGSM     FOF2
format_data = '(I6,5I10,F13.3,5F11.2,I11,4E14.5,F11.2)'

; Buffers
bla = ''
tyear = 0L
tmonth = 0L
tday = 0L
thour = 0L
tmin = 0L
tsec = 0L
tut = 0.
tlat = 0.
tlon = 0.
tszen = 0.
tkp = 0.
tdst = 0.
tap = 0L
tf107 = 0.
tbx = 0.
tby = 0.
tbz = 0.
tfof2 = 0.

; read file
nlines = 0L
openr, unit, '~/Documents/code/ionosonde/ionosonde.txt', /get_lun
while ~eof(unit) do begin
	readf, unit, bla, format=format_headers
	readf, unit, bla, format=format_headers
	readf, unit, bla, format=format_headers
	readf, unit, tyear, tmonth, tday, thour, tmin, tsec, tut, tlat, tlon, tszen, tkp, tdst, tap, tf107, tbx, tby, tbz, tfof2, format=format_data
; 	if nlines eq 0 then print, tyear, tmonth, tday, thour, tmin, tsec, tut, tlat, tlon, tszen, tkp, tdst, tap, tf107, tbx, tby, tbz, tfof2
	nlines += 1
endwhile
point_lun, unit, 0

date = lonarr(nlines)
time = lonarr(nlines)
juls = dblarr(nlines)
pos = fltarr(nlines, 2)
geoinds = fltarr(nlines, 4)
bfield = fltarr(nlines, 3)
fof2 = fltarr(nlines)

il = 0L
while ~eof(unit) do begin
	readf, unit, bla, format=format_headers
	readf, unit, bla, format=format_headers
	readf, unit, bla, format=format_headers
	readf, unit, tyear, tmonth, tday, thour, tmin, tsec, tut, tlat, tlon, tszen, tkp, tdst, tap, tf107, tbx, tby, tbz, tfof2, format=format_data

	date[il] = tyear*10000L + tmonth*100L + tday
	time[il] = thour*100L + tmin
	juls[il] = julday(tmonth, tday, tyear, thour, tmin)

	pos[il,*] = [tlat, tlon]

	geoinds[il,*] = [tkp, tdst, float(tap), tf107*1e22]

	bfield[il,*] = [tbx, tby, tbz]*1e9

	fof2[il] = tfof2

	il += 1
endwhile
free_lun, unit


openw, outlun, '~/Documents/code/ionosonde/ionosonde_idl.dat', /get_lun
writeu, outlun, nlines
writeu, outlun, date, time, juls, pos, geoinds, bfield, fof2
free_lun, outlun

end